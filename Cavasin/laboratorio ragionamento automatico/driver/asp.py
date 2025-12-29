import asyncio
import importlib.util
import multiprocessing
import statistics as stats
import sys
import time
import uuid
from collections.abc import Iterable
from datetime import timedelta
from multiprocessing.connection import _ConnectionBase
from pathlib import Path
from typing import Any, Literal, cast

import config
import solver
from clingo import Configuration, Control, Function, Number
from solver import Input, NamedTuple
from utils import str_timedelta, str_tuple

__all__ = ["Solution", "Statistics", "Instance", "Solver", "AtomIPC", "ResultIPC"]

type AtomIPC = tuple[str, *tuple[int, ...]]


class ResultIPC(NamedTuple):
    statistics: dict
    exhausted: bool
    unsatisfiable: bool
    tag: Literal["result"] = "result"


class Solution(solver.Solution):
    class IPC(NamedTuple):
        costs: list[int]
        best: bool
        atoms: list[AtomIPC]
        tag: Literal["solution"] = "solution"

    def __init__(self, input: Input, runtime: timedelta, solution: "Solution.IPC"):
        # ("placed", B, Capacity, BHeight, Diameter, O, Width, Length, OHeight, MinOccupancy, X, Y)
        def batch_key(atom: AtomIPC):
            return cast(tuple[int, int, int], atom[2:5])

        def box_size_key(atom: AtomIPC):
            return cast(tuple[int, int, int, int], atom[6:10])

        def box_key(atom: AtomIPC):
            return cast(tuple[int, int, int, int, int], atom[5:10])

        self.input = input
        self.objective = -solution.costs[0]
        self.runtime = runtime
        self.best = solution.best

        box_sizes_dict = {
            (box.width, box.length, box.height, box.min_occupancy): box
            for box in input.box_sizes
        }
        batches_dict = {
            (batch.capacity, batch.height, batch.diameter): batch
            for batch in input.batches.values()
        }
        placements = [atom for atom in solution.atoms if atom[0] == "placed"]
        used_boxes_dict = {
            box_key(p): Solution.Box(box_key(p)[0], box_sizes_dict[box_size_key(p)])
            for p in placements
        }
        self.used_boxes = list(used_boxes_dict.values())
        self.used_bottles = [
            Solution.Bottle(batches_dict[batch_key(p)], used_boxes_dict[box_key(p)])
            for p in placements
        ]

        self._fill_out()


class Statistics(solver.Statistics["Statistics"]):
    runtimes: tuple[timedelta | None, timedelta]

    def __init__(self):
        super().__init__()
        self.runtimes = (
            None,
            config.time_limit,
        )

    def update(self, start_time: float, raw: dict[str, Any] | None = None):
        if raw:
            try:
                self.raw |= raw["summary"]
                self.runtimes = (
                    timedelta(
                        seconds=self.raw["times"]["total"] - self.raw["times"]["solve"]
                    ),
                    timedelta(seconds=self.raw["times"]["solve"]),
                )
                return
            except KeyError:
                pass
        self.runtimes = (
            None,
            timedelta(seconds=time.monotonic() - start_time),
        )

    @property
    def total_time(self):
        try:
            return timedelta(seconds=self.raw["times"]["total"])
        except KeyError:
            pass
        return sum(filter(None, self.runtimes), timedelta())

    @staticmethod
    def summarize(statistics: config.Sequence["Statistics"]):
        mean_runtime = timedelta(
            seconds=stats.fmean(
                statistic.total_time.total_seconds() for statistic in statistics
            )
        )
        s = f"\naverage runtime was: {str_timedelta(mean_runtime)}"
        if all(statistic.runtimes[0] is not None for statistic in statistics):
            mean_flattening = timedelta(
                seconds=stats.fmean(
                    cast(timedelta, statistic.runtimes[0]).total_seconds()
                    for statistic in statistics
                )
            )
            s += f"\nof which grounding: {str_timedelta(mean_flattening)}"
        return s

    def __str__(self):
        return "\n".join(
            (
                f"grounding time: {str_timedelta(self.runtimes[0]) if self.runtimes[0] is not None else '?'}s",
                f"total:\t\t{str_timedelta(self.total_time)}s",
            )
        )

    def asdict(self):
        raise NotImplementedError


class Instance(solver.Instance):
    _statistics: Statistics

    def __init__(self, input: Input):
        super().__init__(input)
        self._statistics = Statistics()

    @property
    def statistics(self):
        return self._statistics

    @staticmethod
    def dump_inputs(out_dir: Path, inputs: Iterable["Input"]):
        out_dir.mkdir(parents=True, exist_ok=True)
        p = Path(out_dir / f"{config.seed}-instances.lp")
        with p.open("w") as file:
            for i, input in enumerate(inputs):
                file.writelines(
                    (
                        f"% instance #{i + 1}\n",
                        *(
                            str(atom[0]) + str_tuple(atom[1:]) + ".\n"
                            for atom in Instance.to_ipc(input)
                        ),
                        "\n\n",
                    ),
                )

    def add_model(self, solution_ipc: Solution.IPC):
        solution = Solution(
            self.input,
            timedelta(seconds=time.monotonic() - self.start_time),
            solution_ipc,
        )
        self.solutions.append(solution)
        self._table.add_row(*solution.as_table_row())

    def set_result(self, result: ResultIPC):
        self.statistics.update(self.start_time, result.statistics)
        if result.exhausted:
            self.was_best()
        if result.unsatisfiable:
            self.unsatisfiable = True

    def mark_start_time(self):
        self.start_time = time.monotonic()

    @staticmethod
    def instantiate(ctl: Control, input_ipc: list[AtomIPC]):
        functions = [
            Function(tuple[0], list(map(Number, tuple[1:]))) for tuple in input_ipc
        ]
        ctl.add(" ".join(str(f) + "." for f in functions))

    @staticmethod
    def to_ipc(input: Input) -> list[tuple[str, *tuple[int, ...]]]:
        return [
            (
                "demijohns",
                input.demijohns_number,
                input.demijohn_capacity,
            ),
            *(
                (
                    "batch",
                    batch.capacity,
                    batch.height,
                    batch.diameter,
                    batch.quantity,
                )
                for batch in input.batches.values()
            ),
            *(
                (
                    "boxSize",
                    box_size.width,
                    box_size.length,
                    box_size.height,
                    box_size.min_occupancy,
                )
                for box_size in input.box_sizes
            ),
        ]

    def asdict(self) -> dict[str, Any]:
        return super().asdict() | {
            "runtime": self.statistics.total_time,
        }


def worker(
    sender: _ConnectionBase,
    src: str,
    input: list[AtomIPC],
):
    try:
        ctl = Control()
        cast(
            Configuration, ctl.configuration.solve
        ).parallel_mode = f"{config.processes},split"
        ctl.add(src)
        Instance.instantiate(ctl, input)

        # parts of a logic program without an explicit #program specification
        # are by default put into a program called base without arguments.
        ctl.ground([("base", [])])
        with ctl.solve(yield_=True) as handle:
            for model in handle:
                sender.send(
                    Solution.IPC(
                        model.cost,
                        model.optimality_proven,
                        [
                            (atom.name, *(arg.number for arg in atom.arguments))
                            for atom in model.symbols(shown=True)
                        ],
                    )
                )
            result = handle.get()
            sender.send(
                ResultIPC(ctl.statistics, result.exhausted, bool(result.unsatisfiable))
            )
    except KeyboardInterrupt:
        pass


class Solver:
    _src: str

    def __init__(self):
        self.name = "clingo"
        with open("model.lp") as file:
            self._src = file.read()

    async def solve(self, input: Input):
        instance = Instance(input)
        yield instance

        receiver, sender = multiprocessing.Pipe(duplex=False)
        clingo = multiprocessing.Process(
            target=worker, args=(sender, self._src, Instance.to_ipc(input))
        )
        instance.mark_start_time()
        end_time = time.monotonic() + config.time_limit.total_seconds()
        clingo.start()
        while True:
            time_left = max(0, end_time - time.monotonic())
            if await asyncio.get_running_loop().run_in_executor(
                None, receiver.poll, time_left
            ):
                ipc: Solution.IPC | ResultIPC = receiver.recv()
                match ipc.tag:
                    case "solution":
                        instance.add_model(ipc)
                        yield instance
                    case "result":
                        instance.set_result(ipc)
                        break
            else:
                clingo.terminate()
                break
        clingo.join(5)
        if clingo.is_alive():
            clingo.kill()
            clingo.join()
