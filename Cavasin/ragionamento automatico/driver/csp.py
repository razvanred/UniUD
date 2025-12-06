import json
from pathlib import Path
import statistics as stats
import time
import warnings
from collections.abc import Sequence
from contextlib import contextmanager
from datetime import timedelta
from typing import Any, Iterable, cast

import config
import minizinc as MZN
import solver
from minizinc.error import MiniZincWarning
from solver import Input
from utils import str_timedelta

__all__ = ["Solution", "Statistics", "Instance", "Solver"]


class Solution(solver.Solution):
    def __init__(
        self,
        input: Input,
        runtime: timedelta,
        best: bool,
        solver_solution,
    ):
        self.input = input
        self.objective = solver_solution.bottled_wine
        self.runtime = runtime
        self.best = best

        used_boxes_dict = {
            bottle["box"]: Solution.Box(
                bottle["box"] - 1,
                input.box_sizes[solver_solution.boxes[bottle["box"] - 1] - 1],
            )
            for bottle in solver_solution.bottles
            if bottle["box"] is not None
        }
        self.used_boxes = list(used_boxes_dict.values())

        batches_list = list(input.batches.values())
        self.used_bottles = [
            Solution.Bottle(
                batches_list[bottle["batch"] - 1],
                used_boxes_dict[bottle["box"]],
            )
            for bottle in solver_solution.bottles
            if bottle["box"] is not None
        ]

        self._fill_out()


class Statistics(solver.Statistics["Statistics"]):
    runtimes: tuple[timedelta | None, timedelta | None, timedelta]

    def __init__(self):
        super().__init__()
        self.runtimes = (
            None,
            None,
            config.time_limit,
        )

    def update(self, start_time: float, raw: dict[str, Any] | None = None):
        if raw:
            self.raw |= raw
            try:
                self.runtimes = (
                    cast(timedelta, self.raw["flatTime"]),
                    cast(timedelta, self.raw["initTime"]),
                    cast(timedelta, self.raw["solveTime"]),
                )
                return
            except KeyError:
                pass
        self.runtimes = (
            None,
            None,
            timedelta(seconds=time.monotonic() - start_time),
        )
        return

    @property
    def total_time(self):
        return sum(filter(None, self.runtimes), timedelta())

    @staticmethod
    def summarize(statistics: Sequence["Statistics"]):
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
            s += f"\nof which flattening: {str_timedelta(mean_flattening)}"
        return s

    def __str__(self):
        return "\n".join(
            (
                f"flattening time: {str_timedelta(self.runtimes[0]) if self.runtimes[0] is not None else '?'}s",
                f"total:\t\t {str_timedelta(self.total_time)}s",
            )
        )

    def asdict(self) -> dict[str, Any]:
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
        p = Path(out_dir / f"{config.seed}-instances.json")
        with p.open("w") as file:
            json.dump(
                [Instance.to_csp(input) for input in inputs],
                file,
                check_circular=False,
                indent=4,
            )

    def add_result(self, result: MZN.Result):
        self.statistics.update(self.start_time, result.statistics)
        if len(result) >= 1:
            for solver_solution in result if len(result) > 1 else [result.solution]:
                solution = Solution(
                    self.input,
                    self.statistics.total_time,
                    result.status == MZN.Status.OPTIMAL_SOLUTION,
                    solver_solution,
                )
                self.solutions.append(solution)
                self._table.add_row(*solution.as_table_row())

        match result.status:
            case MZN.Status.OPTIMAL_SOLUTION:
                assert self.solutions
                self.last_best()
            case MZN.Status.UNSATISFIABLE:
                self.unsatisfiable = True

    @contextmanager
    def instantiate(self, branch: MZN.Instance):
        with branch.branch() as branch:
            input = Instance.to_csp(self.input)
            for key, value in input.items():
                branch[key] = value
            self.start_time = time.monotonic()
            yield branch

    @staticmethod
    def to_csp(input: Input):
        t = input._asdict()
        t["batches"] = [batch._asdict() for batch in input.batches.values()]
        t["box_sizes"] = [box_size._asdict() for box_size in input.box_sizes]
        return t

    def asdict(self) -> dict[str, Any]:
        return super().asdict() | {
            "runtime": self.statistics.total_time,
        }


class Solver:
    _minizinc: MZN.Instance

    def __init__(self):
        self.name = config.csp_solver_label
        solver = MZN.Solver.lookup(config.csp_solver_label)
        model = MZN.Model("model.mzn")
        self._minizinc = MZN.Instance(solver, model)

    async def solve(self, input: Input):
        instance = Instance(input)
        yield instance
        with instance.instantiate(self._minizinc) as minizinc:
            with warnings.catch_warnings(category=MiniZincWarning, action="ignore"):
                async for result in minizinc.solutions(
                    time_limit=config.time_limit,
                    processes=config.processes,
                    intermediate_solutions=True,
                ):
                    instance.add_result(result)
                    yield instance
