from collections.abc import AsyncIterator, Iterable, Sequence, Set
import csv
from datetime import datetime, timedelta
import json
from math import ceil
from pathlib import Path, PurePath
import random
import asyncio
import statistics
import time
from typing import Any, Final, NamedTuple, cast, override
import warnings
from minizinc.error import MiniZincWarning
import rich.box
from rich.columns import Columns
from rich.console import Console, Group, RenderResult
from rich.live import Live
from rich.panel import Panel
from rich.progress import (
    BarColumn,
    Progress,
    Task,
    TextColumn,
    TimeElapsedColumn,
)
from rich.progress_bar import ProgressBar
from rich.table import Column, Table
import minizinc as MiniZinc
from rich.text import Text


class BottleSize(NamedTuple):
    capacity: float
    height: int
    diameter: int


Batch = NamedTuple(
    "Batch",
    [("quantity", int)]
    + [(field, type(getattr(BottleSize, field))) for field in BottleSize._fields],
)


class BoxSize(NamedTuple):
    width: int
    length: int
    height: int
    minCoverage: int


class Instance(NamedTuple):
    demijohnsNumber: int
    demijohnCapacity: int
    batches: Sequence[Batch]
    boxSizes: Sequence[BoxSize]

    @staticmethod
    def generate(
        demijohnsNumber: int,
        demijohnCapacity: int,
        usableBottles: Set[int],
        bottles: int,
        count: int,
    ):
        for _ in range(count):
            yield Instance(
                demijohnsNumber,
                demijohnCapacity,
                [
                    Batch(quantity, **(bottleSizes[k]._asdict()))
                    for (k, quantity) in zip(
                        bottleSizes.keys(),
                        randomDistribute(len(bottleSizes), bottles, usableBottles),
                    )
                ],
                random.sample(boxSizes, boxQuantity),
                # [
                #     BoxSize(
                #         (width := random.choice(boxWidths)),
                #         int(width * random.choice(boxRatios)),
                #         random.choice(boxHeights),
                #         random.choice(boxCoverages),
                #     )
                #     for _ in range(boxQuantity)
                # ],
            )

    @staticmethod
    def serialize(outDir: PurePath, seed: int, instances: Iterable["Instance"]):
        p = Path(outDir / f"{seed}-instances.json")
        p.parent.mkdir(parents=True, exist_ok=True)
        with p.open("w") as file:
            json.dump(
                [instance.asdict() for instance in instances],
                file,
                check_circular=False,
                indent=4,
            )

    def populateBranch(self, branch: MiniZinc.Instance):
        for key, value in self.asdict().items():
            branch[key] = value

    def asdict(self):
        t = self._asdict()
        t["batches"] = [batch._asdict() for batch in self.batches]
        t["boxSizes"] = [boxSize._asdict() for boxSize in self.boxSizes]
        return t

    def __rich_console__(self, console, options) -> RenderResult:
        table = Table(
            "wine",
            "bottles",
            title="Instance",
            title_justify="left",
            box=rich.box.MINIMAL,
        )
        for name in bottleSizes.keys():
            table.add_column(name)
        table.add_row(
            f"{self.demijohnsNumber* self.demijohnCapacity}L",
            str(sum(batch.quantity for batch in self.batches)),
            *(str(batch.quantity) for batch in self.batches),
        )
        yield table
        yield Columns(
            (
                Panel.fit(
                    f"{boxSize.width*boxSize.length}cm², {boxSize.minCoverage}%({int(a:=boxSize.width*boxSize.length*boxSize.minCoverage/100)}cm²)",
                    title=f"[#8C603A]{boxSize.width}[/]x[#F2C6A0]{boxSize.length}[/]x[#BF8C60]{boxSize.height}[/]",
                    title_align="left",
                    padding=(0, 0),
                )
                for boxSize in self.boxSizes
            )
        )


class Solution:
    class Bottle(NamedTuple):
        batch: int
        box: int

    class Box(NamedTuple):
        id: int
        size: int

    def __init__(
        self,
        instance: Instance,
        objective: float,
        runtime: timedelta,
        solverSolution,
    ):
        self.instance = instance
        self.objective = objective
        self.runtime = runtime
        self.best = False
        self.usedBottles: Sequence[Solution.Bottle] = [
            Solution.Bottle(bottle["batch"] - 1, bottle["box"] - 1)
            for bottle in solverSolution.bottles
            if bottle["box"] is not None
        ]
        self.usedBoxes: Sequence[Solution.Box] = [
            Solution.Box(box, solverSolution.boxes[box] - 1)
            for box in set(bottle.box for bottle in self.usedBottles)
        ]
        self.batchCounters: Sequence[int] = count(
            len(instance.batches), (bottle.batch for bottle in self.usedBottles)
        )
        self.boxCounters: Sequence[int] = count(
            len(instance.boxSizes), (box.size for box in self.usedBoxes)
        )
        self.boxCoverages: Sequence[int] = [
            round(
                100
                * sum(
                    self.instance.batches[bottle.batch].diameter ** 2
                    for bottle in self.usedBottles
                    if bottle.box == box.id
                )
                / ((boxSize := instance.boxSizes[box.size]).width * boxSize.length)
            )
            for box in self.usedBoxes
        ]

    def toTableRow(self):
        return (
            Text(field, style="green" if self.best else "")
            for field in (
                str(self.runtime)[:-3],
                f"{self.objective}L",
                f"{len(self.usedBottles)}: {strTuple(self.batchCounters)}",
                f"{len(self.usedBoxes)}: {strTuple(self.boxCounters)}",
                strTuple(f"{e}%" for e in self.boxCoverages),
            )
        )


class Run:
    def __init__(self, instance: Instance, timeLimit: timedelta):
        self.instance: Instance = instance
        self.statistics: dict[str, float | int | timedelta] = {}
        self.solutions: list[Solution] = []
        self.unsatisfiable: bool = False
        self.runtimes: tuple[timedelta | None, timedelta | None, timedelta] = (
            None,
            None,
            timeLimit,
        )
        self.table = Table(
            Column("runtime", no_wrap=True),
            Column("objective", no_wrap=True),
            Column("bottles counters", no_wrap=True),
            Column("boxes counters", no_wrap=True),
            Column("coverages"),
            title="Solutions",
            title_justify="left",
            box=rich.box.MINIMAL,
        )
        self.startTime: float = time.monotonic()

    def addResult(self, result: MiniZinc.Result):
        if len(result) >= 1:
            runtime = cast(timedelta, result.statistics["time"])
            result.statistics.pop("time", None)
            self.statistics |= result.statistics
            for solverSolution in result if len(result) > 1 else [result.solution]:
                solution = Solution(
                    self.instance,
                    cast(float, result.objective),
                    runtime,
                    solverSolution,
                )
                self.solutions.append(solution)
                self.table.add_row(*solution.toTableRow())
        else:
            self.statistics |= result.statistics

        if result.status == MiniZinc.Status.OPTIMAL_SOLUTION:
            assert self.solutions
            self.solutions[-1].best = True
            self.table.rows[-1].style = "green"

        if result.status == MiniZinc.Status.UNSATISFIABLE:
            self.unsatisfiable = True

        try:
            self.runtimes = (
                cast(timedelta, self.statistics["flatTime"]),
                cast(timedelta, self.statistics["initTime"]),
                cast(timedelta, self.statistics["solveTime"]),
            )
        except KeyError:
            self.runtimes = (
                None,
                None,
                timedelta(seconds=time.monotonic() - self.startTime),
            )

    def toDict(self) -> dict[str, Any]:
        dict = {
            "runtime": sum(filter(None, self.runtimes), timedelta()),
        }
        if self.solutions:
            solution = self.solutions[-1]
            dict |= {"objective": solution.objective, "best": solution.best}
        elif self.unsatisfiable:
            dict |= {"objective": "unsatisfiable"}
        return dict

    @staticmethod
    def serialize(outDir: PurePath, runs: Iterable["Run"]):
        p = Path(
            outDir / f"{seed}-{datetime.now().strftime("%Y-%m-%dT%H%M%S")}-runs.csv"
        )
        p.parent.mkdir(parents=True, exist_ok=True)
        with p.open("w", newline="") as file:
            writer = csv.DictWriter(
                file,
                ["instance", "runtime", "objective", "best"],
                extrasaction="ignore",
            )
            writer.writeheader()
            for i, run in enumerate(runs):
                dict = run.toDict()
                dict["instance"] = i
                writer.writerow(dict)
        pass

    def __rich__(self):
        if self.solutions:
            return self.table
        elif self.unsatisfiable:
            return Text("unsatisfiable\n", style="bold green")
        else:
            return Text("no solutions found\n", style="strong")


class CountDownBarColumn(BarColumn):
    @override
    def render(self, task: Task) -> ProgressBar:
        progressBar = super().render(task)
        if task.total is None:
            return progressBar
        remaining = task.total - task.elapsed if task.elapsed else 0
        progressBar.completed = max(0, remaining)
        progressBar.pulse = False
        return progressBar


class TimeRemainingColumn(TimeElapsedColumn):
    @override
    def render(self, task: Task) -> Text:
        if task.total is None:
            return Text("-:--:--", style="progress.elapsed")
        remaining = task.total - max(0, task.elapsed) if task.elapsed else 0
        return Text(
            f"-{timedelta(seconds=max(0,ceil(remaining)))}", style="progress.elapsed"
        )


def randomDistribute(
    marginalPopulation: int, k: int, marginalSupport: Set[int]
) -> list[int]:
    if marginalPopulation == 0 or not marginalSupport:
        return [0] * marginalPopulation
    v: list[int] = [
        random.choice(range(k + 1)) if i in marginalSupport else 0
        for i in range(marginalPopulation)
    ]
    v: list[int] = [n * k // max(1, sum(v)) for n in v]
    for _ in range(k - sum(v)):
        lucky, *_ = random.choices(
            list(marginalSupport), [v[i] + 1 for i in marginalSupport]
        )
        v[lucky] += 1
    return v


def count(domain: int, v: Iterable[int]):
    c = [0] * domain
    for i in v:
        c[i] += 1
    return c


def strTuple(v: Iterable[object]):
    return f"({", ".join(str(e) for e in v)})"


async def quietSolve[T](iterator: AsyncIterator[T]):
    with warnings.catch_warnings(category=MiniZincWarning, action="ignore"):
        async for e in iterator:
            yield e


async def solve(instance: Instance, solverRun: MiniZinc.Instance, live: Live):
    runProgress.reset(runTask, start=True)
    runProgress._tasks[runTask].stop_time = None
    run = Run(instance, timeLimit)
    async for result in quietSolve(
        solverRun.solutions(
            time_limit=timeLimit, processes=processes, intermediate_solutions=True
        )
    ):
        run.addResult(result)
        live.update(Group(run, progressGroup))
    live.auto_refresh = False
    runProgress.stop_task(runTask)
    live.update(progressGroup, refresh=True)
    console.print(run)
    live.auto_refresh = True
    return run


# parameters
bottleSizes: Final[dict[str, BottleSize]] = {
    # demi or half
    "demi": BottleSize(0.375, 24, 6),
    # standard or bordeaux
    "standard": BottleSize(0.75, 30, 8),
    # 1L
    "1L": BottleSize(1.0, 32, 8),
    # magnum
    "magnum": BottleSize(1.5, 35, 9),
    # jeroboam
    "jeroboam": BottleSize(3.0, 47, 13),
}
boxSizes: Final[Sequence[BoxSize]] = [BoxSize(12, 12, 100, 1)]
demijohnsNumber: Final[int] = 10
demijohnCapacity: Final[int] = 1
usableBottleSizes: Final[Set[int]] = {0}  # set(range(len(bottleSizes)))
bottleQuantity: Final[int] = 4
boxQuantity: Final[int] = 1
assert boxQuantity <= len(boxSizes)
# boxWidths: Final[Sequence[int]] = [15, 25, 30, 40]
# boxRatios: Final[Sequence[float]] = [1.0, 1.6, 2.0]
# boxHeights: Final[Sequence[int]] = [30, 35, 50]
# boxCoverages: Final[Sequence[int]] = [50, 60, 70]

seed: Final[int] = 230706628
solverLabel: Final[str] = "gecode"
runCount: Final[int] = 8
timeLimit: Final[timedelta] = timedelta(minutes=5)
processes: Final[int] = 6
outDir: Final[PurePath] = PurePath("out")

# ui
console = Console(highlight=False)

totalProgress = Progress(
    TimeElapsedColumn(table_column=Column(justify="right", min_width=7)),
    BarColumn(
        complete_style="blue",
    ),
    TextColumn("{task.description}"),
)
totalTask = totalProgress.add_task(f"run 1 of {runCount}", start=False, total=runCount)
runProgress = Progress(
    TimeRemainingColumn(
        table_column=Column(justify="right", min_width=7),
    ),
    CountDownBarColumn(
        complete_style="blue",
        finished_style="blue",
    ),
    TextColumn(
        "{task.description}",
    ),
)
runTask = runProgress.add_task(
    f"solving with {solverLabel}",
    total=timeLimit.total_seconds(),
    start=False,
)

progressGroup = Group(runProgress, totalProgress)


async def main():
    solver: MiniZinc.Solver = MiniZinc.Solver.lookup(solverLabel)
    model = MiniZinc.Model("model.mzn")
    minizinc = MiniZinc.Instance(solver, model)
    instances = list(
        Instance.generate(
            demijohnsNumber,
            demijohnCapacity,
            usableBottleSizes,
            bottleQuantity,
            runCount,
        )
    )
    Instance.serialize(outDir, seed, instances)
    runs: list[Run] = []
    try:
        with Live(progressGroup, vertical_overflow="crop", console=console) as live:
            totalProgress.start_task(totalTask)
            for i, instance in enumerate(instances):
                with minizinc.branch() as branch:
                    instance.populateBranch(branch)
                    console.print(instance)
                    run = await solve(instance, branch, live)
                    runs.append(run)
                    totalProgress.update(
                        totalTask,
                        description=f"run {i+1} of {runCount} completed",
                        completed=i + 1,
                    )
    finally:
        Run.serialize(outDir, runs)
        if runs:
            runtimes = [run.runtimes for run in runs]
            meanRuntime = timedelta(
                seconds=statistics.fmean(
                    sum(filter(None, summary), timedelta()).total_seconds()
                    for summary in runtimes
                )
            )
            console.print(f"\naverage runtime was {meanRuntime}")
            if all(summary[0] for summary in runtimes):
                meanFlatTime = timedelta(
                    seconds=statistics.fmean(
                        cast(timedelta, summary[0]).total_seconds()
                        for summary in runtimes
                    )
                )
                console.print(f"of which flattening {meanFlatTime}")


random.seed(a=seed, version=2)
try:
    asyncio.run(main())
except KeyboardInterrupt:
    console.print("Control-C", style="logging.level.error")
