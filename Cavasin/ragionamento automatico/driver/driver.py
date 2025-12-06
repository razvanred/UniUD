import asyncio
import random
from datetime import timedelta
from math import ceil
from typing import override

import asp
import config
import csp
from rich.console import Console, Group
from rich.live import Live
from rich.progress import (
    BarColumn,
    Progress,
    Task,
    TextColumn,
    TimeElapsedColumn,
)
from rich.progress_bar import ProgressBar
from rich.table import Column
from rich.text import Text
from solver import Input, Instance, Solver

# __all__ = []


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
            f"-{timedelta(seconds=max(0, ceil(remaining)))}", style="progress.elapsed"
        )


# ui
console = Console(highlight=False)

total_progress = Progress(
    TimeElapsedColumn(table_column=Column(justify="right", min_width=7)),
    BarColumn(
        complete_style="blue",
    ),
    TextColumn("{task.description}"),
)
total_task = total_progress.add_task("description", start=False, total=config.run_count)
run_progress = Progress(
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
run_task = run_progress.add_task(
    "description",
    total=config.time_limit.total_seconds(),
    start=False,
)

progress_group = Group(run_progress, total_progress)


async def solve_all[T: Instance](inputs: list[Input], solver: Solver[T]):
    async def solve(live: Live):
        run_progress.reset(run_task, start=True)
        run_progress._tasks[run_task].stop_time = None
        async for instance in solver.solve(input):
            live.update(Group(instance, progress_group))
        live.auto_refresh = False
        run_progress.stop_task(run_task)
        live.update(progress_group, refresh=True)
        console.print(instance)  # pyright: ignore[reportPossiblyUnboundVariable]
        console.print(instance.statistics, end="\n\n")  # pyright: ignore[reportPossiblyUnboundVariable]
        live.auto_refresh = True
        return instance  # pyright: ignore[reportPossiblyUnboundVariable]

    total_progress.update(total_task, description=f"run 1 of {config.run_count}")
    run_progress.update(run_task, description=f"solving with {solver.name}")
    instances: list[T] = []
    # try:
    with Live(progress_group, vertical_overflow="crop", console=console) as live:
        total_progress.start_task(total_task)
        for i, input in enumerate(inputs):
            console.print(input)
            instance = await solve(live)
            instances.append(instance)
            total_progress.update(
                total_task,
                description=f"run {i + 1} of {config.run_count} completed",
                completed=i + 1,
            )
    # except Exception as ex:
    # print(ex)
    # pass
    Instance.dump(config.out_dir, solver.name, instances)
    return instances


async def main():
    random.seed(config.seed)
    inputs = list(Input.generate())
    csp.Instance.dump_inputs(config.out_dir, inputs)
    asp.Instance.dump_inputs(config.out_dir, inputs)

    minizinc = csp.Solver()
    console.rule(title=minizinc.name)
    minizinc_instances = await solve_all(inputs, minizinc)
    if minizinc_instances:
        console.print(
            csp.Statistics.summarize(
                [instance.statistics for instance in minizinc_instances]
            ),
            end="\n\n",
        )
    clingo = asp.Solver()
    console.rule(clingo.name)
    clingo_instances = await solve_all(inputs, clingo)
    if clingo_instances:
        console.print(
            asp.Statistics.summarize(
                [instance.statistics for instance in clingo_instances]
            ),
            end="\n\n",
        )


if __name__ == "__main__":
    try:
        asyncio.run(main())
    except KeyboardInterrupt:
        console.print("Control-C", style="logging.level.error")
    exit(0)
