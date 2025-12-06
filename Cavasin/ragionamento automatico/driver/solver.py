import csv
import random
import time
from abc import ABC, abstractmethod
from collections.abc import (
    AsyncIterator,
    Iterable,
    Iterator,
    Sequence,
)
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any, Hashable, NamedTuple, Protocol

from clingo import Configuration

import config
import rich.box
from rich.columns import Columns
from rich.console import RenderResult
from rich.panel import Panel
from rich.table import Column, Table
from rich.text import Text
from utils import random_distribute, str_timedelta, str_tuple

__all__ = ["BottleSize", "Batch", "BoxSize", "Input", "Instance", "Solver"]


class BottleSize(NamedTuple):
    capacity: int
    height: int
    diameter: int


class Batch(NamedTuple):
    quantity: int
    capacity: int
    height: int
    diameter: int


class BoxSize(NamedTuple):
    width: int
    length: int
    height: int
    min_occupancy: int


class Input(NamedTuple):
    demijohns_number: int
    demijohn_capacity: int
    batches: dict[str, Batch]
    box_sizes: Sequence[BoxSize]

    @staticmethod
    def generate():
        for _ in range(config.run_count):
            yield Input(
                config.demijohns_number,
                config.demijohn_capacity,
                {
                    key: Batch(quantity, **(config.bottle_sizes[key]._asdict()))
                    for (key, quantity) in zip(
                        (
                            key
                            for key in config.bottle_sizes.keys()
                            if key in config.usable_bottle_sizes
                        ),
                        random_distribute(
                            len(config.usable_bottle_sizes),
                            config.bottles_quantity,
                        ),
                    )
                    if quantity > 0
                },
                [
                    BoxSize(
                        (width := random.choice(config.box_widths)),
                        int(width * random.choice(config.box_ratios)),
                        random.choice(config.box_heights),
                        random.choice(config.box_occupancies),
                    )
                    for _ in range(config.box_sizes_quantity)
                ]
                if config.box_sizes is None
                else random.sample(config.box_sizes, config.box_sizes_quantity),
            )

    def __rich_console__(self, console, options) -> RenderResult:
        table = Table(
            "wine",
            "bottles",
            title="Instance",
            title_justify="left",
            box=rich.box.MINIMAL,
        )
        for name in config.bottle_sizes.keys():
            table.add_column(name)
        table.add_row(
            f"{(self.demijohns_number * self.demijohn_capacity) / 1000}L",
            str(sum(batch.quantity for batch in self.batches.values())),
            *(
                str(self.batches[key].quantity if key in self.batches else 0)
                for key in config.bottle_sizes.keys()
            ),
        )
        yield table
        yield Columns(
            (
                Panel.fit(
                    f"{box_size.width * box_size.length * box_size.height}cm³, {box_size.min_occupancy}%({int(box_size.width * box_size.length * box_size.height * box_size.min_occupancy / 100)}cm³)",
                    title=f"[#8C603A]{box_size.width}[/]x[#F2C6A0]{box_size.length}[/]x[#BF8C60]{box_size.height}[/]",
                    title_align="left",
                    padding=(0, 0),
                )
                for box_size in self.box_sizes
            )
        )


class Solution(ABC):
    class Bottle(NamedTuple):
        batch: Batch
        box: "Solution.Box"

    class Box(NamedTuple):
        id: int
        size: BoxSize

    input: Input
    objective: int
    runtime: timedelta
    best: bool
    used_bottles: Sequence["Solution.Bottle"]
    used_boxes: Sequence["Solution.Box"]
    batch_counters: dict[Batch, int]
    box_counters: dict[BoxSize, int]
    box_occupancies: dict["Solution.Box", int]

    def _fill_out(self):
        def count[T: Hashable](items: Iterable[T], keys: Iterable[T]):
            counter = {key: 0 for key in items}
            for key in keys:
                counter[key] += 1
            return counter

        self.batch_counters = count(
            self.input.batches.values(), (bottle.batch for bottle in self.used_bottles)
        )
        self.box_counters = count(
            self.input.box_sizes, (box.size for box in self.used_boxes)
        )
        self.box_occupancies = {
            box: round(
                100
                * sum(
                    bottle.batch.capacity
                    for bottle in self.used_bottles
                    if bottle.box == box
                )
                / (box.size.width * box.size.length * box.size.height)
            )
            for box in self.used_boxes
        }

    def as_table_row(self) -> Iterator[Text]:
        return (
            Text(field, style="green" if self.best else "")
            for field in (
                str_timedelta(self.runtime),
                f"{self.objective / 1000}L",
                f"{len(self.used_bottles)}: {str_tuple(self.batch_counters.values())}",
                f"{len(self.used_boxes)}: {str_tuple(self.box_counters.values())}",
                str_tuple(f"{e}%" for e in self.box_occupancies.values()),
            )
        )


class Statistics[T](ABC):
    raw: dict[str, Any]

    def __init__(self):
        self.raw = {}

    @property
    @abstractmethod
    def total_time(self) -> timedelta: ...

    @staticmethod
    @abstractmethod
    def summarize(statistics: Sequence[T]) -> str: ...

    @abstractmethod
    def asdict(self) -> dict[str, Any]: ...

    @abstractmethod
    def __str__(self) -> str: ...


class Instance(ABC):
    input: Input
    solutions: list[Solution]
    unsatisfiable: bool
    start_time: float
    _table: Table

    def __init__(self, input: Input):
        self.input = input
        self.solutions = []
        self.unsatisfiable = False
        self._table = Table(
            Column("runtime", no_wrap=True),
            Column("objective", no_wrap=True),
            Column("bottles counters", no_wrap=True),
            Column("boxes counters", no_wrap=True),
            Column("occupancies"),
            title="Solutions",
            title_justify="left",
            box=rich.box.MINIMAL,
        )
        self.start_time = time.monotonic()

    @property
    @abstractmethod
    def statistics(self) -> Statistics: ...

    @staticmethod
    @abstractmethod
    def dump_inputs(out_dir: Path, inputs: Iterable["Input"]) -> None: ...

    @staticmethod
    def dump(out_dir: Path, solver_name: str, instances: Iterable["Instance"]):
        p = (
            out_dir
            / f"{config.seed}-{datetime.now().strftime('%Y-%m-%dT%H%M%S')}-{solver_name}-runs.csv"
        )
        p.parent.mkdir(parents=True, exist_ok=True)
        with p.open("w", newline="") as file:
            writer = csv.DictWriter(
                file,
                ["instance", "runtime", "objective", "best"],
                extrasaction="ignore",
            )
            writer.writeheader()
            for i, run in enumerate(instances):
                dict = run.asdict()
                dict["instance"] = i
                writer.writerow(dict)
        pass

    def last_best(self):
        if self.solutions:
            self.solutions[-1].best = True
            self._table.rows[-1].style = "green"

    def asdict(self) -> dict[str, Any]:
        if self.solutions:
            return {
                "objective": self.solutions[-1].objective,
                "best": self.solutions[-1].best,
            }
        if self.unsatisfiable:
            return {"objective": "unsatisfiable"}
        return {}

    def __rich__(self) -> Table | Text:
        if self.unsatisfiable:
            return Text("unsatisfiable\n", style="bold green")
        elif self.solutions:
            return self._table
        else:
            return Text("no solutions found\n", style="strong")


def walk_config(node: Configuration, prefix: str | None = None):
    def prefix_lines(prefix: str, s: str):
        return "".join(prefix + s for s in s.splitlines(True))

    assert node.keys
    for key in node.keys:
        path = f"{prefix}.{key}" if prefix else key
        child = getattr(node, key)

        if getattr(child, "keys", None) is None:
            print(prefix_lines("\t# ", node.description(key)))
            print(f"\t{path} = {repr(child)}\n")
        else:
            print(prefix_lines("# ", node.description(key)))
            print(f"[{path}]\n")
            walk_config(child, path)


class Solver[T: Instance](Protocol):
    name: str

    def solve(self, input: Input) -> AsyncIterator[T]: ...
