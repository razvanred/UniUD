from collections.abc import Sequence
from datetime import timedelta
from pathlib import Path
from typing import Final, Literal

from solver import BottleSize, BoxSize

bottle_sizes: Final[dict[str, BottleSize]] = {
    # demi or half
    "demi": BottleSize(375, 24, 6),
    # standard or bordeaux
    "standard": BottleSize(750, 30, 8),
    # 1L
    "1L": BottleSize(1000, 32, 8),
    # magnum
    "magnum": BottleSize(1500, 35, 9),
    # jeroboam
    "jeroboam": BottleSize(3000, 47, 13),
}
usable_bottle_sizes: Final[set[str]] = {
    "demi",
    "standard",
}  # set(range(len(bottleSizes)))
box_sizes: Final[Sequence[BoxSize]] = [
    BoxSize(11, 11, 100, 40),
    BoxSize(20, 11, 100, 20),
    BoxSize(17, 30, 100, 50),
    BoxSize(18, 18, 25, 60),
]
box_sizes_quantity: Final[int] = 2
bottles_quantity: Final[int] = 12
demijohns_number: Final[int] = 10
demijohn_capacity: Final[int] = 1000
box_widths: Final[Sequence[int]] = [15, 25, 30, 40]
box_ratios: Final[Sequence[float]] = [1.0, 1.6, 2.0]
box_heights: Final[Sequence[int]] = [30, 35, 50]
box_occupancies: Final[Sequence[int]] = [50, 60, 70]

seed: Final[int] = 230706628
run_count: Final[int] = 2
time_limit: Final[timedelta] = timedelta(minutes=3)
processes: Final[int] = 6
out_dir: Final[Path] = Path("out")

csp_solver_label: Final[str] = "gecode"

asp_parallel_mode: Final[Literal["compete", "split"]] = "split"

assert box_sizes_quantity <= len(box_sizes)
