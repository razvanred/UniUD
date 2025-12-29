from collections.abc import Sequence
from datetime import timedelta
from pathlib import Path
from typing import Final, Literal

from solver import BottleSize, BoxSize

bottle_sizes: Final[dict[str, BottleSize]] = {
    "demi": BottleSize(375, 24, 6),
    "standard": BottleSize(750, 30, 8),
    "1L": BottleSize(1000, 32, 8),
    "magnum": BottleSize(1500, 35, 9),
    "jeroboam": BottleSize(3000, 47, 13),
}
usable_bottle_sizes: Final[set[str] | None] = None  # None for all
# usable_bottle_sizes: Final[set[str] | None] = {
#     "demi",
#     "1L",
# }
box_sizes: Final[Sequence[BoxSize] | None] = [
    BoxSize(20, 17, 35, 10),
    BoxSize(13, 13, 25, 20),
    BoxSize(13, 13, 47, 30),
    BoxSize(26, 27, 47, 20),
    BoxSize(26, 27, 32, 20),
    BoxSize(16, 25, 30, 20),
]  # None to generate them individually
box_sizes_quantity: Final[int] = 2
bottles_quantity: Final[int] = 10
demijohns_number: Final[int] = 10
demijohn_capacity: Final[int] = 10000
box_widths: Final[Sequence[int]] = [15, 25, 30, 40]
box_ratios: Final[Sequence[float]] = [1.0, 1.6, 2.0]
box_heights: Final[Sequence[int]] = [30, 35, 50]
box_occupancies: Final[Sequence[int]] = [5, 20, 25]

seed: Final[int] = 372845237
run_count: Final[int] = 15
time_limit: Final[timedelta] = timedelta(minutes=10)
processes: Final[int] = 4
out_dir: Final[Path] = Path("out")

csp_solver_label: Final[str] = "gecode"

asp_parallel_mode: Final[Literal["compete", "split"]] = "split"

assert box_sizes_quantity <= len(box_sizes)
