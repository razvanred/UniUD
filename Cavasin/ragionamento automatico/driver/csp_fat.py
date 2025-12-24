from datetime import timedelta

import config
import csp
import minizinc as MZN
from solver import Input
from utils import reimport_module

__all__ = ["Solver"]


class Solution(csp.Solution):
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


_csp = reimport_module(csp, __name__)
_csp.Solution = Solution  # pyright: ignore[reportAttributeAccessIssue]


class Solver(_csp.Solver):
    def __init__(self):
        self.name = config.csp_solver_label + " (fat)"
        solver = MZN.Solver.lookup(config.csp_solver_label)
        model = MZN.Model("model-fat.mzn")
        self._minizinc = MZN.Instance(solver, model)
