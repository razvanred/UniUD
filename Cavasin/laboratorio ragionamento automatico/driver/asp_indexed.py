from datetime import timedelta
from typing import cast

import asp
from asp import AtomIPC
from utils import reimport_module
from solver import Input

__all__ = ["Solver"]


class Solution(asp.Solution):
    def __init__(self, input: Input, runtime: timedelta, solution: "Solution.IPC"):
        # ("bottle", I, Capacity, BHeight, Diameter)
        def batch_key(atom: AtomIPC):
            return cast(tuple[int, int, int], atom[2:])

        # ("box", I, Width, Length, OHeight, MinOccupancy)
        def box_size_key(atom: AtomIPC):
            return cast(tuple[int, int, int, int], atom[2:])

        # ("bottle", I, Capacity, BHeight, Diameter)
        # ("placed", Bottle, Box, X, Y)
        def bottle_key(atom: AtomIPC):
            match atom:
                case ("bottle", *_):
                    return cast(int, atom[1])
                case ("placed", *_) | _:
                    return cast(int, atom[1])

        # ("box", I, Width, Length, OHeight, MinOccupancy)
        # ("placed", Bottle, Box, X, Y)
        def box_key(atom: AtomIPC):
            match atom:
                case ("box", *_):
                    return cast(int, atom[1])
                case ("placed", *_) | _:
                    return cast(int, atom[2])

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
        boxes = [atom for atom in solution.atoms if atom[0] == "box"]
        bottles = [atom for atom in solution.atoms if atom[0] == "bottle"]
        placements = [atom for atom in solution.atoms if atom[0] == "placed"]
        boxes_dict = {
            box_key(b): Solution.Box(box_key(b), box_sizes_dict[box_size_key(b)])
            for b in boxes
        }
        bottles_dict = {bottle_key(b): batches_dict[batch_key(b)] for b in bottles}

        self.used_boxes = [boxes_dict[box_key(p)] for p in placements]
        self.used_bottles = [
            Solution.Bottle(bottles_dict[bottle_key(p)], boxes_dict[box_key(p)])
            for p in placements
        ]

        self._fill_out()


_asp = reimport_module(asp, __name__)
_asp.Solution = Solution  # pyright: ignore[reportAttributeAccessIssue]


class Solver(_asp.Solver):
    def __init__(self):
        self.name = "clingo (indexed)"
        with open("model-indexed.lp") as file:
            self._src = file.read()
