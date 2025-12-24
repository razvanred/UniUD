import asp

__all__ = ["Solver"]


class Solver(asp.Solver):
    def __init__(self):
        self.name = "clingo (unrolled)"
        with open("model-unrolled.lp") as file:
            self._src = file.read()
