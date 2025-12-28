import importlib.util
import random
import sys
from collections.abc import Collection, Iterable
from datetime import timedelta
from os import PathLike
from pathlib import Path
from types import ModuleType
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from _typeshed import StrPath

__all__ = ["str_tuple", "str_timedelta", "random_distribute", "reimport_module"]


def str_tuple(v: Iterable[object]):
    return "(" + ", ".join(str(e) for e in v) + ")"


def str_timedelta(timedelta: timedelta):
    total_seconds = timedelta.total_seconds()
    hours = int(total_seconds // 3600)
    minutes = int((total_seconds % 3600) // 60)
    seconds = total_seconds % 60

    return f"{hours}:{minutes:02d}:{seconds:05.2f}"


def random_distribute(
    marginal_population: int, k: int, marginal_support: Collection[int] | None = None
) -> list[int]:
    if marginal_support is None:
        marginal_support = range(marginal_population)
    if marginal_population == 0 or len(marginal_support) == 0:
        return [0] * marginal_population
    v: list[int] = [
        random.choice(range(k + 1)) if i in marginal_support else 0
        for i in range(marginal_population)
    ]
    v = [n * k // max(1, sum(v)) for n in v]
    for _ in range(k - sum(v)):
        lucky, *_ = random.choices(
            list(marginal_support), [v[i] + 1 for i in marginal_support]
        )
        v[lucky] += 1
    return v


def reimport_module(module: ModuleType, copy_name: str) -> ModuleType:
    copy_name = f"{module.__name__}_{copy_name}"
    spec = importlib.util.spec_from_file_location(copy_name, module.__file__)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    sys.modules[copy_name] = module
    spec.loader.exec_module(module)  # pyright: ignore[reportOptionalMemberAccess]
    return module


# async def tests():
#     async def csp_battery():
#         random.seed(config.seed)
#         inputs = list(Input.generate())
#         csp.Instance.dump_inputs(config.out_dir, inputs)

#         minizinc = csp_channeled_unrolled.Solver()
#         console.rule(title=minizinc.name)
#         minizinc_instances = await solve_all(inputs, minizinc)
#         if minizinc_instances:
#             console.print(
#                 csp.Statistics.summarize(
#                     [instance.statistics for instance in minizinc_instances]
#                 ),
#                 end="\n\n",
#             )

#         minizinc = csp_channeled.Solver()
#         console.rule(title=minizinc.name)
#         minizinc_instances = await solve_all(inputs, minizinc)
#         if minizinc_instances:
#             console.print(
#                 csp.Statistics.summarize(
#                     [instance.statistics for instance in minizinc_instances]
#                 ),
#                 end="\n\n",
#             )

#         minizinc = csp.Solver()
#         console.rule(title=minizinc.name)
#         minizinc_instances = await solve_all(inputs, minizinc)
#         if minizinc_instances:
#             console.print(
#                 csp.Statistics.summarize(
#                     [instance.statistics for instance in minizinc_instances]
#                 ),
#                 end="\n\n",
#             )

#         minizinc = csp_fat.Solver()
#         console.rule(title=minizinc.name)
#         minizinc_instances = await solve_all(inputs, minizinc)
#         if minizinc_instances:
#             console.print(
#                 csp.Statistics.summarize(
#                     [instance.statistics for instance in minizinc_instances]
#                 ),
#                 end="\n\n",
#             )

#     async def asp_battery():
#         random.seed(config.seed)
#         inputs = list(Input.generate())
#         asp.Instance.dump_inputs(config.out_dir, inputs)

#         clingo = asp.Solver()
#         console.rule(clingo.name)
#         clingo_instances = await solve_all(inputs, clingo)
#         if clingo_instances:
#             console.print(
#                 asp.Statistics.summarize(
#                     [instance.statistics for instance in clingo_instances]
#                 ),
#                 end="\n\n",
#             )

#         clingo = asp_unrolled.Solver()
#         console.rule(clingo.name)
#         clingo_instances = await solve_all(inputs, clingo)
#         if clingo_instances:
#             console.print(
#                 asp.Statistics.summarize(
#                     [instance.statistics for instance in clingo_instances]
#                 ),
#                 end="\n\n",
#             )

#         clingo = asp_indexed.Solver()
#         console.rule(clingo.name)
#         clingo_instances = await solve_all(inputs, clingo)
#         if clingo_instances:
#             console.print(
#                 asp.Statistics.summarize(
#                     [instance.statistics for instance in clingo_instances]
#                 ),
#                 end="\n\n",
#             )

#     # default config
#     config.seed = 372845237
#     config.bottles_quantity = 10
#     # await csp_battery()
#     # await asp_battery()

#     config.seed = 344450736
#     config.bottles_quantity = 12
#     # await csp_battery()
#     # await asp_battery()

#     config.seed = 1577213729
#     config.bottles_quantity = 16
#     config.box_sizes_quantity = 3
#     # await csp_battery()
