import random
from collections.abc import Collection, Iterable
from datetime import timedelta

__all__ = ["str_tuple", "str_timedelta", "random_distribute"]


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
