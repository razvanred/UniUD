import argparse
from fractions import Fraction
from functools import reduce
import random

parser = argparse.ArgumentParser()
parser.add_argument("exp", type=int)

args = parser.parse_args()


size = 2**args.exp
img = [
    [
        tuple(random.choice([[100, 150, 200], random.choices(range(0, 255), k=3)]))
        for _ in range(size)
    ]
    for _ in range(size)
]
mask = [
    [True if random.randint(0, 1) else False for _ in range(size)] for _ in range(size)
]
num = sum(cell for row in mask for cell in row)
r = (
    0
    if num == 0
    else (
        reduce(
            lambda acc, t: (
                acc
                + Fraction(1, num)
                * (
                    t[0][0] * Fraction(2989, 10000)
                    + t[0][1] * Fraction(5870, 10000)
                    + t[0][2] * Fraction(1140, 10000)
                )
                if t[1]
                else acc
            ),
            zip(sum(img, []), sum(mask, [])),
            Fraction(0),
        )
    )
)


def treeify(mat):
    size = len(mat)
    if size > 1:
        top_left = [mat[i][: size // 2] for i in range(size // 2)]
        top_right = [mat[i][size // 2 :] for i in range(size // 2)]
        bot_left = [mat[i][: size // 2] for i in range(size // 2, size)]
        bot_right = [mat[i][size // 2 :] for i in range(size // 2, size)]
        return (
            "Q",
            treeify(top_left),
            treeify(top_right),
            treeify(bot_left),
            treeify(bot_right),
        )
    else:
        return ("C", mat[0][0])


def compress(mat):
    match mat:
        case ("Q", a, b, c, d):
            t = ("Q", compress(a), compress(b), compress(c), compress(d))
            match t:
                case ("Q", ("C", a), ("C", b), ("C", c), ("C", d)) if a == b == c == d:
                    return ("C", a)
            return t
        case _:
            return mat


def serializeTree(tree):
    match tree:
        case ("Q", a, b, c, d):
            return f"(Q {serializeTree(a)} {serializeTree(b)} {serializeTree(c)} {serializeTree(d)})"
        case ("C", v):
            return f"(C {v})"


print(f"size {size}\n")
print(serializeTree(compress(treeify(img))), end=" ")
print(serializeTree(compress(treeify(mask))), end="\n\n")
print(r)
print(float(r))
