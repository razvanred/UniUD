#!/bin/env python3

# Questo script genera le istanze casuali

import random
import sys

def random_graph(size):
    zinc="["
    asp=""
    for i in range(0,size):
        zinc += "|"
        for j in range(0,size):
            length=random.randint(-25,20)
            if length >= 0 and i != j:
                zinc += f"{length},"
                asp += f"strada({i},{j},{length}).\n"
            else:
                zinc += "-1,"
        zinc += "\n"
    zinc += "|]\n"
    return (zinc,asp)

def generate(size, name):
    graph = random_graph(size)
    f = open(f"benchmark/minizinc/{name}.dzn", "w+")
    f.write(f"start = 1;\nlimit = 6;\nn = {size};\nm={graph[0]};")
    f.close()
    f = open(f"benchmark/asp/{name}.lp", "w+")
    f.write(f"start(0).\nlimit(6).\nn({size}).\n{graph[1]}")
    f.close()

def main():
    generate(6, "easy1")
    generate(10, "easy2")
    generate(12, "easy3")

    for i in range(1,31):
        generate(14, f"mid{i}")

    generate(30, "hard1")
    generate(50, "hard2")
    generate(100, "hard3")

if __name__ == "__main__":
    main()