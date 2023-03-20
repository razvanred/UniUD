#!/bin/env python3

import random

def random_graph(size):
    zinc="["
    asp=""
    for i in range(0,size):
        zinc += "|"
        for j in range(0,size):
            length=random.randint(-20,20)
            if length >= 0 and i != j:
                zinc += f"{length},"
                asp += f"strada({i},{j},{length}).\n"
            else:
                zinc += "-1,"
        zinc += "\n"
    zinc += "|]\n"
    return (zinc,asp)

graph = random_graph(5)
print(graph[0])
print(graph[1])