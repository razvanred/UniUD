#!/bin/env python3

import random

def random_graph(size):
    text="["
    for _i in range(0,size):
        text += "|"
        for _j in range(0,size):
            text += f"{random.randint(-23,20)},"
        text += "\n"
    text += "|]"
    return text

print(random_graph(100))
