#!/bin/env python3

import re
import sys

regex_asp = r"Time *: ([0-9]+(.[0-9]+)?s) \(Solving: ([0-9]+(.[0-9]+)?s).*\)"
regex_cop = r".*elapsed: ([0-9]+(.[0-9]+)?) s.*"
regex_names = r"benchmark/asp/(.*)\.lp"
regex_result = r"(.*SAT.*)|(total.*)"

f = open("results/times-asp.txt", "r")
asp = f.read()
f.close()

f = open("results/times-minizinc.txt", "r")
cop = f.read()
f.close()

times_asp = re.findall(regex_asp, asp, 0)
times_cop = re.findall(regex_cop, cop, 0)
result_asp = re.findall(regex_result, asp, 0)
result_cop = re.findall(regex_result, cop, 0)
names = re.findall(regex_names, asp, 0)

f = open("results/table.txt", "w+")
for i in range(0,36):
    f.write(f"{names[i]} & {''.join(result_cop[i])} & {times_cop[i][0]}s $ {''.join(result_asp[i])} & {times_asp[i][0]}({times_asp[i][2]}2)\\\\\n")

f.close()