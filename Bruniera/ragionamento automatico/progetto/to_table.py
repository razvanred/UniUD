#!/bin/env python3

import re
import sys

regex_asp = r"Time *: ([0-9]+(.[0-9]+)?s) \(Solving: ([0-9]+(.[0-9]+)?s).*\)"
regex_cop = r".*elapsed: ([0-9]+(.[0-9]+)?) s.*"
regex_names = r"benchmark/asp/(.*)\.lp"
regex_result = r"(=====)?(([A-Z]*SAT)([A-Z]*))(=====)?|(total.*)"

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

#print(result_cop)
#print(result_asp)

f = open("results/table.txt", "w+")

f.write("\\begin{tabular}{|c||c|c|c|c|}\n\
\\hline\n\
\\multirow{2}{3em}{Test} & \\multicolumn{2}{c|}{Minizinc} & \\multicolumn{2}{c|}{ASP}\\\\\n\
\\cline{2-5}\n\
& Ottimo & Tempo & Risultato & Tempo totale (Solving)\\\\\n\
\\hhline{|=||=|=|=|=|}\n")

for i in range(0,36):
    f.write(f"{names[i]} & {''.join((result_cop[i][2],result_cop[i][5]))} & {times_cop[i][0]}s & {result_asp[i][2]} & {times_asp[i][0]} ({times_asp[i][2]})\\\\\n")
    f.write("\\hline\n")

f.write("\\end{tabular}")
f.close()