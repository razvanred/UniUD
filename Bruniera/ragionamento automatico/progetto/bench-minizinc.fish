#!/usr/bin/fish
function minizinc-run -a file
    echo $file
    set output (minizinc --solver org.gecode.gecode minizinc/progetto.mzn $file --output-time --time-limit 300000 2>/dev/null | string split0)
    set temp (echo $output | grep -m2 -e 'elapsed' -e 'total' -e 'UNSAT' | string split0)
    echo -e "$file:\n$temp" >>results/times-minizinc.txt
    echo -e "$file:\n$output" >>results/solutions-minizinc.txt
end


rm results/*minizinc*

set files (ls benchmark/minizinc/easy*.dzn -v)
for file in $files
    minizinc-run $file
end
set files (ls benchmark/minizinc/mid*.dzn -v)
for file in $files
    minizinc-run $file
end
set files (ls benchmark/minizinc/hard*.dzn -v)
for file in $files
    minizinc-run $file
end
