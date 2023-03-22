#!/usr/bin/fish
function asp-run -a file
    echo $file
    set output (clingo asp/progetto.lp $file --time-limit=300 2>/dev/null | string split0)
    set temp (echo $output | grep -m2 -e 'Time.*:' -e 'SAT' -e'UNK' | string split0)
    echo -e "$file:\n$temp" >>results/times-asp.txt
    echo -e "$file:\n$output" >>results/solutions-asp.txt
end


rm results/*asp*

set files (ls benchmark/asp/easy*.lp -v)
for file in $files
    asp-run $file
end
set files (ls benchmark/asp/mid*.lp -v)
for file in $files
    asp-run $file
end
set files (ls benchmark/asp/hard*.lp -v)
for file in $files
    asp-run $file
end
