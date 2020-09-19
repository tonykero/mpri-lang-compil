#!/bin/bash

for filename in tests/*.imp; do
    script="$filename"
    asm="${filename%.*}.asm"
    echo "$script"
    ./_build/impc.native "$script" && java -jar Mars4_5.jar "$asm"
done