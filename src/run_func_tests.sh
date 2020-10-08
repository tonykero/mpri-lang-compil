#!/bin/bash

for filename in func/tests/*.fun; do
    echo "=====compile======"
    script=$(basename "$filename")
    script="${script%.*}"
    ./run_func.sh "$script"
    echo "=====execute======"
    script="${filename%.*}"
    ./_build/impc/impc.native "${script}.imp" &&
    java -jar ./impc/Mars4_5.jar nc "${script}.asm"
done