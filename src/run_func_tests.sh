#!/bin/bash

for filename in func/tests/*.fun; do
    echo "==============="
    script=$(basename "$filename")
    script="${script%.*}"
    ./run_func.sh "$script"
done