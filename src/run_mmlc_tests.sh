#!/bin/bash

for filename in mmlc/tests/*.mml; do
    echo "==============="
    script=$(basename "$filename")
    script="${script%.*}"
    ./run_mmlc.sh "$script"
done