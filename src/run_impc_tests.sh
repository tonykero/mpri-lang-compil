#!/bin/bash

for filename in impc/tests/*.imp; do
    echo "===================="
    script=$(basename "$filename")
    script="${script%.*}"
    ./run_impc.sh "$script"
done