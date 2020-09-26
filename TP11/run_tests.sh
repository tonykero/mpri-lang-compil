#!/bin/bash

for filename in tests/*.imp; do
    echo "===================="
    script=$(basename "$filename")
    script="${script%.*}"
    ./run.sh "$script"
done