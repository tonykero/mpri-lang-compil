#!/bin/bash
echo "$1 Output:"
./_build/func/func.native ./func/tests/$1.fun && cat ./func/tests/$1.imp