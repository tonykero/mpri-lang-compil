#!/bin/bash
echo "$1 Output:"
./_build/func.native tests/$1.fun && cat tests/$1.imp