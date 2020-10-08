#!/bin/bash
echo "$1 Output:"
./_build/impc.native tests/$1.imp &&
java -jar Mars4_5.jar nc tests/$1.asm