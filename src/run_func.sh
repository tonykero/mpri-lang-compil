#!/bin/bash
echo "$1 Output:"
./_build/func/func.native ./func/tests/$1.fun && cat ./func/tests/$1.imp &&

./_build/impc/impc.native ./func/tests/$1.imp &&
java -jar ./impc/Mars4_5.jar nc ./func/tests/$1.asm
