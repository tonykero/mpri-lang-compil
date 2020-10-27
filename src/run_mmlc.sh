#!/bin/bash
echo "$1 Output:"
./_build/mmlc/mmlc.native ./mmlc/tests/$1.mml && #cat ./mmlc/tests/$1.fun &&
./_build/func/func.native ./mmlc/tests/$1.fun && #cat ./mmlc/tests/$1.imp &&
./_build/impc/impc.native ./mmlc/tests/$1.imp &&
java -jar ./impc/Mars4_5.jar nc ./mmlc/tests/$1.asm
