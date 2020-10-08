#!/bin/bash
echo "$1 Output:"
./_build/impc/impc.native ./impc/tests/$1.imp &&
java -jar ./impc/Mars4_5.jar ./impc/tests/$1.asm