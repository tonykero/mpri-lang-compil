#!/bin/bash
echo "$1 Output:"
./_build/impc_reference/impc.native ./impc/tests/$1.imp &&
java -jar ./impc/Mars4_5.jar nc ./impc/tests/$1.asm