#!/bin/bash
./_build/impc.native tests/$1.imp &&
java -jar Mars4_5.jar tests/$1.asm