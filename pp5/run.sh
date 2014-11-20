#!/bin/bash

./run z.in solution/dcc > z.ans
./run z.in ./dcc > z.out
diff -ys z.ans z.out 
