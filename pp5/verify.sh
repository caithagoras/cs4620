#!/bin/bash

./dcc -d tac < z.in > z.out 2>&1
solution/dcc -d tac < z.in > z.ans 2>&1
diff -sy z.ans z.out

./dcc < z.in > z.out 2>&1
solution/dcc < z.in > z.ans 2>&1
diff -sq z.ans z.out
