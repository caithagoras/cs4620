#!/bin/bash

./dcc < z.in > z.out 2>&1
solution/dcc < z.in > z.ans 2>&1
diff -sy z.ans z.out
