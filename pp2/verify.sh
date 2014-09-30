#!/bin/bash

./dcc < z.in > z.out
solution/dcc < z.in > z.ans
diff -sy z.ans z.out
