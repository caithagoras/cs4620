#!/bin/bash

file=$1;

./dcc < "samples/$file.decaf" &> cpr
diff -s cpr "samples/$file.out"
