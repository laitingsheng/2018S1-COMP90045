#!/bin/bash

cat $1
./Paz -p $1 2> .trace > output
cat output
diff -i output $1
rm output
