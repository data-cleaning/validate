#!/bin/bash

# helper script for quick compilation. Not needed for pkg building.

cd pkg/src
rm --verbose *.o *.so
gcc -std=gnu99 -I/usr/share/R/include -DNDEBUG -fpic -fopenmp -O2 -Wall -pipe  -g  -c *.c 

gcc -std=gnu99 -shared -o validate.so *.o -L/usr/lib/R/lib -lR
cd ../../

