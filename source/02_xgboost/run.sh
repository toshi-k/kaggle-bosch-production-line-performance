#!/bin/sh

for i in `seq 1000 1010`
do
	echo "run with seed: ${i}"
	Rscript main.r ${i}
done
