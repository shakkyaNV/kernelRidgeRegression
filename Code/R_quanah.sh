#!/bin/bash

randomseed=$1
filename=$2

module load intel R

for n in 100 200
do
	for sd in 0.02 0.05
	do
		Rscript ./$filename.R $n $sd $randomseed $SLURM_JOB_ID $SLURM_JOB_NAME;
	done
done
