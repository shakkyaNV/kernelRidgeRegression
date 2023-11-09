#!/bin/bash

source ~/.bashrc
randomseed=$1
filename=$2
runTimeName=$3
multiplier=0

module load intel R

for n in 100 150
do
	for sd in 0.01 0.02
	do
		Rscript $PATH_TO_KRR/Code/$filename.R $n $sd $randomseed $SLURM_JOB_ID $multiplier $runTimeName $SLURM_JOB_NAME;
	done
done
