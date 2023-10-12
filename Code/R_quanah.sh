#!/bin/bash

source ~/.bashrc
randomseed=$1
filename=$2
runTimeName=$3

module load intel R

for n in 100 200
do
	for sd in 1 1
	do
		Rscript $PATH_TO_KRR/Code/$filename.R $n $sd $randomseed $SLURM_JOB_ID $runTimeName $SLURM_JOB_NAME;
	done
done
