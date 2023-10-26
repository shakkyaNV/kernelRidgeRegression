#!/bin/bash

source ~/.bashrc
randomseed=$1
filename=$2
runTimeName=$3

module load intel R

for n in 250 350
do
	for sd in 0.01 0.02
	do
		Rscript $PATH_TO_KRR/Code/$filename.R $n $sd $randomseed $SLURM_JOB_ID $runTimeName $SLURM_JOB_NAME;
	done
done
