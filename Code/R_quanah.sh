#!/bin/bash

source ~/.bashrc
randomseed=$1
filename=$2
runTimeName=$3
sd=0.5

module load intel R

for n in 100 150
do
	for multiplier in `seq 0.005 -0.0005 -0.005`
	do
		Rscript $PATH_TO_KRR/Code/$filename.R $n $sd $randomseed $SLURM_JOB_ID $multiplier $runTimeName $SLURM_JOB_NAME;
	done
done
