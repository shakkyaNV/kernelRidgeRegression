#!/bin/bash

source ~/.bashrc

JOB_NAME='multiD'
APPEND=$(date +'%H-%M-%b-%d-%Y')

for i in {100..102};
do
  sbatch              \
    --job-name=$JOB_NAME \
    --output=$PATH_TO_KRR/Logs/${JOB_NAME}_${APPEND}_%j.out \
    --error=$PATH_TO_KRR/Logs/${JOB_NAME}_${APPEND}_%j.err \
    --partition=quanah \
    --nodes=$NODES \
    --ntasks=$NTASKS \
    --time=$TIME \
    --mail-type=NONE \
    --mail-user=$MAILUSER \
    $PATH_TO_KRR/Code/R_quanah.sh $i $JOB_NAME
done
