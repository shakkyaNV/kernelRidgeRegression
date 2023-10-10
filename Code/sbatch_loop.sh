#!/bin/bash

source ~/myenv.conf
JOB_NAME='multiD'
APPEND=$(date +'%b-%d-%Y')

for i in {100..102};
do
  sbatch              \
    --job-name=$JOB_NAME \
    --output=$PATH_TO_LOGS/${JOB_NAME}_${APPEND}_%j.out \
    --error=$PATH_TO_LOGS/${JOB_NAME}_${APPEND}_%j.err \
    --partition=quanah \
    --nodes=$NODES \
    --ntasks=$NTASKS \
    --time=$TIME \
    --mail-type=NONE \
    --mail-user=$MAILUSER \
    $PATH_TO_CODE/R_quanah.sh $i $JOB_NAME
done