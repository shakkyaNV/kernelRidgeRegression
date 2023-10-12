#!/bin/bash

source ~/.bashrc

# Check if the first argument is provided
if [ -n "$1" ]; then
  runTimeName="$1"
else
  # Set a default value if no argument is provided
  runTimeName="defTest"
fi

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
    $PATH_TO_KRR/Code/R_quanah.sh $i $JOB_NAME $runTimeName
done
