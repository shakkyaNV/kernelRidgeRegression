#!/bin/bash

# Source the config file
source ~/myenv.conf

# Setup 
JOBNAME=test_R
#SBATCH --chdir=./
#SBATCH --job-name=$JOBNAME
#SBATCH --output=$PATH_TO_LOGS/$JOBNAME-$(date + "%j-%H-%M").out
#SBATCH --error=$PATH_TO_LOGS/$JOBNAME-$(date + "%j-%H-%M").err
#SBATCH --partition quanah
#SBATCH --nodes=$NODES
#SBATCH --ntasks=$NTASKS
#SBATCH --time=$TIME
#SBATCH --mail-type=$MAILTYPE
#SBATCH --mail-user=$MAILUSER

# Set the number of threads of MKL (following assigns same cpus specified in sbatch)
# export MKL_NUM_THREADS=$NSLOTS

echo "Output file now $OUTPUT"
cd $PATH_TO_CODE/test_R/
module load intel R
randomseed=$1

# Execute R codes in parallel
for n in {100, 200}
do
  for m in {0.02, 0.05}
  do
    Rscript ./R_main.R $n $m $randomseed
  done
done
