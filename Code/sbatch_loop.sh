#!/bin/bash

for i in {100..200};
do
  sbatch R_quanah.sh $i;
  echo "Submitted i: " $i;
done