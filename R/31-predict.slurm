#!/bin/bash
#SBATCH --mail-user=brenton.kenkel@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --ntasks=1
#SBATCH --time=4:00:00
#SBATCH --mem=8000M
#SBATCH --array=0-191
#SBATCH --output=31-predict-%a.out

setpkgs -a R_3.2.0_gcc

echo "SLURM_JOBID: " $SLURM_JOBID
echo "SLURM_ARRAY_TASK_ID: " $SLURM_ARRAY_TASK_ID
echo "SLURM_ARRAY_JOB_ID: " $SLURM_ARRAY_JOB_ID

# Array IDs seem to be capped at 1001, so we have to hack the year
COW_YEAR=`expr 1816 + $SLURM_ARRAY_TASK_ID`
echo "COW_YEAR: " $COW_YEAR

Rscript 31-predict.r $COW_YEAR 1
