#!/bin/bash
#SBATCH --mail-user=brenton.kenkel@gmail.com
#SBATCH --mail-type=ALL
#SBATCH --nodes=10
#SBATCH --tasks-per-node=1
#SBATCH --time=24:00:00
#SBATCH --mem=2000M
#SBATCH --output=15-train.out

setpkgs -a R_3.2.0_gcc
setpkgs -a openmpi_1.8.4

srun --mpi=pmi2 Rscript 15-train.r
