#!/bin/tcsh
#BSUB -n 1
#BSUB -W 200:00
#BSUB -J Test
#BSUB -o stdout.%J
#BSUB -e stderr.%J
#BSUB -R "rusage[mem=12000]"
R CMD BATCH ConeforRand2g1.R
