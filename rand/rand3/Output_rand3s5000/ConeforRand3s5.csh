#!/bin/tcsh
#BSUB -n 1
#BSUB -W 120:00
#BSUB -J Test
#BSUB -o stdout.%J
#BSUB -e stderr.%J
#BSUB -R "rusage[mem=10000]"
R CMD BATCH ConeforRand3s5.R