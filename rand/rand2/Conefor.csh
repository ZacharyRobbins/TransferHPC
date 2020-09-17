#!/bin/tcsh
#BSUB -n 1
#BSUB -W 120:00
#BSUB -J Test
#BSUB -o stdout.%J
#BSUB -e stderr.%J
#BSUB -R "rusage[mem=6000]"
R CMD BATCH Coneforcluster1.R
