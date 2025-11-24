#!/bin/bash

#SBATCH --job-name=PlantSweepeR-Taxon-Alignment     # Job name
#SBATCH --mail-type=ALL                  # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=jtmiller@ucsb.edu    # Where to send mail
#SBATCH --output=/blue/guralnick/millerjared/PlantSweepeR/logs/taxon-alignment/align-wcvp-ncbi%j.log                  # Standard output and error log
#SBATCH --nodes=1                        # Run all processes on a single node
#SBATCH --ntasks=1                       # Run a single task
#SBATCH --cpus-per-task=1               # Number of CPU cores per task
#SBATCH --mem=100gb                       # Job memory request
#SBATCH --time=00-48:00:00               # Time limit days-hrs:min:sec
#SBATCH --account=guralnick
#SBATCH --qos=guralnick-b
pwd; hostname; date

#load modules

module load R/4.5

#do some (or alot) of coding
Rscript --vanilla /blue/guralnick/millerjared/PlantSweepeR/code/finished/R/001-taxon-alignment.R
