#!/bin/bash

#SBATCH --job-name=symbiota-occ-data-formatting     # Job name
#SBATCH --mail-type=ALL                  # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=jtmiller@ucsb.edu    # Where to send mail
#SBATCH --output=/blue/guralnick/millerjared/PlantSweepeR/logs/002-data-formatting/symbiota-data-formatting%j.log                  # Standard output and error log
#SBATCH --nodes=1                        # Run all processes on a single node
#SBATCH --ntasks=1                       # Run a single task
#SBATCH --cpus-per-task=1               # Number of CPU cores per task
#SBATCH --mem=20gb                       # Job memory request
#SBATCH --time=00-96:00:00               # Time limit days-hrs:min:sec
#SBATCH --qos=soltis-b
pwd; hostname; date

#load modules

module load R/4.3

#do some (or alot) of coding
Rscript --vanilla /blue/guralnick/millerjared/PlantSweepeR/code/finished/R/002F-symbiota-occ-data-formatting.R

