#!/bin/bash

#SBATCH --job-name=gbif-join-raw-and-processed-occ-data     # Job name
#SBATCH --mail-type=ALL                  # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=jtmiller@ucsb.edu    # Where to send mail
#SBATCH --output=/blue/guralnick/millerjared/PlantSweepeR/logs/003-join-raw-and-processed-data/join-gbif%j.log                   # Standard output and error log
#SBATCH --nodes=1                        # Run all processes on a single node
#SBATCH --ntasks=1                       # Run a single task
#SBATCH --cpus-per-task=1               # Number of CPU cores per task
#SBATCH --mem=800gb                       # Job memory request
#SBATCH --time=00-48:00:00               # Time limit days-hrs:min:sec
#SBATCH --account=guralnick
#SBATCH --qos=guralnick-b
pwd; hostname; date

#load modules

module load R/4.5

#do some (or alot) of coding
Rscript --vanilla /blue/guralnick/millerjared/PlantSweepeR/code/finished/R/003B-gbif-join-raw-and-processed-occ-data.R

## example for running: sbatch /blue/guralnick/millerjared/BoCP/finished-code/bash/002B-join-raw-and-processed-gbif-data-scheduler.sh
