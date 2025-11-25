#!/bin/bash

#SBATCH --job-name=Archive-Name-Matches-Scheduler             # Job name
#SBATCH --mail-type=FAIL,ARRAY_TASKS     # Mail events (NONE, BEGIN, END, FAIL, ALL)
#SBATCH --mail-user=jtmiller@ucsb.edu    # Where to send mail
#SBATCH --output=/blue/guralnick/millerjared/PlantSweepeR/logs/007-archive-name-matches/task%A-%a.out                 # Standard output and error log
#SBATCH --nodes=1                        # Run all processes on a single node
#SBATCH --ntasks=1                       # Run a single task
#SBATCH --cpus-per-task=1               # Number of CPU cores per task
#SBATCH --mem-per-cpu=30gb                   # Job memory request
#SBATCH --time=00-96:00:00               # Time limit days-hrs:min:sec
#SBATCH --array=1-60                  # Array Range
#SBATCH --account=guralnick             # We're using Guralnick
#SBATCH --qos=guralnick-b                # We're using Guralnick resources
pwd; hostname; date

# Set the number of runs that each SLURM task should do
PER_TASK=639

# Calc the starting and ending values for this task based on the SLURM task and the num of runs per task
START_NUM=$(( ($SLURM_ARRAY_TASK_ID - 1) * $PER_TASK + 1 ))
END_NUM=$(( $SLURM_ARRAY_TASK_ID * $PER_TASK ))

# Print the task and run range
echo This is task $SLURM_ARRAY_TASK_ID, which will do runs $START_NUM to $END_NUM

# Run the loop of runs for this task.
for (( run=$START_NUM; run<=END_NUM; run++ )); do
# Export these values as enviromental variables that R can read
export START_NUM=$run

  echo This is SLURM task $SLURM_ARRAY_TASK_ID, run number $run
  # Continue with run details...
#load modules

module load R/4.5

#do some (or alot) of coding
Rscript --vanilla /blue/guralnick/millerjared/PlantSweepeR/code/finished/R/007-archive-name-matches.R

done

date
## example for running: sbatch /blue/guralnick/millerjared/BoCP/finished-code/bash/004-build-species-tables-scheduler.sh