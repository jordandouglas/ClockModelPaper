#!/bin/bash -e

#SBATCH --job-name   	 Ran_1_bactrian95
#SBATCH --account    	 nesi00390
#SBATCH --time       	 60:00:00
#SBATCH --ntasks     	 1
#SBATCH --nodes	     	 1
#SBATCH --mem		 	 8000
#SBATCH --cpus-per-task	 4
#SBATCH --output    	 output.txt
#SBATCH --error      	 error.err


module load beagle-lib/3.0.1-gimkl-2017a
module load Java/1.8.0_144


~/beast/bin/beast -seed 30074 -overwrite -beagle_SSE tmp.xml







