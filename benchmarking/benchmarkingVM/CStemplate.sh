#!/bin/bash -e

#SBATCH --job-name   	 JOBID
#SBATCH --account    	 nesi00390
#SBATCH --time       	 TOTALTIME:00:00
#SBATCH --ntasks     	 1
#SBATCH --nodes	     	 1
#SBATCH --mem		 	 MEMORY
#SBATCH --cpus-per-task	 4
#SBATCH --output    	 JOBID.txt
#SBATCH --error      	 JOBID.err


module load beagle-lib/3.0.1-gimkl-2017a
module load Java/1.8.0_144


~/beast/bin/beast -seed RANDOMSEED -overwrite -beagle_SSE tmp.xml
rm tmp.xml





