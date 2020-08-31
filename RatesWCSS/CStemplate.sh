#!/bin/bash -e

#SBATCH --job-name   	 quant_SIM
#SBATCH --account    	 nesi00390
#SBATCH --time       	 20:00:00
#SBATCH --ntasks     	 1
#SBATCH --nodes	     	 1
#SBATCH --mem		 500
#SBATCH --cpus-per-task	 1
#SBATCH --output    	 O_SIM.out
#SBATCH --error      	 E_SIM.err


module load beagle-lib/3.0.1-gimkl-2017a
module load Java/1.8.0_144


~/beast/bin/beast -overwrite -seed RANDOMSEED -beagle_SSE runSIM.xml
