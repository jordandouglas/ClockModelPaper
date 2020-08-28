#!/bin/bash -e

#SBATCH --job-name   	 simu17_cat_adaptive
#SBATCH --account    	 nesi00390
#SBATCH --time       	 20:00:00
#SBATCH --ntasks     	 1
#SBATCH --nodes	     	 1
#SBATCH --mem		 	 4000
#SBATCH --cpus-per-task	 4
#SBATCH --output    	 simu17_cat_adaptive.txt
#SBATCH --error      	 simu17_cat_adaptive.err


module load beagle-lib/3.0.1-gimkl-2017a
module load Java/1.8.0_144


~/beast/bin/beast -seed 13594 -overwrite -beagle_SSE tmp.xml
rm tmp.xml





