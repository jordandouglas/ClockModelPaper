#!/bin/sh


rm *.tiff
Rscript toTiff.R

# Real data   
for t in *.tiff;
do

	echo "$t"
	tiffcp $t "F$t" -c lzw
	rm $t
done




#cd benchmarkingVM
#Rscript ../render.R