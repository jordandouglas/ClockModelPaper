#!/bin/sh



scp -r jdou557@login02.fos.auckland.ac.nz/benchmarkingVM.zip .

#settings=("real" "cat" "quant" "AVMN" "bactrian98" "bactrian95" "NER" "real_06" "quant_06")
datasets=("Sauquet_2011" "Moyle_2016" "simulated" "Dornburg_2012" "Broughton_2013" "Ran_2018_dna" "Rightmyer_2013" "Kawahara_2013" "Cognato_2001")





 echo "Removing log and tree files"
              
# Real data   
for batch in {1..5}
do

	echo "Batch${batch}"
	cd "Batch${batch}"

	for d in ${datasets[@]} ; do
	    echo "Dataset $d"
	    cd $d

	    for e in */ ; do
	        echo "Partition $e"
	        cd $e


			for logfile in *.log; do
				echo $logfile
				#/mnt/e/STOREDFILES/BEAST2/linux/beast/bin/loganalyser -oneline ${logfile}.log -burnin 0 > ${logfile}.ess
			 done



	        cd ../
	    done
	    

    	#sleep 1

   	 	cd ../
	done

	cd ../

done




