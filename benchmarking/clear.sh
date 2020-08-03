#!/bin/sh





#settings=("real" "cat" "quant" "AVMN" "bactrian98" "bactrian95" "NER" "real_06" "quant_06")
datasets=("Sauquet_2011" "Moyle_2016" "simulated" "Dornburg_2012" "Broughton_2013" "Ran_2018_dna" "Cannon_2016_dna" "Rightmyer_2013" "Kawahara_2013" "Cognato_2001")





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



	         rm *.log
	         rm *.trees



	        cd ../
	    done
	    

    	#sleep 1

   	 	cd ../
	done

	cd ../

done




