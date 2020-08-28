#!/bin/sh



TEMPLATE="../../../../../CStemplate.sh"

#settings=("real" "real_adaptive" "cat" "quant" "AVMN" "bactrian98" "bactrian95" "NER" "real_06" "quant_06")
settings=("cat" "cat_adaptive" "real" "real_06" "real_adaptive" "quant_cached" "quant_cached_adaptive" "quant_06_cached" "bactrian95" "NER")

datasets=("simulated" "Ran_2018_dna" "Rightmyer_2013" "Broughton_2013" "Sauquet_2011" "Moyle_2016" "Dornburg_2012" "Kawahara_2013" "Cognato_2001")




# Randomly shuffle the datasets
#datasets_shuffled=( $(shuf -e "${datasets[@]}") )

for d in ${datasets[@]} ; do
    echo "$d"
    cd $d

	dshort="${d:0:4}"

    for e in */ ; do
        echo "$e"
        cd $e


			eshort="${e%?}"

          # Randomly shuffle the settings
          settings_shuffled=( $(shuf -e "${settings[@]}") )
          for s in "${settings_shuffled[@]}"
          do
		  
			mkdir -p $s
			rm -f Batch*
			cd $s
	  
			for ((i=1;i<=20;i++)); do
			
				mkdir -p "Batch$i"
				cd "Batch$i"
				rand=$RANDOM
				echo "Dispatching ${dshort}${i}_${s} with seed $rand"
				sed "s/JOBID/${dshort}${i}_${s}/g" ${TEMPLATE} > temp.sl
				sed "s/RANDOMSEED/$rand/g" temp.sl > temp2.sl
				sed "s/DATASET/$d/g" temp2.sl > temp.sl
				sed "s/SETTING/$s/g" temp.sl > temp2.sl
				sed "s/PARTITION/${eshort}/g" temp2.sl > temp.sl
				
				
				
				if [ "$d" == "Ran_2018_dna" ] || [ "$d" == "Broughton_2013" ]  || [ "$d" == "Rightmyer_2013" ]; then
					
					echo "Big job"
					sed "s/TOTALTIME/60/g" temp.sl > temp2.sl
					sed "s/MEMORY/8000/g" temp2.sl > temp.sl
					
					
				else
				
					echo "Small job"
					sed "s/TOTALTIME/20/g" temp.sl > temp2.sl
					sed "s/MEMORY/4000/g" temp2.sl > temp.sl
					
				
				fi
				
				
				#mv temp2.sl temp.sl
				
				
				cp ../../../../../datasets/${d}/${e}/benchmark_${s}.xml tmp.xml
				sed 's/logEvery="2000" mode="tree"/logEvery="100000000000" mode="tree"/g' tmp.xml > tmp2.xml
				mv tmp2.xml tmp.xml
				
				


				
				#sbatch temp.sl
				sleep 2
				rm -f temp.sl 
				rm -f temp2.sl 
			
				cd ../

				#sbatch ~/beast/bin/beast -overwrite -beagle_SSE  > "output_${s}.out" >> "error_${s}.err"
			
			
			done
			
			cd ../

	  done



        cd ../
    done


    #sleep 1

    cd ../


    # Dummy job for ~5 min to give the other batches time to finish
    #~/beast/bin/beast  -overwrite -beagle_SSE ../datasets/startdummy.xml

done



# Dummy job to ensure that the other jobs are not benefitting from free CPUs
#~/beast/bin/beast  -overwrite -beagle_SSE ../datasets/endless_dummy.xml

