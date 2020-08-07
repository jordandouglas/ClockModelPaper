#!/bin/sh


# Dummy job for ~5 min to give the other batche times to start
#~/beast/bin/beast  -overwrite -beagle_SSE ../datasets/startdummy.xml

#settings=("real" "real_adaptive" "cat" "quant" "AVMN" "bactrian98" "bactrian95" "NER" "real_06" "quant_06")
settings=("cat" "cat_adaptive" "real" "real_06" "real_adaptive" "quant_cached" "quant_cached_adaptive" "quant_06_cached" "bactrian95")

datasets=("Rightmyer_2013" "Ran_2018_dna" "Broughton_2013" "simulated" "Sauquet_2011" "Moyle_2016" "Dornburg_2012" "Kawahara_2013" "Cognato_2001")




# Randomly shuffle the datasets
#datasets_shuffled=( $(shuf -e "${datasets[@]}") )

for d in ${datasets[@]} ; do
    echo "$d"
    cd $d

    for e in */ ; do
        echo "$e"
        cd $e


          # Randomly shuffle the settings
          settings_shuffled=( $(shuf -e "${settings[@]}") )
          for s in "${settings_shuffled[@]}"
          do
              
             if [ ! -f "output_${s}.out" ]; then
                echo "Beginning ../../../datasets/$d/$e/benchmark_${s}.xml"
                echo "$1,$d,$s,$(date),start" >> ../../../progress.csv
                ~/beast/bin/beast -overwrite -beagle_SSE ../../../datasets/$d/$e/benchmark_${s}.xml > "output_${s}.out" >> "error_${s}.err"
                echo "$1,$d,$s,$(date),finish" >> ../../../progress.csv
             fi
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

