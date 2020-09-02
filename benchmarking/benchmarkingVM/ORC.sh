#!/bin/sh




#settings=("real" "real_adaptive" "cat" "quant" "AVMN" "bactrian98" "bactrian95" "NER" "real_06" "quant_06")
settings=("cat" "cat_adaptive" "real" "real_06" "real_adaptive" "quant_cached" "quant_cached_adaptive" "quant_06_cached" "bactrian95" "NER" "AVMVN")

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


		rm -f temp*.xml

          # Randomly shuffle the settings
          settings_shuffled=( $(shuf -e "${settings[@]}") )
          #for s in "${settings_shuffled[@]}"
         # do
		  
			
		
			
			#xml="benchmark_${s}.xml"
			#echo "Modifying ${xml}"
			#sed "s/inference.TipRateLogger/orc.inference.TipRateLogger/g" ${xml} > temp.xml
			#sed "s/NERVariants/orc.ner/g" temp.xml > temp2.xml
			#sed "s/AdaptableOperatorSampler/orc.operators.AdaptableOperatorSampler/g" temp2.xml > temp.xml
			#sed "s/SampleFromPriorOperator/orc.operators.SampleFromPriorOperator/g" temp.xml > temp2.xml
			
			#rm temp.xml

			#mv temp2.xml ${xml}


		#done

        cd ../
    done


    cd ../


done




