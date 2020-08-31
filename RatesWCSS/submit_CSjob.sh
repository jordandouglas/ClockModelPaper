#!/bin/sh
TEMPLATE=../../CStemplate.sh



for sim in {1..100}
do
rand=$RANDOM
sed "s/SIM/${sim}/g" ${TEMPLATE} > temp.sl
sed "s/RANDOMSEED/$rand/g" temp.sl > temp2.sl 
echo "run${sim}.xml with seed ${rand}"
sbatch temp2.sl 
rm -f temp.sl
rm -f temp2.sl 
sleep 2
done


