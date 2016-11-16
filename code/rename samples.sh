#!/bin/bash

if [ -z "$1" ] 
then 
echo "No file with barcodes and sample names specified." 
echo "Correct usage: Rename_for_dDocent.sh barcodefile" 
exit 1 
else 
NAMES=( `cut -c 11-15 $1 `) 
BARCODES=( `cut -f1 $1 `) 
LEN=( `wc -l $1 `) 
LEN=$(($LEN - 1)) 

echo ${NAMES[0]} 
echo ${BARCODES[0]} 

for ((i = 0; i <= $LEN; i++)); 
do 
mv ${BARCODES[$i]}.F.fq.gz ${NAMES[$i]}.F.fq.gz  
mv ${BARCODES[$i]}.R1.fq.gz ${NAMES[$i]}.R1.fq.gz  
mv ${BARCODES[$i]}-RG.bam ${NAMES[$i]}-RG.bam  
mv ${BARCODES[$i]}-RG.bam.bai ${NAMES[$i]}-RG.bam.bai  
done
fi
