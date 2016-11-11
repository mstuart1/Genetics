#!/bin/bash

# get reference file
cp ~/02-apcl-ddocent/jonsfiles/reference.fasta ./

# make namelist so that a popmap can be created
ls *.F.fq.gz > namelist
sed -i'' -e 's/.F.fq.gz//g' namelist

# make popmap
cut -f1 -d "_" namelist > p
paste namelist p > popmap
rm p

# Create list of BAM files
ls *-RG.bam >bamlist.list

# make cat-RRG.bam and mapped.bed files - this version uses all bams, no dups removed
bamtools merge -list bamlist.list -out cat-RRG.bam &>/dev/null
samtools index cat-RRG.bam 
wait
bamToBed -i cat-RRG.bam > map.bed
bedtools merge -i map.bed > mapped.bed

# split the mapped.bed out into 1000 intervals
DP=$(mawk '{print $4}' cov.stats | sort -rn | perl -e '$d=.005;@l=<>;print $l[int($d*@l)]')

CC=$( mawk -v x=$DP '$4 < x' cov.stats | mawk '{len=$3-$2;lc=len*$4;tl=tl+lc} END {OFMT = "%.0f";print tl/1000}')

mawk -v x=$DP '$4 < x' cov.stats |sort -V -k1,1 -k2,2 | mawk -v cutoff=$CC 'BEGIN{i=1}
        {
        len=$3-$2;lc=len*$4;cov = cov + lc
        if ( cov < cutoff) {x="mapped."i".bed";print $1"\t"$2"\t"$3 > x}
        else {i=i+1; x="mapped."i".bed"; print $1"\t"$2"\t"$3 > x; cov=0}
        }'

# run freebayes using the cat-RRG.bam to save memory
ls mapped.*.bed | sed 's/mapped.//g' | sed 's/.bed//g' | shuf | parallel --memfree 50G --no-notice \
--delay 1 freebayes -b cat-RRG.bam -t mapped.{}.bed -v raw.{}.vcf -f reference.fasta \
-m 5 -q 5 -E 3 --min-repeat-entropy 1 -V --populations popmap -n 4 -j 15 &

