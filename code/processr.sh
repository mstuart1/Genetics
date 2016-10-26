#!/bin/bash
process_radtags -b ../logs/barcodes_48 -c -q --renz_1 pstI --renz_2 mluCI \
-i gzfastq --adapter_1 ACACTCTTTCCCTACACGACGCTCTTCCGATCT \
-f ./P069.fastq.gz -o ./

mv process_radtags.log ../logs/69process.log
