#!/bin/bash
process_radtags -b ./logs/barcodes -c -q --renz_1 pstI --renz_2 mluCI \
-i gzfastq --adapter_1 ACACTCTTTCCCTACACGACGCTCTTCCGATCT \
-f ./Pool1/P065.fastq.gz -o ./Pool1/

mv process_radtags.log ../logs/65process.log
