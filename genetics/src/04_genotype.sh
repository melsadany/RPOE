#!/bin/bash

PROJECT_DIR=/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/genetics
cd ${PROJECT_DIR}


# xargs -I {} ./scripts/genotype.sh {} < /Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/samples.txt

### for qsub:
while IFS=$'\t' read -r SAMPLE_ID FASTQ1 FASTQ2; do
    echo "Sample is: ${SAMPLE_ID}"
    qsub -cwd -q JM,UI,CCOM -pe smp 16 -N genotype_${SAMPLE_ID} -o ${PROJECT_DIR}/logs/genotype_${SAMPLE_ID}.log -j y -ckpt user ${PROJECT_DIR}/src/03_genotype.sh "${SAMPLE_ID}" 
    # qsub -q JM -pe smp 16 -V -cwd -j y -o ./log/ -N "job_${SAMPLE}" ./scripts/genotype.sh "${SAMPLE}"
done < data/sampleids

