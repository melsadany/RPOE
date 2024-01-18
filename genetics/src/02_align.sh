#! /bin/bash

PROJECT_DIR=/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/genetics
cd ${PROJECT_DIR}

# make sure you have a file that has 3 columns tab-delimeted file with SAMPLE_ID, FASTQ1, and FASTQ2
# see the 00_build-metadata-file.R script to build the metadata file needed
# change the task ID to be sample row/line number in that file
# see example in data/sampleids
while IFS=$'\t' read -r SAMPLE_ID FASTQ1 FASTQ2; do
    echo "Sample is: ${SAMPLE_ID}"
    echo "FASTQ1 is: ${FASTQ1}"
    echo "FASTQ2 is: ${FASTQ2}"
    qsub -cwd -q JM,UI,CCOM -pe smp 8 -N align_${SAMPLE_ID} -o logs/align_${SAMPLE_ID}.log -j y -ckpt user ${PROJECT_DIR}/src/01_align.sh "${SAMPLE_ID}" "${FASTQ1}" "${FASTQ2}"
done < data/sampleids



