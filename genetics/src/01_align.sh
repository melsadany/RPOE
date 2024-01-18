#!/bin/bash

### submit the 02_align.sh file on argon by simply running the sh script

set -e 
# Define paths to required resources
BWA_BIN="/Dedicated/jmichaelson-wdata/bcbio-dna/bin/bwa"
GATK_BIN="/Dedicated/jmichaelson-wdata/bcbio-dna/bin/gatk"
SAMTOOLS_BIN="/Dedicated/jmichaelson-wdata/bcbio-dna/bin/samtools"
BWA_INDEX="/Dedicated/jmichaelson-wdata/bcbio-dna/genomes/Hsapiens/hg38/bwa/hg38.fa"
REFERENCE_GENOME="/Dedicated/jmichaelson-wdata/bcbio-dna/genomes/Hsapiens/hg38/seq/hg38.fa"

# I couldn't manage to get PICARD to work, so I installed it on my EGA conda environment
PICARD="/Dedicated/jmichaelson-wdata/msmuhammad/workbench/genomics/jar/picard.jar"
conda activate EGA

DBSNP="/Dedicated/jmichaelson-wdata/bcbio-dna/genomes/Hsapiens/hg38/variation/hg38_v0_Homo_sapiens_assembly38.dbsnp138.vcf.gz"
MILLS_INDELS="/Dedicated/jmichaelson-wdata/bcbio-dna/genomes/Hsapiens/hg38/variation/hg38_v0_Homo_sapiens_assembly38.known_indels.vcf.gz"

#
P_DIR="/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/genetics"

# Check if required number of arguments are passed
if [[ $# -ne 3 ]] && [[ $# -ne 5 ]]; then
    echo "Usage for one lane: $0 <SampleID> <FASTQ1> <FASTQ2>"
    echo "Usage for two lanes: $0 <SampleID> <FASTQ1_lane1> <FASTQ2_lane1> <FASTQ1_lane2> <FASTQ2_lane2>"
    exit 1
fi

# Assign command-line arguments to variables
SAMPLE_ID=$1
FASTQ1_lane1=$2
FASTQ2_lane1=$3

# Step 1: Run BWA MEM on each pair of FASTQ files
(${BWA_BIN} mem -K 100000000 -v 3 -t 8 -Y -M -R "@RG\tID:${SAMPLE_ID}_lane1\tSM:${SAMPLE_ID}\tPL:ILLUMINA\tLB:lib1" $BWA_INDEX $FASTQ1_lane1 $FASTQ2_lane1 > ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1.sam) &
PID1=$!

if [[ $# -eq 5 ]]; then
    FASTQ1_lane2=$4
    FASTQ2_lane2=$5
    (${BWA_BIN} mem -K 100000000 -v 3 -t 8 -Y -M -R "@RG\tID:${SAMPLE_ID}_lane2\tSM:${SAMPLE_ID}\tPL:ILLUMINA\tLB:lib2" $BWA_INDEX $FASTQ1_lane2 $FASTQ2_lane2 > ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2.sam) &
    PID2=$!
    wait $PID1 $PID2
else
    wait $PID1
fi

# Step 2: Sort SAM files to BAM files
${SAMTOOLS_BIN} sort -@ 2 -O BAM -o ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1_sorted.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1.sam &
PID3=$!

if [[ $# -eq 5 ]]; then
    ${SAMTOOLS_BIN} sort -@ 2 -O BAM -o ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2_sorted.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2.sam &
    PID4=$!
    wait $PID3 $PID4
else
    wait $PID3
fi

# Step 3: Merge the BAM files (if two lanes)
if [[ $# -eq 5 ]]; then
    ${SAMTOOLS_BIN} merge ${P_DIR}/data/derivatives/${SAMPLE_ID}_sorted.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1_sorted.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2_sorted.bam
else
    mv ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1_sorted.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_sorted.bam
fi

### the below code will mark duplicates and perform BQSR using GATK
### Step 6: Mark duplicates
#if using the picard.jar
#java -jar $PICARD MarkDuplicates I=${SAMPLE_ID}_sorted_set.bam O=${SAMPLE_ID}_sorted_set_marked.bam M=${SAMPLE_ID}_marked_dup_metrics.txt
#if using the conda picard
picard MarkDuplicates I=${P_DIR}/data/derivatives/${SAMPLE_ID}_sorted.bam O=${P_DIR}/data/derivatives/${SAMPLE_ID}_sorted_set_marked.bam M=${P_DIR}/data/derivatives/${SAMPLE_ID}_marked_dup_metrics.txt

$GATK_BIN MarkDuplicates --INPUT ${P_DIR}/data/derivatives/${SAMPLE_ID}_sorted.bam --OUTPUT ${P_DIR}/data/derivatives/${SAMPLE_ID}_sorted_set_marked.bam --METRICS_FILE ${P_DIR}/data/derivatives/${SAMPLE_ID}_marked_dup_metrics.txt

### Step 7: Base recalibration
$GATK_BIN BaseRecalibrator --input ${P_DIR}/data/derivatives/${SAMPLE_ID}_sorted_set_marked.bam --reference $REFERENCE_GENOME --known-sites $DBSNP --known-sites $MILLS_INDELS --output ${P_DIR}/data/derivatives/${SAMPLE_ID}_recal_data.table

$GATK_BIN ApplyBQSR --reference $REFERENCE_GENOME --input ${P_DIR}/data/derivatives/${SAMPLE_ID}_sorted_set_marked.bam --bqsr-recal-file ${P_DIR}/data/derivatives/${SAMPLE_ID}_recal_data.table --output ${P_DIR}/data/derivatives/${SAMPLE_ID}_recal.bam

## Clean up intermediate files
rm ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1.sam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2.sam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1_sorted.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2_sorted.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1_sorted.bam.bai ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2_sorted.bam.bai 
if [[ $# -eq 5 ]]; then
    rm ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2.sam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane1_sorted.bam ${P_DIR}/data/derivatives/${SAMPLE_ID}_lane2_sorted.bam
fi

echo "Pipeline finished successfully!"

