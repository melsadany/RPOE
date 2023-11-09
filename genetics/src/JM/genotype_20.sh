#!/bin/bash

# Check if sample name was provided
if [ "$#" -ne 1 ]; then
    echo "Usage: $0 sample_name"
    exit 1
fi

SAMPLE=$1
MAX_JOBS=20
CHROMOSOMES=(chr1 chr2 chr3 chr4 chr5 chr6 chr7 chr8 chr9 chr10 chr11 chr12 chr13 chr14 chr15 chr16 chr17 chr18 chr19 chr20 chr21 chr22 chrX)

for CHR in "${CHROMOSOMES[@]}"
do
	REF=/Dedicated/jmichaelson-sdata/lowpass/ref_panel/1KG/split/1000GP.${CHR}.hg38
	BAM=/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/${SAMPLE}_recal.bam
	# Check if BAM file exists
	if [ ! -f "$BAM" ]; then
		echo "Error: BAM file $BAM not found for sample $SAMPLE"
		exit 1
	fi
	mkdir -p /Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}

	while IFS="" read -r LINE || [ -n "$LINE" ]
	do   
		printf -v ID "%02d" $(echo $LINE | cut -d" " -f1)
		IRG=$(echo $LINE | cut -d" " -f3)
		ORG=$(echo $LINE | cut -d" " -f4)
		REGS=$(echo ${IRG} | cut -d":" -f 2 | cut -d"-" -f1)
		REGE=$(echo ${IRG} | cut -d":" -f 2 | cut -d"-" -f2)
		OUT=/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}/${SAMPLE}_imputed
		/Dedicated/jmichaelson-sdata/lowpass/bin/GLIMPSE2_phase --bam-file ${BAM} --reference ${REF}_${CHR}_${REGS}_${REGE}.bin --output ${OUT}_${CHR}_${REGS}_${REGE}.bcf &
		# If the number of background jobs is >= 4, wait for any job to complete
        	while (( $(jobs -p | wc -l) >= MAX_JOBS )); do
           	 sleep 1
        	done
	done < /Dedicated/jmichaelson-sdata/lowpass/chunks/chunks.${CHR}.txt
	wait
	LST=/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}/list.${CHR}.txt
	ls -1v /Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}/${SAMPLE}_imputed_${CHR}_*.bcf > ${LST}

	OUT=/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}/${SAMPLE}_${CHR}_ligated.bcf
	/Dedicated/jmichaelson-sdata/lowpass/bin/GLIMPSE2_ligate --input ${LST} --output $OUT
done


