

CHROMOSOMES=(chr1 chr2 chr3 chr4 chr5 chr6 chr7 chr8 chr9 chr10 chr11 chr12 chr13 chr14 chr15 chr16 chr17 chr18 chr19 chr20 chr21 chr22 chrX chrY)

### extract sites
for CHR in "${CHROMOSOMES[@]}"; do
 bcftools view -G -Oz -o /sdata/lowpass/ref_panel/1KG/${CHR}.sites.vcf.gz /sdata/lowpass/ref_panel/1KG/ALL.${CHR}.shapeit2_integrated_v1a.GRCh38.20181129.phased.vcf.gz
 bcftools index -f /sdata/lowpass/ref_panel/1KG/${CHR}.sites.vcf.gz
done

 ### split reference panel into chunks and make the binary refs
for CHR in "${CHROMOSOMES[@]}"; do
 ### split reference panel into chunks
 /sdata/lowpass/bin/GLIMPSE2_chunk --threads 8 --sequential --input /sdata/lowpass/ref_panel/1KG/${CHR}.sites.vcf.gz --region $CHR --output /sdata/lowpass/chunks/chunks.${CHR}.txt --map maps/genetic_maps.b38/${CHR}.b38.gmap.gz


 REF=/sdata/lowpass/ref_panel/1KG/ALL.${CHR}.shapeit2_integrated_v1a.GRCh38.20181129.phased.vcf.gz
 MAP=maps/genetic_maps.b38/${CHR}.b38.gmap.gz
 while IFS="" read -r LINE || [ -n "$LINE" ];
 do
   printf -v ID "%02d" $(echo $LINE | cut -d" " -f1)
   IRG=$(echo $LINE | cut -d" " -f3)
   ORG=$(echo $LINE | cut -d" " -f4)

   ./bin/GLIMPSE2_split_reference --threads 8 --reference ${REF} --map ${MAP} --input-region ${IRG} --output-region ${ORG} --output ref_panel/1KG/split/1000GP.${CHR}.hg38
 done < chunks/chunks.${CHR}.txt
done

CHR=chr22

CHROMOSOMES=(chr1 chr2 chr3 chr4 chr5 chr6 chr7 chr8 chr9 chr10 chr11 chr12 chr13 chr14 chr15 chr16 chr17 chr18 chr19 chr20 chr21 chr22 chrX chrY)
SAMPLE=Sample_0001_H-NDVR
REF=ref_panel/1KG/split/1000GP.${CHR}.hg38
BAM=/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/${SAMPLE}_recal.bam
mkdir -p /Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}
while IFS="" read -r LINE || [ -n "$LINE" ]; 
do   
	printf -v ID "%02d" $(echo $LINE | cut -d" " -f1)
	IRG=$(echo $LINE | cut -d" " -f3)
	ORG=$(echo $LINE | cut -d" " -f4)
	CHR=$(echo ${LINE} | cut -d" " -f2)
	REGS=$(echo ${IRG} | cut -d":" -f 2 | cut -d"-" -f1)
	REGE=$(echo ${IRG} | cut -d":" -f 2 | cut -d"-" -f2)
	OUT=/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}/${SAMPLE}_imputed
	./bin/GLIMPSE2_phase --bam-file ${BAM} --reference ${REF}_${CHR}_${REGS}_${REGE}.bin --output ${OUT}_${CHR}_${REGS}_${REGE}.bcf
done < chunks/chunks.${CHR}.txt

LST=/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}/list.${CHR}.txt
ls -1v /Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}/${SAMPLE}_imputed_${CHR}_*.bcf > ${LST}

OUT=/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/glimpse/${SAMPLE}/${SAMPLE}_${CHR}_ligated.bcf
./bin/GLIMPSE2_ligate --input ${LST} --output $OUT




#########################################################################
### parallelize the reference-building
#########################################################################

#!/bin/bash

# function to process each chromosome
process_chr() {
  CHR=$1

  ### extract sites
  bcftools view -G -Oz -o /sdata/lowpass/ref_panel/1KG/${CHR}.sites.vcf.gz /sdata/lowpass/ref_panel/1KG/ALL.${CHR}.shapeit2_integrated_v1a.GRCh38.20181129.phased.vcf.gz
  bcftools index -f /sdata/lowpass/ref_panel/1KG/${CHR}.sites.vcf.gz

  ### split reference panel into chunks
  /sdata/lowpass/bin/GLIMPSE2_chunk --sequential --input /sdata/lowpass/ref_panel/1KG/${CHR}.sites.vcf.gz --region $CHR --output /sdata/lowpass/chunks/chunks.${CHR}.txt --map maps/genetic_maps.b38/${CHR}.b38.gmap.gz

  REF=/sdata/lowpass/ref_panel/1KG/ALL.${CHR}.shapeit2_integrated_v1a.GRCh38.20181129.phased.vcf.gz
  MAP=maps/genetic_maps.b38/${CHR}.b38.gmap.gz

  while IFS="" read -r LINE || [ -n "$LINE" ];
  do
    printf -v ID "%02d" $(echo $LINE | cut -d" " -f1)
    IRG=$(echo $LINE | cut -d" " -f3)
    ORG=$(echo $LINE | cut -d" " -f4)

    ./bin/GLIMPSE2_split_reference --reference ${REF} --map ${MAP} --input-region ${IRG} --output-region ${ORG} --output ref_panel/1KG/split/1000GP.${CHR}.hg38
  done < chunks/chunks.${CHR}.txt
}

export -f process_chr  # export the function so it's available to xargs

# chromosomes to process
CHROMOSOMES=("chr1" "chr2" "chr3" "chr4" "chr5" "chr6" "chr7" "chr8" "chr9" "chr10" "chr11" "chr12" "chr13" "chr14" "chr15" "chr16" "chr17" "chr18" "chr19" "chr20" "chr21" "chr22" "chrX" "chrY")

printf "%s\n" "${CHROMOSOMES[@]}" | xargs -n 1 -P 8 -I % bash -c 'process_chr "%"'


