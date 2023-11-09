#!/bin/bash

# Check if GLIMPSE script exists and is executable
#if [ ! -x genotype.sh ]; then
#    echo "genotype.sh not found or not executable"
#    exit 1
#fi

# Use GNU parallel to run up to 12 jobs in parallel
#cat /Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/samples.txt | parallel -j 12 ./scripts/genotype.sh {}

xargs -I {} ./scripts/genotype.sh {} < /Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/samples.txt

### for qsub:
while read -r SAMPLE; do
    qsub -q JM -pe smp 16 -V -cwd -j y -o ./log/ -N "job_${SAMPLE}" ./scripts/genotype.sh "${SAMPLE}"
done < /Dedicated/jmichaelson-wdata/jmichaelson/NDVR/genomics/pilot_96/samples_abbr.txt

### delete all jobs if necessary
#qdel $(qstat -u jmichaelson | awk '{print $1}')

