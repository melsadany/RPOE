################################################################################
#                    create a tab-delim metadata file for samples              #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/genetics"
setwd(project.dir)
################################################################################
# # I copied all data from the sequencer to my /projects/RPOE/genetics/data/raw
# system(paste0('for dir in /Dedicated/jmichaelson-sdata/NDVR/genomics/pilot_96/Project_Michaelson_23119/Sample_*/; do cp -r "$dir" ',
#               project.dir, '/data/raw/ ; done'))
################################################################################
################################################################################
metadata <- data.frame(f = list.files("/Dedicated/jmichaelson-sdata/NDVR/genomics/2023_aug_21/fq/")) 
metadata$SampleNum <- sapply(strsplit(metadata$f, split = "_"), function(x) x[2])
metadata$LaneNum <- sapply(strsplit(metadata$f, split = "_"), function(x) x[3])
metadata$date <- sapply(strsplit(metadata$f, split = "_"), function(x) x[4])
metadata$SAMPLE_ID <- sapply(strsplit(metadata$f, split = "_"), function(x) x[5])
metadata$LaneID <- sapply(strsplit(metadata$f, split = "_"), function(x) x[6])
metadata$Run <- sapply(strsplit(metadata$f, split = "_"), function(x) x[7])

meta2 <- metadata %>%
  mutate(fastq = ifelse(Run=="R1", "FASTQ1", "FASTQ2"),
         f = paste0("/Dedicated/jmichaelson-sdata/NDVR/genomics/2023_aug_21/fq/", f)) %>%
  pivot_wider(id_cols = SAMPLE_ID, names_from = fastq, values_from = f)
write_delim(meta2, "data/sampleids", delim = "\t", col_names = F)
################################################################################


################################################################################


################################################################################


################################################################################