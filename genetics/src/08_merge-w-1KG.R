################################################################################
#                            merge with 1KG, and save                          #
################################################################################
rm(list = ls())
gc()
library(tidyverse)
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/genetics"
setwd(project.dir)
################################################################################
## get intersection between 1KG, and my cohort
kg <- read_table('/Dedicated/jmichaelson-wdata/msmuhammad/data/genomics/1KG/ALL.chr.plink.hapmap3plus.bim',
                 col_names = F)

my.co <- read_delim('data/derivatives/merged-glimpse/all-samples/in-hg19/samples-1-96_filtered_hg19.bim', 
                    col_names = F, delim = '\t')
# the chr names in my bim file have the "chr", we don't want that since it's not in 1KG naming
my.co.2 <- my.co %>%
  mutate(X2 = sub("chr", "", X2))
my.co.2 %>%
  write_delim('data/derivatives/merged-glimpse/all-samples/in-hg19/samples-1-96_filtered_hg19.bim', 
              col_names = F, delim = '\t')
rm(my.co);gc()
##### 
int <- data.frame(SNP = intersect(kg$X2, my.co.2$X2))
length(int$SNP)
int %>%
  write_tsv('data/derivatives/merged-glimpse/all-samples/in-hg19/1KG_hapmap3_plus_overlap.txt', 
            col_names = F)
#####
# extract from 1KG first
cmd <- paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/plink/plink",
             "--bfile", "/Dedicated/jmichaelson-wdata/msmuhammad/data/genomics/1KG/ALL.chr.plink.hapmap3plus",
             "--extract", "data/derivatives/merged-glimpse/all-samples/in-hg19/1KG_hapmap3_plus_overlap.txt", 
             "--make-bed",
             "--threads", 45,
             "--out", "data/derivatives/merged-glimpse/all-samples/in-hg19/1KG_hapmap-intersected-w-my-co", 
             sep = " ")
#####
# extract from my samples
cmd <- paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/plink/plink",
             "--bfile", "data/derivatives/merged-glimpse/all-samples/in-hg19/final",
             "--extract", "data/derivatives/merged-glimpse/all-samples/in-hg19/1KG_hapmap3_plus_overlap.txt", 
             "--make-bed",
             "--threads", 45,
             "--out", "data/derivatives/merged-glimpse/all-samples/in-hg19/final_hapmap-intersected-w-1KG", 
             sep = " ")


#### merge data and subset
cmd <- paste("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/plink/plink",
             "--bfile", "data/derivatives/merged-glimpse/all-samples/in-hg19/final_hapmap-intersected-w-1KG",
             "--bmerge", "data/derivatives/merged-glimpse/all-samples/in-hg19/1KG_hapmap-intersected-w-my-co",
             "--make-bed",
             "--threads", 45,
             "--out", "data/derivatives/merged-glimpse/all-samples/in-hg19/final-merged-w-1KG_hapmap", 
             sep = " ")
system(cmd)
##