################################################################################
#                         combine PGS and reformat                            #
################################################################################
rm(list = ls()); gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/genetics"
setwd(project.dir)
################################################################################
################################################################################
################################################################################
#### PC data ####
pc <- read_csv(file = 'data/derivatives/KING_pca_results.csv',
               show_col_types = F) %>%
  rename(IID = sample.ID)
## get all PGS files 
df <- data.frame(file = list.files("data/derivatives/pgs", full.names = T, pattern = "_PGS"))
registerDoMC(cores = 4)
all <- foreach(i=1:nrow(df), .combine = inner_join) %dopar% {
  return(read_tsv(df$file[i]))
}
################################################################################
################################################################################
################################################################################
