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

#### get unrelated samples for PC correction ####
unrel <- read_tsv('data/derivatives/unrelated_qc_samples_for_pca.tsv')
#### read in all PGS ####
## get all PGS files 
df <- data.frame(file = list.files("data/derivatives/pgs", full.names = T, pattern = "_PGS"))

## read them in, get residuals, scale, save to new file
pgs_list = list()
pgs_list_corrected = list()

registerDoMC(cores = 4)
for (f in df$file) {
  phenotype <- sub("\\.tsv", "", basename(f))
  ## progress
  message(which(f == df$file))
  ## read in PGS
  pgs_list[[basename(f)]] <- read_tsv(file = f, show_col_types = F) 
  tmp <- pgs_list[[basename(f)]] %>% rename(PGS=2)
  if (!any(is.na(tmp$PGS))) {
    ## merge PGS with genetic PCs
    tmp <- tmp %>%
      inner_join(pc)
    ## get unrelated samples to base the PC correction on
    tmp_unrel <- tmp %>%
      filter(IID %in% unrel$sample.ID)
    ## model PGS ~ PCs
    mod <- lm(PGS ~ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, 
              data = tmp_unrel %>% rename(PGS = 2))
    preds <- predict(mod, newdata = tmp)
    resids <- tmp$PGS - preds
    ## get mean and SD from the unrelated samples used in the PCA
    unrel_mean <- mean(resids[which(tmp$IID %in% tmp_unrel$IID)])
    unrel_sd <- sd(resids[which(tmp$IID %in% tmp_unrel$IID)])
    ## scale residuals to unrelated samples
    corrected_pgs <- (resids - unrel_mean) / unrel_sd  
    ## save results to list
    pgs_list_corrected[[basename(f)]] <- tmp %>%
      mutate(pgs_10pc_corrected = corrected_pgs,
             pgs_name = phenotype) %>%
      select(IID, pgs_name, pgs_raw = PGS, pgs_10pc_corrected)
  }
}
#### make long dataframes with all of the PGS merged ####
pgs <- bind_rows(pgs_list) %>%
  pivot_longer(cols = c(2:(length(pgs_list)+1)), names_to = "phenotype", values_to = "PGS") %>% drop_na(PGS)
pgs2 <- bind_rows(pgs_list_corrected) %>%
  pivot_longer(cols = c(2:(length(pgs_list)+1)), names_to = "phenotype", values_to = "PGS_corrected") %>% drop_na(PGS_corrected)

################################################################################
################################################################################
################################################################################
