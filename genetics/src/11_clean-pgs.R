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
# predict population labels for my samples, so that the PGS correction is done separately for each population
# read the KINGS pca
kings.pc <- read_csv("data/derivatives/KING_pca_results.csv")
kg.king <- kings.pc %>%
  filter(population != "NDVR")
populations <- data.frame(population = unique(kg.king$population),
                          pop_number = c(1:26)) %>%
  mutate(pop_number = as.factor(pop_number))
kg.king <- kg.king %>%
  full_join(populations)
# train a random forest to predict pop, using pcs
library(randomForest)
rf2 <- randomForest(x = kg.king%>%select(starts_with("pc")),
                                 y = as.factor(kg.king$pop_number),
                    ntree = 500, random_state = 0, xtest = kings.pc %>%
                      filter(population == "NDVR") %>% select(starts_with("pc")))
predicted.pop <- cbind(rf2$test$predicted %>% as.data.frame(),
                       kings.pc %>%
                         filter(population == "NDVR") %>% select(IID = sample.ID)) %>%
  rename(pop_number = 1) %>%
  left_join(populations)
################################################################################
################################################################################
################################################################################
################################ script from Lucas #############################
################################################################################
################################################################################
#### PC data ####
pc.r <- read_csv(file = 'data/derivatives/KING_pca_results.csv',
               show_col_types = F) %>%
  rename(IID = sample.ID) %>%
  mutate(population = ifelse(population == "NDVR", NA, population))
predicted.pop2 <- left_join(predicted.pop, pc.r %>% select(-population))
pc <- full_join(pc %>% filter(population != "NDVR"), predicted.pop2)
## get all PGS files 
df <- data.frame(file = list.files("data/derivatives/pgs", full.names = T, pattern = "_PGS"))
registerDoMC(cores = 4)
all <- foreach(i=1:nrow(df), .combine = inner_join) %dopar% {
  return(read_tsv(df$file[i]))
}
################################################################################
################################################################################
################################################################################
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
    # loop per popula 
    corr <- foreach(k = unique(tmp$population), .combine = rbind) %dopar% {
      tt <- tmp_unrel %>% filter(population==k)
      ttmp <- tmp %>% filter(population == k)
      ## model PGS ~ PCs
      mod <- lm(PGS ~ pc1 + pc2 + pc3 + pc4 + pc5 + pc6 + pc7 + pc8 + pc9 + pc10, 
                data = tt %>% rename(PGS = 2))
      preds <- predict(mod, newdata = ttmp)
      resids <- ttmp$PGS - preds
      ## get mean and SD from the unrelated samples used in the PCA
      unrel_mean <- mean(resids[which(ttmp$IID %in% tt$IID)])
      unrel_sd <- sd(resids[which(ttmp$IID %in% tt$IID)])
      ## scale residuals to unrelated samples
      corrected_pgs <- (resids - unrel_mean) / unrel_sd  
      return(ttmp[,1:3] %>% mutate(pgs_10pc_corrected = corrected_pgs))
    }
    ## save results to list
    pgs_list_corrected[[basename(f)]] <- corr %>%
      mutate(pgs_name = phenotype) %>%
      select(IID, pgs_name, pgs_raw = PGS, pgs_10pc_corrected)
  }
}
#### make long dataframes with all of the PGS merged ####
pgs <- bind_rows(pgs_list) %>%
  pivot_longer(cols = c(2:(length(pgs_list)+1)), names_to = "phenotype", values_to = "PGS") %>% drop_na(PGS)
pgs2 <- do.call(rbind, pgs_list_corrected) %>%
  rename(PGS_corrected=4)
################################################################################
################################################################################
# IDs remapping for the first 9 sequenced samples
mapping <- readxl::read_xlsx("data/raw/last-3-plates.xlsx", sheet = 4) %>%
  mutate(seqname = paste0("S", c(1:96), "_recal"))
# save combined corrected and raw PGS after adding the remapping
pgs.remapped <- left_join(pgs2, 
                          mapping %>% 
                            select(2:6, IID = seqname))
write_rds(pgs.remapped, "data/derivatives/combined-pgs-long-format-corrected-per-population.rds", compress = "gz")
################################################################################
################################################################################
# check PGS distribution?
pgs.remapped <- read_rds("data/derivatives/combined-pgs-long-format-corrected-per-population.rds")
pgs.remapped %>% 
  filter(plate == "P3") %>%
  ggplot(aes(x=PGS_corrected, fill=plate))+
  geom_histogram()+
  facet_wrap(~pgs_name, scales = "free")
ggsave("figs/dist-pgs-corr2-P3.png", bg = "white",
       width = 18, height = 10, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
