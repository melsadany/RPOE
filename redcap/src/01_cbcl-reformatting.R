################################################################################
#                           reshaping CBCL data from RedCap                    #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/redcap"
setwd(project.dir)
################################################################################
# read raw file
raw <- read_csv(list.files("data/raw/", pattern = "Cbclsummary", full.names = T)) %>%
  drop_na(cbclasr_timestamp)
################################################################################
################################################################################
# separate cbcl by column index
cbcl.1 <- raw %>%
  select(1,3:170)
cbcl.2 <- raw %>%
  select(1,171:338)
cbcl.3 <- raw %>%
  select(1,339:506)
cbcl.4 <- raw %>%
  select(1,507:674)
cbcl.5 <- raw %>%
  select(1,675:842)

################################################################################
# make a dataframe to match colnames
tmp <- data.frame(cbcl1 = colnames(cbcl.1),
                  cbcl2 = colnames(cbcl.2),
                  cbcl3 = colnames(cbcl.3),
                  cbcl4 = colnames(cbcl.4),
                  cbcl5 = colnames(cbcl.5)) %>%
  mutate(cons = cbcl1)
# write_csv(tmp, "data/derivatives/tmp/cbcl-cols.csv")
# in that file, rename the cons to a matching colname for all cbcls
tmp2 <- read_csv("data/derivatives/tmp/cbcl-cols.csv")
################################################################################
colnames(cbcl.1) <- tmp2$cons
colnames(cbcl.2) <- tmp2$cons
colnames(cbcl.3) <- tmp2$cons
colnames(cbcl.4) <- tmp2$cons
colnames(cbcl.5) <- tmp2$cons
################################################################################
# drop missing cbcls, and edit devGenes_id to match
cbcl.1 <- cbcl.1 %>% 
  drop_na(timestamp) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_2"))
cbcl.2 <- cbcl.2 %>% 
  drop_na(timestamp) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_3"))
cbcl.3 <- cbcl.3 %>% 
  drop_na(timestamp) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_4"))
cbcl.4 <- cbcl.4 %>% 
  drop_na(timestamp) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_5"))
cbcl.5 <- cbcl.5 %>% 
  drop_na(timestamp) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_6"))
################################################################################
# combine, and save
cbcl.all <- rbind(cbcl.1, cbcl.2, cbcl.3, cbcl.4, cbcl.5)
write_csv(cbcl.all, "data/derivatives/cbcl.csv")
################################################################################
