################################################################################
#                          reshaping KDOCS data from RedCap                    #
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
raw <- read_csv(list.files("data/raw/", pattern = "Kdocssummary", full.names = T))
################################################################################
################################################################################
# separate kdocs by column index
kdocs.p1 <- raw %>%
  select(1, 3:54) %>%
  drop_na(2)
kdocs.1 <- raw %>%
  select(1, 55:106) %>%
  drop_na(2)
kdocs.2 <- raw %>%
  select(1, 107:158) %>%
  drop_na(2)
kdocs.3 <- raw %>%
  select(1, 159:210) %>%
  drop_na(2)
kdocs.4 <- raw %>%
  select(1, 211:262) %>%
  drop_na(2)
kdocs.5 <- raw %>%
  select(1, 263:314) %>%
  drop_na(2)
################################################################################
# combine colnames, and save them for reviewing
tmp <- data.frame(p1 = colnames(kdocs.p1),
                  k1 = colnames(kdocs.1),
                  k2 = colnames(kdocs.2),
                  k3 = colnames(kdocs.3),
                  k4 = colnames(kdocs.4),
                  k5 = colnames(kdocs.5)) %>%
  mutate(con = k1)
# write_csv(tmp, "data/derivatives/tmp/kdocs-cols.csv")
# read reviewed colnames
tmp2 <- read_csv("data/derivatives/tmp/kdocs-cols.csv")
################################################################################
# rename colnames
colnames(kdocs.p1) <- tmp2$con
colnames(kdocs.1) <- tmp2$con
colnames(kdocs.2) <- tmp2$con
colnames(kdocs.3) <- tmp2$con
colnames(kdocs.4) <- tmp2$con
colnames(kdocs.5) <- tmp2$con
################################################################################
# drop missing data, and rename IDs
kdocs.1 <- kdocs.1 %>% mutate(devGenes_id = paste0(devGenes_id, "_2"))
kdocs.2 <- kdocs.2 %>% mutate(devGenes_id = paste0(devGenes_id, "_3"))
kdocs.3 <- kdocs.3 %>% mutate(devGenes_id = paste0(devGenes_id, "_4"))
kdocs.4 <- kdocs.4 %>% mutate(devGenes_id = paste0(devGenes_id, "_5"))
kdocs.5 <- kdocs.5 %>% mutate(devGenes_id = paste0(devGenes_id, "_6"))
################################################################################
# combine all, and save
kdocs.all <- rbind(kdocs.p1, kdocs.1, kdocs.2, kdocs.3, kdocs.4, kdocs.5)
write_csv(kdocs.all, "data/derivatives/kdocs.csv")
################################################################################




