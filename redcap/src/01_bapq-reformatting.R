################################################################################
#                           reshaping BAPQ data from RedCap                    #
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
raw <- read_csv(list.files("data/raw/", pattern = "Bapqsummary", full.names = T))
################################################################################
################################################################################
# separate bapq by column index
bapq.p1 <- raw %>%
  select(1,3:40) %>%
  drop_na(2)
bapq.1 <- raw %>%
  select(1,41:78) %>%
  drop_na(2)
bapq.2 <- raw %>%
  select(1,79:116) %>%
  drop_na(2)
bapq.3 <- raw %>%
  select(1,117:154) %>%
  drop_na(2)
bapq.4 <- raw %>%
  select(1,155:192) %>%
  drop_na(2)
bapq.5 <- raw %>%
  select(1,193:230) %>%
  drop_na(2)
################################################################################
# combine colnames, save, and review
tmp <- data.frame(p1 = colnames(bapq.p1),
                  b1 = colnames(bapq.1),
                  b2 = colnames(bapq.2),
                  b3 = colnames(bapq.3),
                  b4 = colnames(bapq.4),
                  b5 = colnames(bapq.5)) %>%
  mutate(con = b1)
# write_csv(tmp, "data/derivatives/tmp/bapq-cols.csv")
# read the reviewed colnames
tmp2 <- read_csv("data/derivatives/tmp/bapq-cols.csv")
################################################################################
# rename columns
colnames(bapq.p1) <- tmp2$con
colnames(bapq.1) <- tmp2$con
colnames(bapq.2) <- tmp2$con
colnames(bapq.3) <- tmp2$con
colnames(bapq.4) <- tmp2$con
colnames(bapq.5) <- tmp2$con
################################################################################
# rename IDs
bapq.1 <- bapq.1 %>% mutate(devGenes_id = paste0(devGenes_id, "_2"))
bapq.2 <- bapq.2 %>% mutate(devGenes_id = paste0(devGenes_id, "_3"))
bapq.3 <- bapq.3 %>% mutate(devGenes_id = paste0(devGenes_id, "_4"))
bapq.4 <- bapq.4 %>% mutate(devGenes_id = paste0(devGenes_id, "_5"))
bapq.5 <- bapq.5 %>% mutate(devGenes_id = paste0(devGenes_id, "_6"))
################################################################################
# combine all, and save
bapq.all <- rbind(bapq.p1, bapq.1, bapq.2, bapq.3, bapq.4, bapq.5)
write_csv(bapq.all, "data/derivatives/bapq.csv")
################################################################################
################################################################################
