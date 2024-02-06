################################################################################
#                       tests reformatting for colnames and data               #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
# before running this script, make sure you downloaded the most updated xlsx file 
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/behavior"
setwd(project.dir)
################################################################################
# read ID mappings
ids <- readxl::read_xlsx("../language/data/raw/RPOE_participants_metadata.xlsx", sheet = 1)
################################################################################
################################################################################
# read cbcl data
cbcl.raw <- read_csv("../redcap/data/derivatives/cbcl.csv")
# get clean names for the questions to keep
col.clean <- read_csv("../redcap/data/derivatives/tmp/cbcl-cols.csv") %>%
  mutate(cons = ifelse(is.na(standard), cons, standard))
colnames(cbcl.raw) <- col.clean$cons
cbcl.clean <- cbcl.raw %>%
  select(devGenes_id, any_of(col.clean$standard[!is.na(col.clean$standard)])) %>%
  mutate_at(.vars = col.clean$standard[!is.na(col.clean$standard)], 
            .funs = function(x) replace_na(as.numeric(x), median(x, na.rm = T))) # replace missing data for each question by the edian value for that question
################################################################################
# get syndromes and scales
cbcl.scales <- read_rds("/wdata/msmuhammad/data/ABCD/cbcl-scales.rds") %>%
  mutate(q_clean = sub("q[0-9]+_", "", cbcl_item))
cbcl.clean.sum <- cbcl.clean %>%
  mutate(att_tot = rowSums(cbcl.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "att"]))),
         ext_tot = rowSums(cbcl.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "ext"]))),
         int_tot = rowSums(cbcl.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "int"]))),
         other_tot = rowSums(cbcl.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "other"]))),
         thought_tot = rowSums(cbcl.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "thought"]))),
         social_tot = rowSums(cbcl.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "social"]))),
         cbcl_tot = rowSums(cbcl.clean%>%select(any_of(cbcl.scales$q_clean))))
cbcl.clean.sum %>%
  pivot_longer(cols = ends_with("tot")) %>%
  ggplot(aes(x=value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free")
write_csv(cbcl.clean.sum, "data/derivatives/cbcl-clean-median-imputed.csv")
################################################################################
################################################################################
################################################################################
################################################################################
# read YSR data
ysr.raw <- read_csv("../redcap/data/derivatives/ysr.csv")
# get clean names for the questions to keep
col.clean.2 <- read_csv("../redcap/data/derivatives/tmp/ysr-cols.csv") %>%
  mutate(v = ifelse(is.na(standard), v, standard))
colnames(ysr.raw) <- col.clean.2$v
ysr.clean <- ysr.raw %>%
  select(devGenes_id, any_of(col.clean.2$standard[!is.na(col.clean.2$standard)])) %>%
  mutate_at(.vars = col.clean.2$standard[!is.na(col.clean.2$standard)], 
            .funs = function(x) replace_na(as.numeric(x), median(x, na.rm = T))) # replace missing data for each question by the edian value for that question
################################################################################
pos.ysr <- c("likes_animals", "honesty", "better_than_others", "friendly", "try_new_things",
             "works_with_hands", "stand_up_rights", "enjoys_being_with_people",
             "makes_others_laugh", "like_helping_others", "fair_to_others", "enjoys_jokes",
             "take_life_easy", "try_helping_others")
ysr.clean.sum <- ysr.clean %>%
  mutate(att_tot = rowSums(ysr.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "att"]))),
         ext_tot = rowSums(ysr.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "ext"]))),
         int_tot = rowSums(ysr.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "int"]))),
         other_tot = rowSums(ysr.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "other"]))),
         thought_tot = rowSums(ysr.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "thought"]))),
         social_tot = rowSums(ysr.clean%>%select(any_of(cbcl.scales$q_clean[cbcl.scales$scale == "social"]))),
         pos_tot = rowSums(ysr.clean%>%select(all_of(pos.ysr))),
         ysr_tot = rowSums(ysr.clean%>%select(any_of(cbcl.scales$q_clean))))
write_csv(ysr.clean.sum, "data/derivatives/ysr-clean-median-imputed.csv")
################################################################################


################################################################################


################################################################################


################################################################################
