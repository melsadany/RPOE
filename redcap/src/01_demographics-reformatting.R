################################################################################
#                       reshaping demographics data from RedCap                #
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
raw <- read_csv(list.files("data/raw/", pattern = "Demographicssummary", full.names = T))
# select fields of interest, just for the main data reporting person
# this means the parent, if they have children. Or the adult
demo.adult <- raw %>%
  select(devGenes_id = participant_id,
         child_count = how_many_children,
         dob = dob_v2,
         gender = gender_v2, gender_other = gender_other_v2,
         sex = sex_56f64a_v2, sex_other = sex_other_v2,
         parent_edu = parent_edu_v2,
         race = race_v2, race_other = race_other_v2,
         ethnicity = ethnicity_v2,
         marital_status = marital_status_v2) %>%
  drop_na(dob) %>%
  mutate(devGenes_id = as.character(devGenes_id))
################################################################################
# demo for children
# demo of child1
demo.child1 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_1_v2,
         gender = gender_child1_v2, gender_other = gender_other_child1_v2,
         sex = child1_sex_v2, sex_other = child1_sex_other_v2,
         race = child1_race_v2, race_other = child1_race_other_v2,
         ethnicity = child1_ethnicity_v2) %>%
  drop_na(dob) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_2"))
# demo of child2
demo.child2 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_2_v2,
         gender = gender_child2_v2, gender_other = gender_other_child2_v2,
         sex = child2_sex_v2, sex_other = child2_sex_other_v2,
         race = child2_race_v2, race_other = child2_race_other_v2,
         ethnicity = child2_ethnicity_v2) %>%
  drop_na(dob) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_3"))
# demo of child3
demo.child3 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_3_v2,
         gender = gender_child3_v2, gender_other = gender_other_child3_v2,
         sex = child3_sex_v2, sex_other = child3_sex_other_v2,
         race = child3_race_v2, race_other = child3_race_other_v2,
         ethnicity = child3_ethnicity_v2) %>%
  drop_na(dob) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_4"))
# demo of child4
demo.child4 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_4_v2,
         gender = gender_child4_v2, gender_other = gender_other_child4_v2,
         sex = child4_sex_v2, sex_other = child4_sex_other_v2,
         race = child4_race_v2, race_other = child4_race_other_v2,
         ethnicity = child4_ethnicity_v2) %>%
  drop_na(dob) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_5"))
# demo of child5
demo.child5 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_5_v2,
         gender = gender_child5_v2, gender_other = gender_other_child5_v2,
         sex = child5_sex_v2, sex_other = child5_sex_other_v2,
         race = child5_race_v2, race_other = child5_race_other_v2,
         ethnicity = child5_ethnicity_v2) %>%
  drop_na(dob) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_6"))
#######
# combine all children data
####### 
demo.child.all <- rbind(demo.child1, 
                        demo.child2,
                        demo.child3,
                        demo.child4,
                        demo.child5)
################################################################################
# combine parents/adults with children data
demo.all <- full_join(demo.adult,
                      demo.child.all)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# some participants didn't do demo2, but did demo1
# extract them here

demo1.ids <- c("4773")
demo.adult1 <- raw %>%
  select(devGenes_id = participant_id,
         child_count = how_many_children,
         dob = dob,
         gender = gender, gender_other = gender_other,
         sex = sex_56f64a, sex_other = sex_other,
         parent_edu = parent_edu,
         race = race, race_other = race_other,
         ethnicity = ethnicity,
         marital_status = marital_status) %>%
  drop_na(dob) %>%
  mutate(devGenes_id = as.character(devGenes_id)) %>%
  filter(devGenes_id %in% demo1.ids)
################################################################################
# demo for children
# demo of child1
demo.child11 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_1,
         gender = gender_child1, gender_other = gender_other_child1,
         sex = child1_sex, sex_other = child1_sex_other,
         race = child1_race, race_other = child1_race_other,
         ethnicity = child1_ethnicity) %>%
  drop_na(dob) %>%
  filter(devGenes_id %in% demo1.ids) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_2"))
# demo of child2
demo.child21 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_2,
         gender = gender_child2, gender_other = gender_other_child2,
         sex = child2_sex, sex_other = child2_sex_other,
         race = child2_race, race_other = child2_race_other,
         ethnicity = child2_ethnicity) %>%
  drop_na(dob)  %>%
  filter(devGenes_id %in% demo1.ids) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_3"))
# demo of child3
demo.child31 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_3,
         gender = gender_child3, gender_other = gender_other_child3,
         sex = child3_sex, sex_other = child3_sex_other,
         race = child3_race, race_other = child3_race_other,
         ethnicity = child3_ethnicity) %>%
  drop_na(dob)  %>%
  filter(devGenes_id %in% demo1.ids) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_4"))
# demo of child4
demo.child41 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_4,
         gender = gender_child4, gender_other = gender_other_child4,
         sex = child4_sex, sex_other = child4_sex_other,
         race = child4_race, race_other = child4_race_other,
         ethnicity = child4_ethnicity) %>%
  drop_na(dob)  %>%
  filter(devGenes_id %in% demo1.ids) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_5"))
# demo of child5
demo.child51 <- raw %>%
  select(devGenes_id = participant_id,
         dob = dob_child_5,
         gender = gender_child5, gender_other = gender_other_child5,
         sex = child5_sex, sex_other = child5_sex_other,
         race = child5_race, race_other = child5_race_other,
         ethnicity = child5_ethnicity) %>%
  drop_na(dob)  %>%
  filter(devGenes_id %in% demo1.ids) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_6"))
#######
# combine all children data
####### 
demo.child.all1 <- rbind(demo.child11, 
                        demo.child21,
                        demo.child31,
                        demo.child41,
                        demo.child51)
################################################################################
# combine parents/adults with children data from demographics1
demo.all1 <- full_join(demo.adult1,
                      demo.child.all1)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# combine data from demographics 1, and 2
demo.all.both <- full_join(demo.all,
                           demo.all1)
# save demographics
write_csv(demo.all.both, "data/derivatives/demographics.csv")
################################################################################


################################################################################


################################################################################