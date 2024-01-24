################################################################################
#                            reshaping YSR data from RedCap                    #
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
raw <- read_csv(list.files("data/raw/", pattern = "Ysrsummary", full.names = T)) %>%
  drop_na(ysr_timestamp)
################################################################################
################################################################################
# ysr is a nightmare in terms of question names, and there's one question missing 
################################################################################
################################################################################
# get each ysr separately
# 
ysr.1 <- raw %>%
  select(1,3:172) %>%
  drop_na(ysr_timestamp) %>%
  rename(devGenes_id = participant_id,
         no_school = no_schooling,
         no_school_d = no_school_explain,
         timestamp = ysr_timestamp,
         hob = hob_child_1,
         org = org_child1,
         jobs = jobs_child1) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_2")) %>%
  rename_at(.vars = vars(contains("describes")), .funs = function(x) sub("_describes", "_d", x)) %>% 
  rename_at(.vars = vars(contains("describe")), .funs = function(x) sub("_describe", "_d", x)) %>%
  rename(sees_things = see_things,
         hear_sounds = hears_sounds,
         hear_sounds_d = hears_sounds_d,
         speech_probs_d = speech_problems_d,
         trouble_sleeping = trouble_sleeping1,
         drug_use = uses_drugs,
         drug_use_d = uses_drugs_d,
         eye_probs = eye_problems,
         eye_probs_d = eye_problems_d,
         ysr_complete = ysr_complete)
ysr.2 <- raw %>%
  select(1, 173:341) %>%
  drop_na(ysr_2_timestamp) %>%
  rename(devGenes_id = participant_id,
         no_school = no_school1,
         no_school_d = no_schooling_v2,
         timestamp = ysr_2_timestamp,
         hob = hob_child2,
         org = org_child2,
         jobs = jobs_child2) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_3"),
         acts_young = NA) %>%
  rename_at(.vars = vars(ends_with("_v2")), .funs = function(x) sub("_v2", "", x)) %>%
  rename_at(.vars = vars(contains("_v2")), .funs = function(x) sub("_v2", "", x)) %>% 
  rename(body_mvmts = body_mvmt,
         speech_probs_d = speech_problems_d,
         hear_sounds = hears_sounds,
         hear_sounds_d = hears_sounds_d,
         body_mvmts_d = body_mvmt_d,
         drug_use = uses_druges,
         drug_use_d = uses_drugs_d,
         drink_alc_d = drinks_alc_d,
         eye_probs = problems_eyes,
         eye_probs_d = problem_eyes_d,
         ysr_complete = ysr_2_complete)
ysr.3 <- raw %>%
  select(1, 342:510) %>%
  drop_na(ysr_3_timestamp) %>%
  rename(devGenes_id = participant_id,
         no_school = no_school_v3,
         no_school_d = no_schooling_v2_v3,
         timestamp = ysr_3_timestamp,
         hob = hob_child3,
         org = org_child3,
         jobs = jobs_child3) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_4"),
         acts_young = NA) %>%
  rename_at(.vars = vars(ends_with("_v3")), .funs = function(x) sub("_v2_v3", "", x)) %>%
  rename_at(.vars = vars(contains("_v3")), .funs = function(x) sub("_v3", "", x)) %>%
  rename(afraid_things_d = afriad_things_d,
         sees_things_d = sees_thing_d,
         drug_use = drugs_use,
         eye_probs = problem_eyes,
         eye_probs_d = eye_problem_d,
         ysr_complete = ysr_3_complete,
         other_physical_probs_d = other_physical_prob_d)
ysr.4 <- raw %>%
  select(1, 511:679) %>%
  drop_na(ysr_4_timestamp) %>%
  rename(devGenes_id = participant_id,
         no_school = no_schooling_v4,
         no_school_d = no_schooling_v2_v3_v4,
         timestamp = ysr_4_timestamp,
         hob = hob_child4,
         org = org_child4,
         jobs = jobs_child4) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_5"),
         acts_young = NA) %>%
  rename_at(.vars = vars(ends_with("_v4")), .funs = function(x) sub("_v2_v3_v4", "", x)) %>%
  rename_at(.vars = vars(contains("_v4")), .funs = function(x) sub("_v4", "", x)) %>%
  rename(store_up_d = stores_up_d,
         drink_alc = drinks_alc,
         drink_alc_d = drinks_alc_d,
         repeat_acts = repeats_acts,
         repeat_acts_d = repeats_acts_d,
         sleep_more = sleeps_more,
         sleep_more_d = sleeps_more_d,
         store_up = stores_up,
         trouble_sleeping = sleeping_probs,
         trouble_sleeping_d = sleeping_probs_d,
         eye_probs = eye_problems,
         other_physical_probs = other_physical_prob,
         ysr_complete = ysr_4_complete)
ysr.5 <- raw %>%
  select(1, 680:848) %>%
  drop_na(ysr_5_timestamp) %>%
  rename(devGenes_id = participant_id,
         no_school = no_schooling_v5,
         no_school_d = no_schooling_v2_v3_v4_v5,
         timestamp = ysr_5_timestamp,
         hob = hob_child5,
         org = org_child5,
         jobs = jobs_child5) %>%
  mutate(devGenes_id = paste0(devGenes_id, "_6"),
         acts_young = NA) %>%
  rename_at(.vars = vars(ends_with("_v5")), .funs = function(x) sub("_v2_v3_v4_v5", "", x)) %>%
  rename_at(.vars = vars(contains("_v5")), .funs = function(x) sub("_v5", "", x)) %>%
  rename(drink_alc = drinks_alc,
         drink_alc_d = drinks_alc_d,
         body_mvmts_d = bodu_mvmts_d,
         speech_problems = speech_prob,
         speech_probs_d = speech_prob_d,
         store_up = stores_up,
         store_up_d = stores_up_d,
         ysr_complete = ysr_5_complete,
         other_physical_probs_d = other_physical_prob_d)
################################################################################
# something is wrong with these ysr
# they don't match same questions
# I made this dataframe to review the question/variable names for each survey and 
# correct them as done in the renaming above
tmp <- data.frame(ysr1 = colnames(ysr.1),
                  ysr2 = c(colnames(ysr.2)[1:34], NA,colnames(ysr.2)[35:170]),
                  ysr3 = c(colnames(ysr.3)[1:34], NA,colnames(ysr.3)[35:170]),
                  ysr4 = c(colnames(ysr.4)[1:34], NA,colnames(ysr.4)[35:170]),
                  ysr5 = c(colnames(ysr.5)[1:34], NA,colnames(ysr.5)[35:170])) %>%
  mutate(equal = ifelse((ysr1==ysr2)&(ysr2==ysr3)&(ysr3==ysr4)&(ysr4==ysr5), T, F))
################################################################################
# SAVE, FINALLY!!!
ysr.all <- rbind(ysr.1,ysr.2, ysr.3, ysr.4, ysr.5)
write_csv(ysr.all, "data/derivatives/ysr.csv")
################################################################################


################################################################################
