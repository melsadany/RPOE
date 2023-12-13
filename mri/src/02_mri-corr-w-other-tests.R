################################################################################
#                       correlate fMRI log data and IQ tests                   #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/mri"
setwd(project.dir)
################################################################################
# read fmri processed data
p.list <- list.dirs("/Dedicated/jmichaelson-sdata/MRI/RPOE", recursive = F)
registerDoMC(cores = 4)
mri.meta <- foreach (i = 1:(length(p.list)-1), .combine = rbind) %dopar% {
  f.path <- p.list[i]
  pid <- sub("/Dedicated/jmichaelson-sdata/MRI/RPOE/", "", f.path)
  f.path2 <- paste0(project.dir, "/data/derivatives/MRI-log/mdata/", pid, "_mdata.tsv")
  if (file.exists(f.path2)) {
    t <- read_tsv(f.path2) %>%
      mutate(te_id = pid) %>%
      mutate(keypress = readr::parse_number(keypress)) %>%
      mutate(hand = ifelse(keypress %in% c(6:9), "R", 
                           ifelse(keypress %in% c(1:5), "L", NA))) %>%
      mutate(converted_keypress = ifelse(hand == "L", 
                                         keypress+5, keypress)) %>%
      mutate(keypress_rel_time = ifelse(keypress_rel_time>0, keypress_rel_time, NA))
    return(t)
  }else {
    return(NULL)
  }
}

task.template <- mri.meta %>% 
  mutate(correct_answer = ifelse(exp_condition == "same", 7, 
                                 ifelse(exp_condition == "diff", 8, 
                                        ifelse(exp_condition == "coherent", 7,
                                               ifelse(exp_condition == "incoherent", 8,
                                                      NA))))) %>%
  select(task, exp_condition, specific, file, duration, correct_answer) %>%
  slice_head(n=209)
task.total <- task.template %>%
  group_by(task, exp_condition) %>%
  dplyr::summarise(task_total = n())
################################################################################
# how many were correct per participant
p.scores <- mri.meta %>%
  left_join(task.template) %>%
  distinct() %>%
  filter(!is.na(correct_answer)) %>%
  select(te_id, task, exp_condition,file, converted_keypress, correct_answer) %>%
  mutate(answered_correctly = ifelse(is.na(converted_keypress), F,
                                     ifelse(converted_keypress == correct_answer, T, F))) %>%
  group_by(te_id, task, exp_condition) %>%
  dplyr::summarise(total = sum(answered_correctly)) %>%
  left_join(task.total, relationship = "many-to-many") %>% 
  mutate(task_exp = paste0(task, "_",exp_condition, "_score")) %>% 
  pivot_wider(names_from = task_exp, values_from = total, id_cols = te_id) %>% ungroup()

# get the relative time needed to define coh/non-coh and same/diff for the ones answered
p.scores.t <- mri.meta %>%
  filter(task %in% c("PS_samediff", "semantic_coherence")) %>%
  mutate(abs_keypress_t = keypress_rel_time - rel_time) %>%
  select(te_id, task, exp_condition, abs_keypress_t) %>%
  drop_na() %>%
  group_by(te_id, task, exp_condition) %>%
  dplyr::summarise(avg_time_for_same_coh = mean(abs_keypress_t)) %>%
  mutate(task_2 = paste0(task, "_", exp_condition, "_avg_t")) %>%
  pivot_wider(names_from = task_2, values_from = avg_time_for_same_coh, id_cols = "te_id")
################################################################################
# how many words were thought of
word.count <- mri.meta %>%
  filter(task == "word_association", exp_condition == "word") %>%
  mutate(count = ifelse(is.na(count), 0, count)) %>%
  group_by(te_id) %>%
  dplyr::summarise(total_word_association_words = sum(count))
################################################################################
# get the relative time or how long it takes them to read the number in mind
number.presses <- mri.meta %>%
  filter(task == "word_association", exp_condition == "number") %>%
  mutate(press = ifelse(is.na(keypress), F, T)) %>%
  select(te_id, press) %>%
  group_by(te_id) %>%
  dplyr::summarise(total_number_press = sum(press))
number.presses.time <- mri.meta %>%
  filter(task == "word_association", exp_condition == "number") %>%
  mutate(abs_keypress_t = keypress_rel_time - rel_time) %>%
  select(te_id, abs_keypress_t) %>%
  drop_na() %>%
  group_by(te_id) %>%
  dplyr::summarise(avg_number_keypress_abs_t = mean(abs_keypress_t, na.omit = T))
################################################################################
# read tests data
nih.tb <- read_csv("../language/data/derivatives/nih-tb_clean_120623.csv")
iq <- read_csv("../language/data/derivatives/wisc-and-wais_clean_120623.csv")
################################################################################
# clean NIH-TB and IQ
m1 <- nih.tb %>%
  select(dev_id, 
         ends_with("age_corrected_standard_score")) %>%
  drop_na()
m2 <- iq %>%
  select(dev_id, te_id,
         paste0(c("PSI", "WM", "VCI"), "_composite_score"),
         # ends_with("composite_score"),
         FSIQ, 
         SI, VC, BD, VP, MR, 
         #FW, PS, IN, AR, 
         DS, CD, SS)
m1.m2 <- inner_join(m1, m2) %>%
  mutate(te_id=ifelse(is.na(te_id), dev_id, te_id)) %>%
  rename(te_id = te_id)
################################################################################
# correlation between fmri scores and iq/nih
m123 <- inner_join(inner_join(p.scores, p.scores.t), 
                   inner_join(m1.m2, inner_join(word.count, 
                                                inner_join(number.presses, number.presses.time))))
corr.table(m123 %>% select(colnames(m1.m2), -te_id, -dev_id),
           m123 %>% select(-colnames(m1.m2))) %>%
  filter(V1 %in% colnames(m1.m2), !V2 %in% colnames(m1.m2)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2))) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ")) %>%
  mutate(V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1))) %>%
  mutate(cat1 = ifelse(V2 %in% c(colnames(number.presses), colnames(word.count), colnames(p.scores)), "total",
                      ifelse(V2 %in% c(colnames(number.presses.time), colnames(p.scores.t)), "time", "other"))) %>%
  ggplot(aes(x=V1, y=V2, fill=r, label=ifelse(pval<0.05, "*", ""))) +
  geom_tile() +
  geom_text(size=3, color = "white") +
  ggh4x::facet_grid2(cols = vars(cat2), rows = vars(cat1), scales = "free", space = "free") +
  redblack.col.gradient + my.guides + null_labs +
  labs(caption = paste0("n(samples): ", nrow(m123)))
ggsave(filename = "figs/correlation-between-fmri-scores-and-data-with-nih-iq.png",
       width = 10, height = 10, units = "in", dpi = 320, bg = "white")
################################################################################


################################################################################


################################################################################