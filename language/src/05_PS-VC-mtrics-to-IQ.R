################################################################################
#            analyzing PS_VC features o predict IQ and NIH-TB scores           #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files here
m1.m2 <- read_rds("data/derivatives/m1m2.rds") 
demo <- read_rds("data/raw/demo.rds")
ps.vc <- read_rds("data/derivatives/ps-vc-text-clean.rds")
#####
pairs <- read_rds("data/derivatives/cons-pairs.rds") %>% as.data.frame()
euc <- read_rds("data/derivatives/euc-distance-w-minimum-of-2-words-per-task.rds") %>% as.data.frame() 
divergence <- read_rds("data/derivatives/divergence-w-minimum-of-3-words-per-task.rds") %>% as.data.frame()
voc.depth <- read_rds("data/derivatives/chulls.rds") %>% as.data.frame() %>%
  pivot_longer(cols = starts_with("vol_"), names_to = "vol_source", values_to = "vol_value") %>%
  mutate(vol_source = sub("vol_", "", vol_source)) %>% filter(nchar(vol_source)==1) %>%
  select(te_id, vol_source, vol_value)
comm.returns <- read_rds("data/derivatives/lmer-inputs/comm-returns.rds") %>% as.data.frame()
#############
################################################################################
################################################################################
# get the latency time to say the first word per task
onset <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
  mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised))),
         word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  mutate(text = tolower(text)) %>%
  filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
  select(te_id=ID, task, word, text, start, end) %>%
  group_by(te_id, word) %>%
  slice_head(n=1) %>%
  mutate(onset = start/1000)
################################################################################
################################################################################
# identify number of comments said by participant
comments <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
  mutate(comment = ifelse(comment == "T", 1, 0), te_id = ID) %>%
  group_by(te_id, word) %>%
  dplyr::summarise(tot_comments = sum(comment, na.rm = T))
################################################################################
################################################################################
# identify off-target in COWAT
off.target <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
  mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised))),
         word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  mutate(text = tolower(text)) %>%
  filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
  select(te_id=ID, task, word, text) %>%
  filter(task == 3) %>%
  mutate(off_target = ifelse(str_sub(text, start = 1, end = 1) == tolower(word), F, T)) %>%
  group_by(te_id, word) %>%
  dplyr::summarise(off_target = sum(off_target, na.rm = T))
################################################################################
################################################################################
# count of repeated prompt
rep.prompt <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
  mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised))),
         word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  mutate(text = tolower(text)) %>%
  filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
  select(te_id=ID, task, word, text) %>%
  mutate(rep_prompt = ifelse(text == tolower(word), T, F)) %>%
  group_by(te_id, word) %>%
  dplyr::summarise(rep_prompt = sum(rep_prompt, na.rm = T))
################################################################################
################################################################################
# repeated words not consec
rep.words <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
  mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised))),
         word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  mutate(text = tolower(text)) %>%
  filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
  select(te_id=ID, task, word, text) %>%
  group_by(te_id, word) %>%
  dplyr::summarise(rep_words_per_prompt = sum(duplicated(text), na.rm = T))
rep.words.recycled <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
  mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised))),
         word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  mutate(text = tolower(text)) %>%
  filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
  select(te_id=ID, task, word, text) %>%
  group_by(te_id) %>%
  dplyr::summarise(count_all = n(),
                   rep_word_at_all = sum(duplicated(text), na.rm = T))
################################################################################
################################################################################
################################################################################
# combine all of these features in one df
all <- full_join(onset %>% select(te_id, prompt = word, onset), 
                 comments %>% rename(prompt = word)) %>%
  full_join(off.target %>% rename(prompt = word)) %>%
  full_join(rep.prompt %>% rename(prompt = word)) %>%
  full_join(rep.words %>% rename(prompt = word)) %>%
  full_join(rep.words.recycled)
avg <- all %>%
  group_by(te_id) %>%
  dplyr::summarise_at(.vars = colnames(all)[3:ncol(all)], .funs = function(x) mean(x, na.rm = T))
avg.t1 <- all %>%
  filter(nchar(prompt)>1) %>% select(-off_target) %>%
  group_by(te_id) %>%
  dplyr::summarise_at(.vars = vars(2:(ncol(all)-2)), .funs = function(x) mean(x, na.rm = T))
avg.t3 <- all %>%
  filter(nchar(prompt)==1) %>%
  group_by(te_id) %>%
  dplyr::summarise_at(.vars = colnames(all)[3:ncol(all)], .funs = function(x) mean(x, na.rm = T))
################################################################################
################################################################################
combined.metrics <- full_join(avg, 
                              euc %>% 
                                select(te_id, prompt = word, full_euc_dist_normalized) %>%
                                group_by(te_id) %>%
                                dplyr::summarise(normalized_euc = mean(full_euc_dist_normalized, na.rm = T))) %>%
  full_join(divergence %>% 
              select(te_id, prompt = word, global_divergence) %>%
              group_by(te_id) %>%
              dplyr::summarise(divergence = mean(global_divergence, na.rm = T))) %>%
  full_join(pairs  %>% 
              select(te_id, prompt = word, cos_similarity) %>%
              group_by(te_id) %>%
              dplyr::summarise(cos_sim = mean(cos_similarity, na.rm = T))) %>%
  full_join(voc.depth  %>% 
              select(te_id, prompt = vol_source, vol_value) %>%
              group_by(te_id) %>%
              dplyr::summarise(vocab_depth = mean(vol_value, na.rm = T))) %>%
  full_join(comm.returns  %>% 
              select(te_id, prompt = word, archetype, lifetime) %>%
              group_by(te_id, prompt, archetype) %>%
              dplyr::summarise(avg_lifetime = mean(lifetime, na.rm = T)) %>%
              ungroup() %>% group_by(te_id, prompt) %>%
              dplyr::summarise(avg_lifetime = mean(avg_lifetime, na.rm = T)) %>%
              ungroup() %>% group_by(te_id) %>%
              dplyr::summarise(community_lifetime = mean(avg_lifetime, na.rm = T))) %>%
  full_join(comm.returns %>%
              select(te_id, prompt = word, archetype, switches) %>%
              group_by(te_id, prompt) %>%
              dplyr::summarise(avg_switches = sum(switches, na.rm = T)) %>%
              ungroup() %>% group_by(te_id) %>%
              dplyr::summarise(community_switches = mean(avg_switches, na.rm = T)))

# correlate features with m1m2
m123 <- inner_join(m1.m2, combined.metrics)
# complete.cases(m123)
corr.table(m123 %>% select(colnames(m1.m2), -ends_with("_id")),
           m123 %>% select(any_of(colnames(combined.metrics)), -te_id),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(combined.metrics),
         V2 %in% colnames(m1.m2)) %>%
  mutate(V2 = sub("_age_corrected_standard_score", "_NIH", V2),
         cat2 = ifelse(grepl("NIH", V2), "NIH-TB", "IQ"),
         V2 = sub("_NIH", "", V2),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V2, y=V1, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, ".",""))))+
  geom_tile()+
  geom_text(color = "white", nudge_x = 0)+
  redblack.col.gradient +
  ggh4x::facet_grid2(cols = vars(cat2), scales = "free") +
  labs(y = "", x = "", 
       caption = paste0("n(samples): ", nrow(m123),"\n",
                        "data was averaged from all prompts", "\n",
                        "    ** FDR < 0.05", "\n",
                        "    .   pval < 0.05")) +
  my.guides
ggsave("figs/corr-iq-nih-averaged-metrics.png", bg = "white",
       width = 8, height = 8, units = "in", dpi = 360)
################################################################################
################################################################################
