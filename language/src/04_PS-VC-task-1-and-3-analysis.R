################################################################################
#            analyzing PS_VC 1&3 and its correlation with other tests          #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = lubridate::interval(dob, age.day) / lubridate::duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
#  read the PS_VC task metadata
ps.vc.metadata.r <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 2) %>%
  dplyr::filter(task_v==2)
ps.vc.metadata <- ps.vc.metadata.r %>%
  mutate(start_in_sec = start_in_sec - ps.vc.metadata.r$start_in_sec[1],
         end_in_sec = end_in_sec - ps.vc.metadata.r$start_in_sec[1]) %>%
  select(task_num, word, start_in_sec, end_in_sec) %>%
  rownames_to_column("task_order") %>% mutate(task_order = as.numeric(task_order))
# keep participants of interest
participants.metadata <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 1)
p.of.int <- participants.metadata %>% 
  filter(work_on_2 != "F")
rm(participants.metadata);rm(ps.vc.metadata.r);gc()
# get demo and clean
demo <- readxl::read_xlsx("data/raw/RPOE_participants_metadata.xlsx") %>%
  mutate(age = age(dob = DOB, floor = F)) %>%
  select(3:7)
write_rds(demo, "data/raw/demo.rds")
################################################################################
# read tests data
m1.m2 <- read_rds("data/derivatives/m1m2.rds")
################################################################################
################################################################################
# extract basic dictionary analysis, word_count, ums_count, waiting time

#####
# clean the transcription data
#####
vc.transcription <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 4) %>%
  mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised))),
         word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  mutate(text = tolower(text)) %>%
  filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
  select(te_id=ID, task, task_order, word, text, start, end) %>%
  filter(word != text) %>% #drop the words that are exactly the same as the prompt word
  distinct(te_id, task, task_order, word, text, .keep_all = T) # only keep unique words and drop repeated by the same participant in the same task/word
#####
# look at categories of words said
#####
vc.analyzed <- cbind(vc.transcription,
                     nrc = syuzhet::get_nrc_sentiment(vc.transcription$text),
                     sentimentr::profanity(vc.transcription$text)%>%select(profanity_count),
                     lingmatch = lingmatch::lma_meta(vc.transcription$text),
                     lingmatch = lingmatch::lma_termcat(vc.transcription$text)) %>%
  select(-c(lingmatch.words, lingmatch.unique_words, lingmatch.clauses, lingmatch.sentences, 
            lingmatch.words_per_clause,
            lingmatch.words_per_sentence, lingmatch.characters_per_word, lingmatch.syllables_per_word,
            lingmatch.type_token_ratio))
########
# save #
########
write_rds(vc.transcription, "data/derivatives/ps-vc-text-clean.rds")
write_csv(vc.transcription, "data/derivatives/ps-vc-text-clean.csv")
write_rds(vc.analyzed, "data/derivatives/ps-vc-text-analyzed.rds")
rm(vc.analyzed);rm(vc.transcription);gc()
all <- read_rds("data/derivatives/ps-vc-text-analyzed.rds")
#####
# count um, uh, eh, hmm, oh
#####
ums <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 4) %>%
  mutate(text = ifelse(is.na(text_revised), text,
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised)))) %>%
  mutate(word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  filter(text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>%
  group_by(ID) %>%
  dplyr::summarise(ums_count = n()) %>%
  rename(te_id=ID) %>%
  full_join(all[,1:5] %>% distinct(te_id)) %>%
  mutate(ums_count = ifelse(is.na(ums_count), 0, ums_count))
#####
# get words count per participant
#####
word.count <- all[,1:5] %>%
  group_by(te_id) %>%
  dplyr::summarise(word_count = n())
#####
# number of characters
#####
chr.wise <- all %>%
  pivot_longer(cols = c(lingmatch.characters, lingmatch.syllables, lingmatch.reading_grade), 
               names_to = "cat1") %>%
  group_by(te_id, cat1) %>%
  dplyr::summarise(avg = mean(value, na.omit = T)) %>%
  pivot_wider(names_from = "cat1", values_from = "avg", id_cols = "te_id")
#####
# ratio of words category being said
#####
word.wise <- all %>%
  pivot_longer(cols = c(starts_with("nrc"), profanity_count, 
                        lingmatch.sixltr, lingmatch.ppron, lingmatch.ipron, lingmatch.adverb, 
                        lingmatch.conj, lingmatch.auxverb, lingmatch.prep, lingmatch.negate, lingmatch.quant), 
               names_to = "cat2") %>%
  group_by(te_id, cat2) %>%
  dplyr::summarise(count = sum(value)) %>%
  left_join(word.count) %>%
  mutate(cat_ratio = count / word_count) %>%
  pivot_wider(names_from = "cat2", values_from = "cat_ratio", id_cols = "te_id")
#####
# get average of waiting time between consecutive pairs of words
#####
wait.time <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 4) %>%
  mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised)))) %>%
  mutate(word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  mutate(text = tolower(text)) %>%
  filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
  select(te_id=ID, task, task_order, word, text, start, end) %>%
  filter(word != text) %>% #drop the words that are exactly the same as the prompt word
  distinct(te_id, task, task_order, word, text, .keep_all = T) %>% # only keep unique words and drop repeated by the same participant in the same task/word
  drop_na(start, end) # drop the words with no timestamps
wait.time2 <- cbind(wait.time[-nrow(wait.time),]%>%select(1:4),
                    w1_index = rownames(wait.time)[-nrow(wait.time)],
                    w2_index = rownames(wait.time)[-1],
                    wait=wait.time$start[-1] - wait.time$end[-nrow(wait.time)]) %>%
  filter(wait>0) %>%
  group_by(te_id) %>%
  dplyr::summarise(avg_wait = mean(wait))
#####
# save summ for PS-VC
#####
ttt <- inner_join(inner_join(chr.wise, word.wise),
                  inner_join(word.count, 
                             inner_join(ums,wait.time2)))
write_rds(ttt, file = "data/derivatives/ps-vc-summary-data.rds")
###############
###############
# get the correlation between nih-tb / IQ and word count/ums count/language features from PS-VC audio
###############
###############
m124 <- inner_join(m1.m2, 
                   inner_join(inner_join(chr.wise, word.wise),
                              inner_join(word.count, inner_join(ums,wait.time2))))
corr.table(m124 %>% select(any_of(c(colnames(m1.m2))),
                           -ends_with("id")),
           m124 %>% select(colnames(chr.wise), colnames(word.wise), 
                           ums_count, word_count, avg_wait, -te_id),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2)), 
         V2 %in% c("ums_count", "word_count", "avg_wait",colnames(chr.wise), colnames(word.wise))) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2)),
         cat1 = ifelse(grepl(paste(c("characters",  "reading_grade", "syllables"), 
                                   collapse = "|"), V2), 
                       "average", 
                       ifelse(V2 %in% c("ums_count", "word_count", "avg_wait"), 
                              "total", 
                              "ratio")),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = sub("lingmatch\\.", "", V2),
         V2 = sub("nrc\\.", "", V2),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(rows = vars(cat1), 
                     cols = vars(cat2),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "language metrics from recorded PS-VC",
       caption = paste0("n(samples): ", nrow(m124), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_iq-nih-PS-VC-language-features.png"),
       width = 6, height = 8, units = "in", dpi = 320, bg = "white")
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
####################### word embeddings by text package ########################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
rm(list=ls());gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(text)
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
all <- read_rds("data/derivatives/ps-vc-text-analyzed.rds") %>%
  mutate(text=tolower(text))
################################################################################
# get word embeddings for prompt words and responses and save
# the package gets embeddings from hugging face model "bert-base-uncased"
# 
emb.text <- textEmbed(unique(all$text))
emb.text.m <- emb.text$texts$texts
emb.text.m <- left_join(all[, 1:5], cbind(text = unique(all$text), emb.text.m))
emb.word <- textEmbed(unique(all$word))
emb.word.m <- emb.word$texts$texts
emb.word.m <- left_join(all[, 1:4], cbind(word = unique(all$word), emb.word.m))
save(emb.text, emb.text.m, emb.word, emb.word.m, file = "data/derivatives/word-embedding-from-text-package.rda")
# load("data/derivatives/word-embedding-from-text-package.rda")
# get the cosine similarity between word embeddings of prompt and the response form participants
similarity <- text::textSimilarity(emb.text.m,emb.word.m)
write_rds(similarity, "data/derivatives/similarity-bet-response-and-prompt-task-1-text-embeddings.rds")
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
##################### similarity between pairs of words ########################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# set up
rm(list=ls());gc();source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(text)
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files before
demo <- read_rds("data/raw/demo.rds")
m1.m2 <- read_rds("data/derivatives/m1m2.rds")
# get the clean transcription for data
all <- read_rds("data/derivatives/ps-vc-text-analyzed.rds")
# get the embeddings from text package
load("data/derivatives/word-embedding-from-text-package.rda")
emb.text.m <- emb.text.m %>% distinct(te_id,word,text, .keep_all = T) # keep unique words per participant for each mini-task
################################################################################
# get the cosine similarity between each pair of words for each participant
# make pairs per task 1
t=3 # identify the task you want to work on. either 1 or 3
registerDoMC(cores = 4)
pairs.sim <- foreach(i = 1:length(unique(emb.text.m$te_id)), .combine = rbind) %dopar% {
  id <- unique(emb.text.m$te_id)[i]
  # get the prompt that we have a transcription for
  prompts <- unique(emb.text.m$word)
  # loop over these prompts and extract similiraity between every possible pair
  p.sim.2 <- foreach(k = 1:length(prompts), .combine = rbind) %dopar% {
    # identify prompt word
    w0 <- prompts[k]
    # extract participant's response
    tmp.emb <- emb.text.m %>% 
      filter(te_id ==id, task==t, word ==w0)
    # make sure you have a response for that prompt
    if (nrow(tmp.emb) == 0) {
      return(NULL)
    }
    # build a dataframe that has every possible combination of pairs from words said
    sim.df <- data.frame(te_id = id,
                         word = w0,
                         w_order = rep(c(1:nrow(tmp.emb)), each = nrow(tmp.emb)),
                         w1 = rep(tmp.emb$text, each = nrow(tmp.emb)),
                         w2 = rep(tmp.emb$text, nrow(tmp.emb)),
                         cos_similarity = NA)
    # loop over these pairs, and calculate the cosine similarity between their word embeddings
    for (j in 1:nrow(sim.df)) {
      # j=1
      w1 <- sim.df$w1[j]
      w2 <- sim.df$w2[j]
      # get embeddings of word 1
      e1 <- tmp.emb %>%
        distinct(text, .keep_all = T) %>%
        filter(text == w1) %>%
        select(-c(1:5))
      # get embeddings of word 2
      e2 <- tmp.emb %>%
        distinct(text, .keep_all = T) %>%
        filter(text == w2) %>%
        select(-c(1:5))
      # calculate the cosine similarity and save it
      sim.df$cos_similarity[j] <- text::textSimilarity(e1,e2, method = "cosine")
    }
    return(sim.df)
  }
  return(p.sim.2)
}
# save the similarity between pairs
write_rds(pairs.sim, paste0("data/derivatives/pairs-sim-by-word-by-participant-task-",t,".rds"))
rm(pairs.sim); rm(t); gc()
# if you already calculated the similarity before, load them here
pairs.sim.1 <- read_rds(paste0("data/derivatives/pairs-sim-by-word-by-participant-task-1.rds"))
pairs.sim.3 <- read_rds(paste0("data/derivatives/pairs-sim-by-word-by-participant-task-3.rds"))
pairs.sim <- rbind(pairs.sim.1 %>% mutate(task=1), pairs.sim.3 %>% mutate(task=3)) %>%
  filter(w1 != w2)
w2_order <- pairs.sim %>% 
  select(1,2,w2_order=w_order,w2=w1) %>% distinct()
pairs.sim <- pairs.sim %>%
  left_join(w2_order, relationship = "many-to-many")
write_rds(pairs.sim, "data/derivatives/pairs-sim-by-word-by-participant.rds")
################################################################################
# histogram of cosine similarity distribution for pairs
# get a df of pairs sim, just for consec pairs
cons.pairs <- pairs.sim %>%
  mutate(consec = ifelse(w2_order==w_order+1, T, F)) %>%
  filter(consec==T)
write_rds(cons.pairs, "data/derivatives/cons-pairs.rds")
pairs.sim %>%
  mutate(consec = ifelse(w2_order==w_order+1, T, F)) %>%
  ggplot(aes(x=cos_similarity, fill = consec)) +
  geom_histogram(bins = 100) +
  facet_wrap(~task) +
  scale_fill_manual(values = redblack.col, name = "consecuetive pairs only") +
  labs(title = "faceted by task")
ggsave(bg = "white", filename = "figs/distribution-of-cos-similarity-of-all-pairs.png",
       width = 4, height = 4, units = "in", dpi = 320)
################################################################################
################################# correlations #################################
################################################################################
######
# correlation between consec. pairs similarity and IQ
######
# get a df of pairs sim, just for consec pairs
m125 <- left_join(cons.pairs, m1.m2) %>%
  drop_na() %>%
  left_join(demo)
# write_rds(m125, "data/derivatives/lmer-inputs/cons-pairs.rds")
####
# lmer for demo
lm <- lmerTest::lmer(cos_similarity ~ age + sex + age:sex + (1|te_id) + (1|word),
                     data = m125 %>%
                       select(cos_similarity, te_id, word, age, sex))
# get summ
demo.lmer <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
  as.data.frame() %>%
  rownames_to_column("fixed") %>%
  filter(fixed != "(Intercept)") %>%
  rename(Estimate = `Est.`,
         confint_min = `2.5%`,
         confint_max = `97.5%`,
         pval = p) %>%
  mutate(var = "demo")
write_rds(demo.lmer, "data/derivatives/demo-lmer/pairs-sim.rds")
####
# lmer for IQ
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=11:37, .combine = rbind) %dopar% {
  var <- colnames(m125)[i]
  # predict cosine similarity of the pair using the IQ/NIH measure. 
  # adding participant's ID as a random variable, and the word/mini-task
  lm <- lmerTest::lmer(cos_similarity ~ xx + age + sex + age:sex + (1|te_id) + (1|word),
                       data = cbind(m125 %>% 
                                      select(cos_similarity, te_id, word, age, sex),
                                    xx=m125[,i]) %>%
                         rename(xx=6))
  gc()
  # combine results in a df, and save
  df <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    rename(Estimate = `Est.`,
           confint_min = `2.5%`,
           confint_max = `97.5%`,
           pval = p) %>%
    mutate(var = var)
  write_rds(df, paste0("data/derivatives/pairs-lmer/", var, ".rds"))
  gc()
  return(df)
}
# combine the saved lmer results
lm.results <- foreach(i=9:ncol(m125), .combine = rbind) %dopar% {
  var <- colnames(m125)[i]
  if (file.exists(paste0("data/derivatives/pairs-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/pairs-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
lm.results <- lm.results %>% mutate(FDR = p.adjust(pval, method = "fdr"))
# write_rds(lm.results, "data/derivatives/pairs-lmer/all-lmer-results.rds", compress = "gz")
# make plot for results
p1 <- lm.results %>%
  filter(fixed == "xx") %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  mutate(var = sub("_age_corrected_standard_score", "_NIH", var),
         cat2 = ifelse(grepl("NIH", var), "NIH-TB", "IQ"),
         var= sub("_NIH", "", var),
         var = factor(var, levels = unique(var))) %>%
  ggplot(aes(x=Estimate, y=var,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free", space = "free") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting cosine similarity of consecuitive pairs", y="",
       caption = paste0("n(samples): ", length(unique(m125$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(cos_similarity ~ X + age + sex + age:sex + (1|te_id) + (1|prompt))", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables"))
# demographics plot
p2 <- demo.lmer %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  ggplot(aes(x=Estimate, y=fixed,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting cosine similarity of consecuitive pairs", y="",
       caption = paste0("n(samples): ", length(unique(m125$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(cos_similarity ~ age + sex + age:sex + (1|te_id) + (1|prompt))"))
patchwork::wrap_plots(p2,p1,ncol = 1,heights = c(1,5))
ggsave(filename = "figs/lmer-cos-sim-consec-pairs-by-iq-random-id-and-word.png",
       width = 8, height = 10, units = "in", bg = "white", dpi = 360)
#############
# get correlation between consec. pairs similarity and the waiting time between these pairs
#############
psvc.clean <- read_rds("data/derivatives/ps-vc-text-clean.rds")
psvc.v0 <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 4) %>% rename(te_id=ID)
psvc.clean.v2 <- left_join(psvc.clean, psvc.v0 %>% select(start, end, colnames(psvc.clean))) %>%
  filter(word != text) %>% #drop the words that are exactly the same as the prompt word
  distinct(te_id, task, task_order, word, text, .keep_all = T) %>% # only keep unique words and drop repeated by the same participant in the same task/word
  drop_na(start, end) # drop the words with no timestamps
wait.time <- cbind(psvc.clean.v2[-nrow(psvc.clean.v2),] %>% select(-text),
                   w1 = psvc.clean.v2$text[-nrow(psvc.clean.v2)],
                   w2 = psvc.clean.v2$text[-1],
                   w1_index = rownames(psvc.clean.v2)[-nrow(psvc.clean.v2)],
                   w2_index = rownames(psvc.clean.v2)[-1],
                   wait=(psvc.clean.v2$start[-1] - psvc.clean.v2$end[-nrow(psvc.clean.v2)])/1000) %>%
  filter(wait>0)
tmp <- inner_join(cons.pairs, wait.time)
tmp %>% 
  ggplot(aes(x=cos_similarity, y=log2(wait))) +
  geom_point(size=1)+
  geom_smooth(method = "loess", color = boxplot.colors[2]) +
  geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  labs(x="cosine similarity of consecutive pair",
       y="log2(silence time between consec. pairs)",
       caption = paste0("n(samples): ", length(unique(tmp$te_id))))
ggsave(filename = "figs/corr-between-consec-pairs-cos-similarity-and-thinking-time.png", bg = "white",
       width = 4, height = 4, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
################### stats table for participants' performance ##################
################################################################################
################################################################################
################################################################################
################################################################################
# set up and clean
rm(list=ls());gc();source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files before
# get the clean transcription for data
all <- read_rds("data/derivatives/ps-vc-text-analyzed.rds")
################################################################################
#####
# table of how many words said by participant in each task
#####
tmp <- all[,1:5] %>%
  distinct(te_id, word, text, .keep_all = T) %>%
  group_by(te_id, word) %>%
  dplyr::summarise(count = n()) %>%
  pivot_wider(names_from = word, values_from = count, id_cols = te_id)
table <- kableExtra::kable(tmp, format="html") %>%
  kableExtra::kable_styling(full_width = T, protect_latex = T)
table
#####
# plot these stats
#####
tmp %>% 
  pivot_longer(cols = colnames(tmp)[-1], names_to = "word") %>%
  mutate(task = ifelse(nchar(word)==1, 3,1)) %>%
  group_by(word, value,task) %>%
  dplyr::summarise(count = n()) %>%
  # mutate(word = factor(word, levels = reorder(word, desc(task)))) %>%
  ggplot(aes(x=value, y=count))+
  geom_histogram(stat = "identity")+
  facet_wrap(~reorder(word, desc(task))) +
  geom_vline(xintercept = 5, color = "red", linetype=2) +
  labs(x="count of words said by participant",
       y="count of participants")
ggsave(filename = "figs/word-count-stats-per-task.png", bg = "white",
       width = 6, height = 5, units = "in", dpi = 320)
################################################################################
################################################################################
################################################################################
#################### calc Euclidean distance and correlate it ##################
################################################################################
################################################################################
################################################################################
# set up and clean
rm(list=ls());gc();source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files before
demo <- read_rds("data/raw/demo.rds")
# get the clean transcription for data
all <- read_rds("data/derivatives/ps-vc-text-analyzed.rds")
# get word embeddings from text package
load("data/derivatives/word-embedding-from-text-package.rda")
rm(emb.text);rm(emb.word);gc()
emb.text.m <- emb.text.m %>% distinct(te_id,word,text, .keep_all = T) # keep unique words per participant for each mini-task
m1.m2 <- read_rds("data/derivatives/m1m2.rds")
################################################################################
#####
# calculate the full euclidean distance traveled per task, and normalize it by number of words
#####
# this also considers at least 2 words said per task to get the euc distance

# get summary per participant per task
tmp <- all[,1:5] %>%
  distinct(te_id, word, text, .keep_all = T) %>%
  group_by(te_id, word) %>%
  dplyr::summarise(count = n()) %>%
  pivot_wider(names_from = word, values_from = count, id_cols = te_id)
# loop over participants and select tasks/words that have enough data for you
# then, get the embeddings for these words, and calculate the distance
# after getting the full euclidean distance per that task, divide it over number of words
euc <- foreach(i=1:length(unique(tmp$te_id)), .combine = rbind) %dopar% {
  id <- unique(tmp$te_id)[i]
  # get counts of response per task for the selected participant
  df1 <- tmp %>% 
    filter(te_id == id) %>%
    pivot_longer(cols = colnames(tmp)[-1]) %>%
    filter(value >=2)
  # identify unique tasks
  words.to.keep <- unique(df1$name)
  df2 <- emb.text.m %>%
    filter(te_id==id,
           word %in% words.to.keep)
  # loop over words here 
  word.opt <- foreach(j = 1:length(words.to.keep), .combine = rbind) %dopar% {
    w0 <- words.to.keep[j]
    df3 <- df2 %>%
      filter(word==w0)
    ######
    # get the overall euclidean distance and divide it by number of words
    text.to.look <- unique(df3$text)
    full.dist <- 0
    for (g in 1:(length(text.to.look)-1)) {
      t0 <- text.to.look[g]
      t1 <- text.to.look[g+1]
      euc.dist <- dist(rbind(as.numeric(emb.text.m%>%
                                          filter(te_id==id,word==w0,text==t0)%>%
                                          select(starts_with("Dim"))),
                             as.numeric(emb.text.m%>%
                                          filter(te_id==id,word==w0,text==t1)%>%
                                          select(starts_with("Dim")))))
      full.dist <- full.dist + euc.dist
    }
    full.dist.n <- as.numeric(full.dist)/nrow(df3)
    ######
    df6 <- data.frame(te_id = id,
                      word = w0,
                      full_euc_dist = as.numeric(full.dist),
                      full_euc_dist_normalized = as.numeric(full.dist.n))
    return(df6)
  }
  return(word.opt)
}
# save euc distances
write_rds(euc, "data/derivatives/euc-distance-w-minimum-of-2-words-per-task.rds")
# euc <- read_rds("data/derivatives/euc-distance-w-minimum-of-2-words-per-task.rds")
#####
# predict normalized Euclidean distance traveled 
#####
# use the IQ/NIH-TB as a major predictor
# add the te_id and the task name as random variables
m123 <- inner_join(euc, m1.m2) %>%
  left_join(demo)
####
# lmer for demo
lm <- lmerTest::lmer(full_euc_dist_normalized ~ age + sex + age:sex + (1|te_id) + (1|word),
                     data = m123 %>%
                       select(full_euc_dist_normalized, te_id, word, age, sex))
# get summ
demo.lmer <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
  as.data.frame() %>%
  rownames_to_column("fixed") %>%
  filter(fixed != "(Intercept)") %>%
  rename(Estimate = `Est.`,
         confint_min = `2.5%`,
         confint_max = `97.5%`,
         pval = p) %>%
  mutate(var = "demo")
write_rds(demo.lmer, "data/derivatives/demo-lmer/euc.rds")
####
# lmer for IQ
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=6:32, .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  # predict euclidean distance using the IQ/NIH measure. 
  # adding participant's ID as a random variable, and the word/mini-task
  lm <- lmerTest::lmer(full_euc_dist_normalized ~ xx + age + sex + age:sex + (1|te_id) + (1|word),
                       data = cbind(m123 %>% 
                                      select(full_euc_dist_normalized, te_id, word, age, sex),
                                    xx=m123[,i]) %>%
                         rename(xx=6))
  gc()
  # combine results in a df, and save
  df <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    rename(Estimate = `Est.`,
           confint_min = `2.5%`,
           confint_max = `97.5%`,
           pval = p) %>%
    mutate(var = var)
  write_rds(df, paste0("data/derivatives/euc-lmer/", var, ".rds"))
  gc()
  return(df)
}
# combine the saved lmer results
lm.results <- foreach(i=6:ncol(m123), .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  if (file.exists(paste0("data/derivatives/euc-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/euc-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
lm.results <- lm.results %>% mutate(FDR = p.adjust(pval, method = "fdr"))
# write_rds(lm.results, "data/derivatives/euc-lmer/all-lmer-results.rds", compress = "gz")
# make plot for results
p1 <- lm.results %>%
  filter(fixed == "xx") %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  mutate(var = sub("_age_corrected_standard_score", "_NIH", var),
         cat2 = ifelse(grepl("NIH", var), "NIH-TB", "IQ"),
         var= sub("_NIH", "", var),
         var = factor(var, levels = unique(var))) %>%
  ggplot(aes(x=Estimate, y=var,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free", space = "free") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting normalized full Euclidean distance", y="",
       caption = paste0("n(samples): ", length(unique(m123$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(normalized_euc_distance ~ X + age + sex + age:sex + (1|te_id) + (1|prompt))", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables", "\n",
                        "Derivation of Euclidean distance was as follows:", "\n",
                        "    distance = traveled distance by participant in each task/word","\n",
                        "    (i.e., sum of distance between consecuitive pairs)","\n",
                        "    Normalized by word count said by a participant in this task/word","\n",
                        "    Only kept participants with at least 2 words in response to task/word"))
# demographics plot
p2 <- demo.lmer %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  ggplot(aes(x=Estimate, y=fixed,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting normalized full Euclidean distance", y="",
       caption = paste0("n(samples): ", length(unique(m123$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(normalized_euc_distance ~ age + sex + age:sex + (1|te_id) + (1|prompt))"))
patchwork::wrap_plots(p2,p1,ncol = 1,heights = c(1,5))
ggsave(filename = "figs/lmer-full-euc-distance-by-iq-random-id-and-word.png",
       width = 8, height = 10, units = "in", bg = "white", dpi = 360)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
####################### calc divergence and correlate it #######################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# set up and clean
rm(list=ls());gc();source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files before
demo <- read_rds("data/raw/demo.rds")
m1.m2 <- read_rds("data/derivatives/m1m2.rds")
# get the clean transcription for data
all <- read_rds("data/derivatives/ps-vc-text-analyzed.rds") %>%
  filter(word != text) %>%
  distinct(te_id, word, text, .keep_all = T)
# get word embeddings from text package
load("data/derivatives/word-embedding-from-text-package.rda")
rm(emb.text);rm(emb.word);gc()
emb.text.m <- emb.text.m %>% distinct(te_id,word,text, .keep_all = T) # keep unique words per participant for each mini-task
# load the pairs similarity values
pairs.sim <- read_rds(paste0("data/derivatives/pairs-sim-by-word-by-participant.rds")) %>%
  filter(w1 != w2)
# get a df of pairs sim, just for consec pairs
cons.pairs <- read_rds("data/derivatives/cons-pairs.rds")
################################################################################
#####
# calculate the optimal and actual trajectories per participant for first 10 words per task/word
#####
# get summary per participant per task
tmp <- all[,1:5] %>%
  distinct(te_id, word, text, .keep_all = T) %>%
  group_by(te_id, word) %>%
  dplyr::summarise(count = n()) %>%
  pivot_wider(names_from = word, values_from = count, id_cols = te_id)
# loop over participants and select tasks/words that have enough data for you
# then, get the embeddings for these words, and calculate the hamiltonian distance, and actual distance

registerDoMC(cores = 6)
divergence <- foreach(i=1:length(unique(tmp$te_id)), .combine = rbind) %dopar% {
  id <- unique(tmp$te_id)[i]
  # get data for selected id
  df1 <- tmp %>% 
    filter(te_id == id) %>%
    pivot_longer(cols = colnames(tmp)[-1]) %>%
    # mutate(task = ifelse(nchar(name)==1,3,1))%>%filter(task==3) %>% # only look at task 3 here for divergence
    filter(value >=3) # make sure the participant has said at least 3 words
  # identify words to keep
  words.to.keep <- unique(df1$name)
  df2 <- emb.text.m %>%
    filter(te_id==id,
           word %in% words.to.keep)
  # make sure the participant has an actual response for each task/word
  if (length(words.to.keep)==0) {
    return(NULL)
  }
  # loop over words here to get traveled, and optimal distances
  word.opt <- foreach(j = 1:length(words.to.keep), .combine = rbind) %dopar% {
    w0 <- words.to.keep[j]
    
    df3 <- pairs.sim %>%
      filter(te_id ==id, word==w0) %>%
      pivot_wider(names_from = "w2", values_from = "cos_similarity", id_cols = "w1") %>%
      # select(-c(te_id, word, w_order, task)) %>%
      column_to_rownames("w1")
    df3 <- df3[,rownames(df3)]
    ######
    # get the optimal trajectory
    library(TSP)
    distance.matrix <- as.dist(1 - df3) # convert cosine similarity matrix to a distance matrix
    tsp.dist <- TSP(distance.matrix) # travelling salesman problem
    tsp.sol <- as.integer(solve_TSP(tsp.dist, method = "repetitive_nn"))
    df4 <- data.frame(w_order = tsp.sol, text = rownames(df3)[tsp.sol])
    df5 <- data.frame(w1 = df4$text[-(nrow(df4))], # make a df with optimal path order
                      w1_order = df4$w_order[-(nrow(df4))],
                      w2 = df4$text[-1],
                      w2_order = df4$w_order[-1],
                      distance = NA)
    for (k in 1:nrow(df5)) { # get optimal path distances
      df5$distance[k] <- (1-df3[df5$w1_order[k],df5$w2_order[k]])
    }
    opt.dist <- sum(df5$distance) # this is the optimal distance for this word for this participant
    act.order <- data.frame(w1 = rownames(df3)[-(nrow(df3))], # make a df with actual path order
                            w1_order = c(1:(nrow(df3)-1)),
                            w2 = colnames(df3)[-1],
                            w2_order = c(2:nrow(df3)),
                            distance = NA)
    for (m in 1:nrow(act.order)) { # get actual path distances
      act.order$distance[m] <- (1-df3[act.order$w1_order[m],act.order$w2_order[m]])
    }
    act.dist <- sum(act.order$distance) # this is the actual distance for this word for this participant
    #######
    df6 <- data.frame(te_id = id,
                      word = w0,
                      opt_dist = opt.dist,
                      act_dist = act.dist,
                      global_divergence = (act.dist - opt.dist),
                      global_divergence_normalized = (act.dist - opt.dist)/nrow(df3),
                      word_count = nrow(df3))
    return(df6)
  }
  return(word.opt)
}
# save the divergence data
write_rds(divergence, "data/derivatives/divergence-w-minimum-of-3-words-per-task.rds")
# divergence <- read_rds("data/derivatives/divergence-w-minimum-of-3-words-per-task.rds")
#####
# predict divergence
#####
# use the IQ/NIH-TB as a major predictor
# add the te_id and the task name as random variables
m122 <- inner_join(divergence, m1.m2) 
# write_rds(m122, "data/derivatives/lmer-inputs/divergence.rds")
# add the average cosine similarity per word for each participant
avg.sim <- cons.pairs %>%
  group_by(te_id, word) %>%
  dplyr::summarise(avg_cos_sim = mean(cos_similarity))
m123 <- left_join(m122, avg.sim) %>%
  left_join(demo)
####
# lmer for demo
lm <- lmerTest::lmer(global_divergence ~ word_count + age + sex + age:sex + (1|te_id) + (1|word),
                     data = m123 %>%
                       select(global_divergence, word_count, te_id, word, age, sex))
# get summ
demo.lmer <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
  as.data.frame() %>%
  rownames_to_column("fixed") %>%
  filter(fixed != "(Intercept)") %>%
  rename(Estimate = `Est.`,
         confint_min = `2.5%`,
         confint_max = `97.5%`,
         pval = p) %>%
  mutate(var = "demo")
write_rds(demo.lmer, "data/derivatives/demo-lmer/divergence.rds")
####
# lmer for IQ
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=9:35, .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  # predict divergence using the IQ/NIH measure. 
  # adding participant's ID as a random variable, and the word/mini-task
  lm <- lmerTest::lmer(global_divergence ~ xx + word_count + age + sex + age:sex + (1|te_id) + (1|word),
                       data = cbind(m123 %>% 
                                      select(global_divergence, te_id, word, word_count, age, sex) %>%
                                      mutate(global_divergence = scale(global_divergence, scale = T, center = T)[,1]),
                                    xx=m123[,i]))
  gc()
  # combine results in a df, and save
  df <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    rename(Estimate = `Est.`,
           confint_min = `2.5%`,
           confint_max = `97.5%`,
           pval = p) %>%
    mutate(var = var)
  write_rds(df, paste0("data/derivatives/divergence-lmer/", var, ".rds"))
  gc()
  return(df)
}
# combine the saved lmer results
lm.results <- foreach(i=9:ncol(m123), .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  if (file.exists(paste0("data/derivatives/divergence-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/divergence-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
lm.results <- lm.results %>% mutate(FDR = p.adjust(pval, method = "fdr"))
# write_rds(lm.results, "data/derivatives/divergence-lmer/all-lmer-results.rds", compress = "gz")
# make plot for results
p1 <- lm.results %>%
  filter(fixed=="xx") %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  mutate(var = sub("_age_corrected_standard_score", "_NIH", var),
         cat2 = ifelse(grepl("NIH", var), "NIH-TB", "IQ"),
         var= sub("_NIH", "", var),
         var = factor(var, levels = unique(var))) %>%
  ggplot(aes(x=Estimate, y=var,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free", space = "free") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting z-standardized divergence", y="",
       caption = paste0("n(samples): ", length(unique(m123$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(z-standardized_divergence ~ X + word_count + age + sex + age:sex + (1|te_id) + (1|prompt))", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables", "\n",
                        "        and word_count is how many points/words were in the path", "\n",
                        "Derivation of divergence was as follows:", "\n",
                        "    divergence = (actual_path - optimal_path) in each task/letter","\n",
                        "    optimal path is the sequence which visits each item exactly once in an order ", "\n",
                        "        that minimizes the total semantic distance traveled","\n",
                        "    actual path is by following the order the participant said","\n",
                        "    (i.e., negative divergence reflects increasignly optimal word selection)", "\n", 
                        "    Only kept participants with at least 3 words in response to task/word"))
# demographics plot
p2 <- demo.lmer %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  ggplot(aes(x=Estimate, y=fixed,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting z-standardized divergence", y="",
       caption = paste0("n(samples): ", length(unique(m123$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(z-standardized_divergence ~ word_count + age + sex + age:sex + (1|te_id) + (1|prompt))"))
patchwork::wrap_plots(p2,p1,ncol = 1,heights = c(1,5))
ggsave(filename = "figs/lmer-divergence-by-iq-and-wc-random-id-and-word.png",
       width = 9, height = 12, units = "in", bg = "white", dpi = 360)
################################################################################
######
# a plot for divergence example
######

## try a umap to dec dimensionality?
tmp <- umap::umap(preserve.seed = T, n_components = 2, emb.text.m%>%select(starts_with("Dim")))
embeddings.umap <- cbind(emb.text.m[,1:5], 
                         tmp$layout) %>%
  # rename(Dim1=7, Dim2=8, Dim3=9)
  rename(Dim1=7, Dim2=8)

#####
# define examples of small and large divergence
###
### actual plot 
### 
library(TSP)
id = "2E_086"
w0 = "A"
p.data <- all %>%
  filter(te_id == id, word == w0) %>%
  select(te_id, word, text) %>%
  rownames_to_column("order") %>%
  left_join(divergence)
p1 <- p.data %>%
  left_join(embeddings.umap %>% mutate(w1=text)) %>%
  mutate(cat = "small") %>%
  ggplot(aes(x=Dim1, y=Dim2, label = w1)) +
  geom_path(show.legend = F,  color = "grey", alpha = 0.7) + geom_text(show.legend = F) +
  labs(title = "Actual Path", 
       subtitle = paste0("Divergence = ", 
                         round(unique(as.numeric(divergence %>% filter(te_id==id, word==w0)%>%select(global_divergence))), 3)))
### optimal order
df3 <- pairs.sim %>%
  filter(te_id ==id, word==w0) %>%
  pivot_wider(names_from = "w2", values_from = "cos_similarity", id_cols = "w1") %>%
  column_to_rownames("w1"); df3 <- df3[,rownames(df3)]
distance.matrix <- as.dist(1 - df3) # convert cosine similarity matrix to a distance matrix
tsp.dist <- TSP(distance.matrix) # travelling salesman problem
tsp.sol <- as.integer(solve_TSP(tsp.dist, method = "repetitive_nn"))
df4 <- data.frame(w_order = tsp.sol, text = rownames(df3)[tsp.sol])
p2 <- left_join(df4, p.data) %>%
  left_join(embeddings.umap %>% mutate(w1=text)) %>%
  ggplot(aes(x=Dim1, y=Dim2, label = w1)) +
  geom_path(show.legend = F,  color = "grey", alpha = 0.7) + geom_text(show.legend = F) +
  labs(title = "Optimal Path")
h.d <- patchwork::wrap_plots(p1,p2,ncol = 1)
## low divergence
id = "2E_072"
w0 = "paper"
p.data <- all %>%
  filter(te_id == id, word == w0) %>%
  select(te_id, word, text) %>%
  rownames_to_column("order") %>%
  left_join(divergence)
p1 <- p.data %>%
  left_join(embeddings.umap %>% mutate(w1=text)) %>%
  mutate(cat = "small") %>%
  ggplot(aes(x=Dim1, y=Dim2, label = w1)) +
  geom_path(show.legend = F,  color = "grey", alpha = 0.7) + geom_text(show.legend = F) +
  labs(title = "Actual Path", 
       subtitle = paste0("Divergence = ", 
                         round(unique(as.numeric(divergence %>% filter(te_id==id, word==w0)%>%select(global_divergence))), 3)))
### optimal order
df3 <- pairs.sim %>%
  filter(te_id ==id, word==w0) %>%
  pivot_wider(names_from = "w2", values_from = "cos_similarity", id_cols = "w1") %>%
  column_to_rownames("w1"); df3 <- df3[,rownames(df3)]
distance.matrix <- as.dist(1 - df3) # convert cosine similarity matrix to a distance matrix
tsp.dist <- TSP(distance.matrix) # travelling salesman problem
tsp.sol <- as.integer(solve_TSP(tsp.dist, method = "repetitive_nn"))
df4 <- data.frame(w_order = tsp.sol, text = rownames(df3)[tsp.sol])
p2 <- left_join(df4, p.data) %>%
  left_join(embeddings.umap %>% mutate(w1=text)) %>%
  ggplot(aes(x=Dim1, y=Dim2, label = w1)) +
  geom_path(show.legend = F,  color = "grey", alpha = 0.7) + geom_text(show.legend = F) +
  labs(title = "Optimal Path")
l.d <- patchwork::wrap_plots(p1,p2,ncol = 1)
######
# combine both plots, and save
patchwork::wrap_plots(h.d, l.d, nrow = 1)
ggsave("figs/example-divergence-umap-2d.png", bg = "white",
       width = 8, height = 8, units = "in", dpi=360)
##################
# get a correlations heatmap between pairs for these participants
##################
# 
# low divergence
p1 <- pairs.sim %>%
  filter(te_id == "2E_072", word == "paper") %>%
  ggplot(aes(x=reorder(w1, w_order), y=reorder(w2, w_order), fill = cos_similarity, label = round(cos_similarity, 4))) +
  geom_tile() +
  geom_text(size = 4, color = "white") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) + 
  my.guides +
  labs(x="words ordered left-right based on the said order",
       y="words ordered top-bottom based on the said order",
       title = "participant: 2E_072",
       subtitle = "task: 1; prompt word: paper")
# high divergence
p2 <- pairs.sim %>%
  filter(te_id == "2E_086", word == "A") %>%
  ggplot(aes(x=reorder(w1, w_order), y=reorder(w2, w_order), fill = cos_similarity, label = round(cos_similarity, 4))) +
  geom_tile() +
  geom_text(size = 3, color = "white") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) + 
  my.guides +
  labs(x="words ordered left-right based on the said order",
       y="words ordered top-bottom based on the said order",
       title = "participant: 2E_086",
       subtitle = "task: 3; prompt letter: A")
# combine plots
patchwork::wrap_plots(p1,p2, nrow = 1)
ggsave("figs/example-pairs-cos-similarity-for-low-and-high-divergence.png", bg = "white",
       width = 14, height = 8, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
#################### calc vocabulary depth and correlate it ####################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# set up and clean
rm(list=ls());gc();source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
library(text)
library(umap)
################################################################################
################################################################################
# load files before
demo <- read_rds("data/raw/demo.rds")
m1.m2 <- read_rds("data/derivatives/m1m2.rds")
# get the clean transcription for data
all <- read_rds("data/derivatives/ps-vc-text-analyzed.rds")
# get word embeddings from text package
load("data/derivatives/word-embedding-from-text-package.rda")
rm(emb.text);rm(emb.word);gc()
emb.text.m <- emb.text.m %>% distinct(te_id,word,text, .keep_all = T) # keep unique words per participant for each mini-task
################################################################################
#####
# calculate the vocabulary depth as the total volume between embeddings/UMAP points in space
#####
#####
# get UMAP for embeddings
tmp.emb <- emb.text.m %>% distinct(text, .keep_all = T)
umap.o <- umap(tmp.emb %>% 
                 select(starts_with("Dim")) %>% 
                 as.matrix(), 
               n_components = 3, preserve.seed = T)
umap.dim <- cbind(tmp.emb[,1:5],
                  umap.o$layout %>%
                    as.data.frame() %>%
                    rename_all(.funs = function(x) paste0("Dim", sub("V", "", x))))
write_rds(umap.dim, "data/derivatives/umap-3d-from-psvc-embeddings.rds", compress = "gz")
# umap.dim <- read_rds("data/derivatives/umap-3d-from-psvc-embeddings.rds")
# 
umap.dim <- full_join(emb.text.m[,1:5], umap.dim[,c(5:8)])
registerDoMC(cores = 6)
chulls <- foreach(i=1:length(unique(emb.text.m$te_id)), .combine = rbind) %dopar% {
  id <- unique(emb.text.m$te_id)[i]
  # get data for selected id
  df <- umap.dim %>%
    filter(te_id==id, task==3) %>%
    distinct(text, .keep_all=T)
    # filter(!grepl(paste("two", "four", "fourth", "five", "fives", "six", "seven", "seventeen", collapse = "|"), text))
  vol.all <- cxhull::cxhull(df[,6:8]%>%as.matrix())$volume
  # what if we did the chull for each letter independently? possibly helping identify outliers source
  df.L <- df %>% filter(word == "L") %>% select(starts_with("Dim"))
  df.F <- df %>% filter(word == "F") %>% select(starts_with("Dim"))
  df.C <- df %>% filter(word == "C") %>% select(starts_with("Dim"))
  df.A <- df %>% filter(word == "A") %>% select(starts_with("Dim"))
  df.S <- df %>% filter(word == "S") %>% select(starts_with("Dim"))
  if (nrow(df.L)>=4) {
    vol.L <- cxhull::cxhull(df.L%>%as.matrix())$volume
    c.L = nrow(df.L)
  } else {
    vol.L = 0;c.L = 0
  }
  if (nrow(df.F)>=4) {
    vol.F <- cxhull::cxhull(df.F%>%as.matrix())$volume
    c.F = nrow(df.F)
  } else {
    vol.F = 0;c.F = 0
  }
  if (nrow(df.C)>=4) {
    vol.C <- cxhull::cxhull(df.C%>%as.matrix())$volume
    c.C = nrow(df.C)
  } else {
    vol.C = 0;c.C = 0
  }
  if (nrow(df.A)>=4) {
    vol.A <- cxhull::cxhull(df.A%>%as.matrix())$volume
    c.A = nrow(df.A)
  } else {
    vol.A = 0;c.A = 0
  }
  if (nrow(df.S)>=4) {
    vol.S <- cxhull::cxhull(df.S%>%as.matrix())$volume
    c.S = nrow(df.S)
  } else {
    vol.S = 0;c.S = 0
  }
  return(data.frame(te_id = id, vol_all = vol.all, 
                    count_all = nrow(df),
                    vol_L = vol.L, count_L = c.L,
                    vol_F = vol.F,count_F = c.F,
                    vol_C = vol.C,count_C = c.C,
                    vol_A = vol.A,count_A = c.A,
                    vol_S = vol.S,count_S = c.S))
}
write_rds(chulls, "data/derivatives/chulls.rds")
####
# check distribution
####
# outliers are there for the total volume from all tasks combined
chulls %>%
  pivot_longer(cols = starts_with("vol"), names_to = "vol_source", values_to = "vol_value") %>%
  pivot_longer(cols = starts_with("count"), names_to = "count_source", values_to = "count_value") %>%
  mutate(vol_source = sub("vol_", "", vol_source),
         count_source = sub("count_", "", count_source)) %>%
  filter(vol_source == count_source) %>%
  ggplot(aes(x=vol_value)) +
  geom_histogram() + facet_wrap("vol_source", scales = "free") +
  labs(title = "distribution convex hull volume of word embeddings")
ggsave(filename = "figs/distribution-of-vocab-depth.png",
       width = 7, height = 6, units = "in", bg = "white", dpi = 360)
############
# correlate with IQ
############
inner_join(m1.m2, chulls) %>%
  pivot_longer(cols = c(colnames(m1.m2), -ends_with("id")), names_to = "measure") %>%
  mutate(measure = sub("_age_corrected_standard_score", "_NIH", measure),
         cat2 = ifelse(grepl("NIH", measure), "NIH-TB", "IQ"),
         measure = sub("_NIH", "", measure),
         measure = factor(measure, levels = unique(measure))) %>%
  filter(!te_id %in% c("2E_091")) %>%
  ggplot(aes(x=vol_all, y=value)) +
  geom_point() +geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red") +
  facet_wrap(~measure, scales = "free") +
  labs(caption = paste0("no correction done for anything here", "\n",
                        "only dropped 1 outlier because of their calculated vocabulary depth"))
ggsave(filename = "figs/corr-iq-nih-vocab-depth.png",
       width = 12, height = 10, units = "in", bg = "white", dpi = 360)
######################################
# probably need to correct for randomeness coming from task
######################################
m123 <- inner_join(m1.m2, 
                   chulls %>%
                     pivot_longer(cols = starts_with("vol"), names_to = "vol_source", values_to = "vol_value") %>%
                     pivot_longer(cols = starts_with("count"), names_to = "count_source", values_to = "count_value") %>%
                     mutate(vol_source = sub("vol_", "", vol_source),
                            count_source = sub("count_", "", count_source)) %>%
                     filter(vol_source == count_source)) %>%
  filter(!te_id %in% c("2E_091")) %>%
  # filter(vol_value<40) %>%
  filter(vol_source != "all") %>%
  left_join(demo)
# write_rds(m123, "data/derivatives/lmer-inputs/chulls.rds")
####
# glm for demo
lm <- glm(vol_value ~ count_value + vol_source + age + sex + age:sex,
          data = m123 %>%
            select(vol_value, te_id,age, sex, count_value, vol_source))
# get summ
demo.lmer <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
  as.data.frame() %>%
  rownames_to_column("fixed") %>%
  filter(fixed != "(Intercept)") %>%
  rename(Estimate = `Est.`,
         confint_min = `2.5%`,
         confint_max = `97.5%`,
         pval = p) %>%
  mutate(var = "demo")
write_rds(demo.lmer, "data/derivatives/demo-lmer/chulls.rds")
####
# glm for IQ
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  lm <- glm(vol_value ~ xx + count_value + vol_source + age + sex + age:sex,
            data = cbind(m123 %>% 
                           select(vol_value, vol_source, te_id, count_value, age, sex) %>%
                           mutate(vol_value = scale(vol_value, scale = T, center = T)[,1]),
                         xx=m123[,i]) %>%
              rename(xx = 7))
  gc()
  # combine results in a df, and save
  df <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    rename(Estimate = `Est.`,
           confint_min = `2.5%`,
           confint_max = `97.5%`,
           pval = p) %>%
    mutate(var = var)
  write_rds(df, paste0("data/derivatives/chulls-lmer/glm-all-union-", var, ".rds"))
  gc()
  return(df)
}
# combine the saved lmer results
lm.results <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  if (file.exists(paste0("data/derivatives/chulls-lmer/glm-all-union-", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/chulls-lmer/glm-all-union-", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
lm.results <- lm.results %>% mutate(FDR = p.adjust(pval, method = "fdr"))
# write_rds(lm.results, "data/derivatives/chulls-lmer/all-lmer-results.rds", compress = "gz")
# make plot for results
p1 <- lm.results %>%
  filter(fixed=="xx") %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  mutate(var = sub("_age_corrected_standard_score", "_NIH", var),
         cat2 = ifelse(grepl("NIH", var), "NIH-TB", "IQ"),
         var= sub("_NIH", "", var),
         var = factor(var, levels = unique(var))) %>%
  ggplot(aes(x=Estimate, y=var,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free", space = "free") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting z-standardized vocabulary depth", y="",
       caption = paste0("n(samples): ", length(unique(m123$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    glm(z-standardized_vocab-depth ~ X + word_count + prompt + age + sex + age:sex)", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables", "\n",
                        "        and word_count is how many points/words in participant's response in this task", "\n",
                        "Derivation of vocabulary depth was as follows:", "\n",
                        "    vocabulary_depth = volume enclosed between 3d semantic space of words said","\n",
                        "    765 word embeddings were derived from 'text' package and then", "\n",
                        "        UMAP was utilized to reduce dimensions for 3D","\n",
                        "    Only kept participants with at least 4 words in response to task/word"))
# demographics plot
p2 <- demo.lmer %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  ggplot(aes(x=Estimate, y=fixed,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting z-standardized vocabulary depth", y="",
       caption = paste0("n(samples): ", length(unique(m123$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    glm(z-standardized_vocab-depth ~ word_count + prompt + age + sex + age:sex)"))
patchwork::wrap_plots(p2,p1,ncol = 1,heights = c(1,5))
ggsave(filename = "figs/glm-vocab-depth-by-iq-and-wc-and-word.png",
       width = 9, height = 12, units = "in", bg = "white", dpi = 360)
##############
# plot volume of some chulls?
##############
# vol for 2E_089
tt <- umap.dim %>%
  filter(te_id == "2E_089", nchar(word)==1)
# vol for 2E_089
ll <- umap.dim %>%
  filter(te_id == "2E_053", nchar(word)==1)
# make a mesh 3d for both participants selected together
data <- rbind(tt, ll) %>% rename(ID=te_id) %>%
  mutate(ID = ifelse(ID=="2E_089", "A", "B"))
# Create mesh3d plots for each ID
library(plotly)
plot <- plot_ly()
# Plot for ID A
plot <- add_trace(
  plot,
  x = ~data$Dim1[data$ID == "A"],
  y = ~data$Dim2[data$ID == "A"],
  z = ~data$Dim3[data$ID == "A"],
  type = "mesh3d",
  opacity = 0.5, # set opacity
  color = I("blue"), # set color
  name = "ID A"
)
# Plot for ID B
plot <- add_trace(
  plot,
  x = ~data$Dim1[data$ID == "B"],
  y = ~data$Dim2[data$ID == "B"],
  z = ~data$Dim3[data$ID == "B"],
  type = "mesh3d",
  opacity = 0.5, # set opacity
  color = I("red"), # set color
  name = "ID B"
)
# Customize layout
plot <- layout(
  plot,
  title = "vocabulary depth/volume example",
  scene = list(
    xaxis = list(title = "Dim1"),
    yaxis = list(title = "Dim2"),
    zaxis = list(title = "Dim3")
  )
)
# Display the plot
plot
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
#################### identify semantic categories and jumping ##################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# set up and clean
rm(list=ls());gc();source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files before
demo <- read_rds("data/raw/demo.rds")
m1.m2 <- read_rds("data/derivatives/m1m2.rds")
# get the clean transcription for data
all <- read_rds("data/derivatives/ps-vc-text-clean.rds")
# get word embeddings from text package
load("data/derivatives/word-embedding-from-text-package.rda")
rm(emb.text);rm(emb.word);gc()
emb.text.m <- emb.text.m %>% 
  filter(!(te_id %in% c("2E_036", "2E_033", "2E_040"))) %>%
  distinct(te_id,word,text, .keep_all = T) # keep unique words per participant for each mini-task
# load the pairs similarity values
pairs.sim <- read_rds(paste0("data/derivatives/pairs-sim-by-word-by-participant.rds")) %>%
  filter(w1 != w2)
# get a df of pairs sim, just for consec pairs
cons.pairs <- read_rds("data/derivatives/cons-pairs.rds")
################################################################################
#####
# identify word categories for all words said
#####
#####
tmp.emb <- emb.text.m %>% 
  filter(task == 1) %>%
  distinct(text, .keep_all = T) %>%
  select(-c(1:4))
######
# try doing archetypes, instead of clustering for these words
######
library(archetypes)
aa <- stepArchetypes(tmp.emb[,-1] %>% as.matrix(), k = 10, nrep = 4, verbose = T)
write_rds(aa, "data/derivatives/embeddings-archetypes-10.rds")
aa <- read_rds("data/derivatives/embeddings-archetypes-10.rds")
a10 <- bestModel(aa)
a10.meta <- cbind(word = tmp.emb$text, 
                  a10$alphas) %>%
  as.data.frame() %>%
  mutate_at(.vars = vars(starts_with("V")), function(x) as.numeric(x))
colnames(a10.meta) <- c("word", paste0("A", c(1:10)))
a10.long <- a10.meta %>%
  pivot_longer(cols = colnames(a10.meta)[2:11], names_to = "archetype") %>%
  arrange(archetype)
t <- a10.long %>%
  group_by(archetype) %>%
  slice_max(order_by = value, n = 10) %>%
  arrange(word)
a10.long %>%
  left_join(t %>% select(word, archetype2 = archetype)) %>%
  filter(word %in% t$word) %>%
  mutate(archetype = factor(archetype, levels = paste0("A", c(1:10))),
         archetype2 = factor(archetype2, levels = paste0("A", c(1:10)))) %>%
  ggplot(aes(x=archetype, y= reorder(word, desc(archetype2)), fill = value, label = round(value, 3))) +
  geom_tile() + 
  geom_text(size=2.5, color = six.colors[6]) +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  my.guides +
  labs(y = "word")
ggsave("figs/word-loadings-in-archetypes.png", bg = "white",
       width = 8, height = 14, units = "in", dpi = 360)
#####
# category assignment
#####
# I decided to assign a category for each word from 1:10, based on the highest archetype value it has
words.categorized <- a10.long %>%
  group_by(word) %>%
  slice_max(order_by = value, n = 1) %>%
  ungroup()
#####
all.cat <- left_join(all[,1:7],
                     words.categorized %>% rename(text=word)) %>%
  filter(task==1) %>%
  filter(!is.na(archetype))
# save
write_csv(all.cat, "data/derivatives/words-categorized-by-archetype.csv")
# all.cat <- read_csv("data/derivatives/words-categorized-by-archetype.csv")
####
################################################################################
#####
# attempt to get a meta for visits to groups, and mean lifetime
#####
registerDoMC(cores = 6)
# archetypes.ls <- paste0("A", c(1:10))
# loop over participants
community.meta <- foreach(i = 1:length(unique(all.cat$te_id)), .combine = rbind) %dopar% {
  # i=1
  id <- unique(all.cat$te_id)[i]
  id.df <- all.cat %>%
    filter(te_id == id)
  # loop over tasks
  tasks.meta <- foreach(j = 1:length(unique(id.df$word)), .combine = rbind) %dopar% {
    # j=1
    prompt <- unique(id.df$word)[j]
    # get how many archetypes the participant visited
    id.df.2 <- id.df %>%
      filter(word == prompt) %>%
      rownames_to_column("index") %>%
      mutate(index = as.numeric(index))
    archetypes.ls <- unique(id.df.2$archetype)
    # define the archetypes df that has all needed info
    arch.meta <- foreach (k = 1:length(archetypes.ls), .combine = rbind) %dopar% {
      # k=1
      a <- archetypes.ls[k]
      # get the words in this archetype
      id.df.3 <- id.df.2 %>%
        filter(archetype == a)
      a.meta <- data.frame(te_id = id,
                           word = prompt) %>%
        mutate(archetype = a,
               archetype_word_count = nrow(id.df.3))
      visit_index <- 1
      # loop over responses
      for (l in 1:nrow(id.df.3)) {
        # l=1
        if (l == 1) {
          a.meta <- a.meta %>%
            mutate(visit_number = visit_index,
                   visit_start_word = id.df.3$text[l],
                   visit_start_time = id.df.3$start[l],
                   visit_last_word = id.df.3$text[l],
                   visit_end_time = id.df.3$end[l])
        } else if (id.df.3$index[l] == (id.df.3$index[l-1]+1)) {
          a.meta$visit_last_word[visit_index] <-id.df.3$text[l]
          a.meta$visit_end_time[visit_index] = id.df.3$end[l]
        } else if (id.df.3$index[l] != (id.df.3$index[l-1]+1)) {
          visit_index <- visit_index+1
          a.meta <- full_join(a.meta,
                          data.frame(te_id = id,
                                     word = prompt,
                                     archetype = a,
                                     archetype_word_count = nrow(id.df.3),
                                     visit_number = c(a.meta$visit_number, visit_index),
                                     visit_start_word = c(a.meta$visit_start_word, id.df.3$text[l]),
                                     visit_start_time = c(a.meta$visit_start_time, id.df.3$start[l]),
                                     visit_last_word = c(a.meta$visit_last_word, id.df.3$text[l]),
                                     visit_end_time = c(a.meta$visit_end_time, id.df.3$end[l])))
        }
      }
      return(a.meta)
    }
    return(arch.meta)
  }
  return(tasks.meta)
}
community.meta <- community.meta %>%
  mutate(lifetime = visit_end_time - visit_start_time)
write_rds(community.meta, "data/derivatives/community-data.rds")
# community.meta <- read_rds("data/derivatives/community-data.rds")
################################################################################
# summarize
t = community.meta %>%
  group_by(te_id, word) %>%
  dplyr::summarise(count = n())
# good example 2E_089, corn
################################################################################
# plot example
all.cat %>%
  drop_na(start) %>%
    filter(start>0,
           te_id == "2E_089")%>%
    mutate(archetype = factor(archetype, levels = paste0("A", c(1:10)))) %>%
    ggplot(aes(x=word, y=start, fill=archetype))+
    geom_tile(aes(height = (end - start)))+
    scale_fill_manual(values = ten.colors) +
    coord_flip() +
  labs(x="time", y="prompt",
       title = paste0("participant: 2E_089"))
ggsave("figs/example-community-switches-and-time.png", bg = "white",
       width = 10, height = 7, units = "in", dpi = 360)
################################################################################
################################################################################
# get average lifetime per archetype per prompt per participants
avg.lifetime <- community.meta %>%
  filter(lifetime>0) %>%
  group_by(te_id, word, archetype) %>%
  dplyr::summarise(mean_lifetime = mean(lifetime))
# combine with IQ
m123 <- inner_join(m1.m2, avg.lifetime) %>%
  left_join(demo)
# write_rds(m123, "data/derivatives/lmer-inputs/comm-lifetime.rds")
####
# lmer for demo
lm <- lmerTest::lmer(mean_lifetime ~ age + sex + age:sex + (1|te_id) + (1|archetype)+ (1|word),
                     data = m123 %>%
                       select(mean_lifetime, te_id, word, age, sex, archetype))
# get summ
demo.lmer <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
  as.data.frame() %>%
  rownames_to_column("fixed") %>%
  filter(fixed != "(Intercept)") %>%
  rename(Estimate = `Est.`,
         confint_min = `2.5%`,
         confint_max = `97.5%`,
         pval = p) %>%
  mutate(var = "demo")
write_rds(demo.lmer, "data/derivatives/demo-lmer/comm-lifetime.rds")
#####
# predict lifetime using IQ, random id, archetype, prompt
#####
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  # predict mean lifetime using the IQ/NIH measure. 
  # adding participant's ID as a random variable, archetype, and prompt
  lm <- lmerTest::lmer(mean_lifetime ~ xx +  +age + sex + age:sex + (1|te_id) + (1|archetype) + (1|word),
                       data = cbind(m123 %>% 
                                      select(archetype, word, te_id, mean_lifetime, age, sex) %>%
                                      mutate(mean_lifetime = log2(mean_lifetime)),
                                    xx=m123[,i]) %>%
                         rename(xx = 7))
  gc()
  # combine results in a df, and save
  df <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    rename(Estimate = `Est.`,
           confint_min = `2.5%`,
           confint_max = `97.5%`,
           pval = p) %>%
    mutate(var = var)
  write_rds(df, paste0("data/derivatives/community-lifetime-lmer/", var, ".rds"))
  gc()
  return(df)
}
# combine the saved lmer results
lm.results <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  if (file.exists(paste0("data/derivatives/community-lifetime-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/community-lifetime-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
lm.results <- lm.results %>% mutate(FDR = p.adjust(pval, method = "fdr"))
# make plot for results
p1 <- lm.results %>%
  filter(fixed=="xx") %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  mutate(var = sub("_age_corrected_standard_score", "_NIH", var),
         cat2 = ifelse(grepl("NIH", var), "NIH-TB", "IQ"),
         var= sub("_NIH", "", var),
         var = factor(var, levels = unique(var))) %>%
  ggplot(aes(x=Estimate, y=var,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free", space = "free") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting log2(mean lifetime spent per archetype)", y="",
       caption = paste0("n(samples): ", length(unique(m123$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(log2(mean_lifetime) ~ X + age + sex + age:sex + (1|te_id) + (1|prompt) + (1|archetype))", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables", "\n"))
# demographics plot
p2 <- demo.lmer %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  ggplot(aes(x=Estimate, y=fixed,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting log2(mean lifetime spent per archetype)", y="",
       caption = paste0("n(samples): ", length(unique(m123$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(log2(mean_lifetime) ~ X + age + sex + age:sex + (1|te_id) + (1|prompt) + (1|archetype))"))
patchwork::wrap_plots(p2,p1,ncol = 1,heights = c(1,5))
ggsave(filename = "figs/lmer-community-lifetime-by-iq-random-id-word-and-archetype.png",
       width = 9, height = 12, units = "in", bg = "white", dpi = 360)
##############
################################################################################
################################################################################
################################################################################
################################################################################
# get number of returns per archetype per prompt per participants
avg.returns <- community.meta %>%
  filter(lifetime>0) %>%
  group_by(te_id, word, archetype) %>%
  slice_max(visit_number, n = 1) %>%
  mutate(switches = visit_number-1)
# combine with IQ
m124 <- inner_join(m1.m2, avg.returns) %>%
  left_join(demo)
# write_rds(m124, "data/derivatives/lmer-inputs/comm-returns.rds")
####
# lmer for demo
lm <- lmerTest::lmer(switches ~ age + sex + age:sex + (1|te_id) + (1|archetype) + (1|word),
                     data = m124 %>%
                       select(switches, te_id, word, age, sex, archetype))
# get summ
demo.lmer <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
  as.data.frame() %>%
  rownames_to_column("fixed") %>%
  filter(fixed != "(Intercept)") %>%
  rename(Estimate = `Est.`,
         confint_min = `2.5%`,
         confint_max = `97.5%`,
         pval = p) %>%
  mutate(var = "demo")
write_rds(demo.lmer, "data/derivatives/demo-lmer/comm-returns.rds")
#####
# predict lifetime using IQ, random id, archetype, prompt
#####
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m124)[i]
  # predict mean lifetime using the IQ/NIH measure. 
  # adding participant's ID as a random variable, archetype, and prompt
  lm <- lmerTest::lmer(switches ~ xx + age + sex + age:sex +(1|te_id) + (1|archetype) + (1|word),
                       data = cbind(m124 %>% 
                                      select(archetype, word, te_id, switches, age, sex) %>%
                                      mutate(switches = scale(switches, scale = T, center = T)[,1]),
                                    xx=m124[,i]) %>%
                         rename(xx = 7))
  gc()
  # combine results in a df, and save
  df <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    rename(Estimate = `Est.`,
           confint_min = `2.5%`,
           confint_max = `97.5%`,
           pval = p) %>%
    mutate(var = var)
  write_rds(df, paste0("data/derivatives/community-returns-lmer/", var, ".rds"))
  gc()
  return(df)
}
# combine the saved lmer results
lm.results <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m124)[i]
  if (file.exists(paste0("data/derivatives/community-returns-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/community-returns-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
lm.results <- lm.results %>% mutate(FDR = p.adjust(pval, method = "fdr"))
# make plot for results
p1 <- lm.results %>%
  filter(fixed=="xx") %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  mutate(var = sub("_age_corrected_standard_score", "_NIH", var),
         cat2 = ifelse(grepl("NIH", var), "NIH-TB", "IQ"),
         var= sub("_NIH", "", var),
         var = factor(var, levels = unique(var))) %>%
  ggplot(aes(x=Estimate, y=var,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free", space = "free") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting z-scaled(number of returns per archetype)", y="",
       caption = paste0("n(samples): ", length(unique(m124$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(z_scaled(number of returns) ~ X + age + sex + age:sex + (1|te_id) + (1|prompt) + (1|archetype))", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables", "\n"))
# demographics plot
p2 <- demo.lmer %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  ggplot(aes(x=Estimate, y=fixed,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate for predicting z-scaled(number of returns per archetype)", y="",
       caption = paste0("n(samples): ", length(unique(m124$te_id)), "\n",
                        "the estimates are derived from the model below:", "\n",
                        "    lmer(z_scaled(number of returns) ~ age + sex + age:sex + (1|te_id) + (1|prompt) + (1|archetype))"))
patchwork::wrap_plots(p2,p1,ncol = 1,heights = c(1,5))
ggsave(filename = "figs/lmer-community-returns-by-iq-random-id-word-and-archetype.png",
       width = 9, height = 12, units = "in", bg = "white", dpi = 360)
##############


####


################################################################################
################################################################################
################################################################################
###################### combine all lmer results in 1 plot ######################
################################################################################
################################################################################
################################################################################
# set up and clean
rm(list=ls());gc();source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files before
m1.m2 <- read_rds("data/derivatives/m1m2.rds") 
#####
# pairs lmer
#####
pairs <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m1.m2)[i]
  if (file.exists(paste0("data/derivatives/pairs-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/pairs-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
pairs <- pairs %>% 
  mutate(FDR = p.adjust(pval, method = "fdr"), 
         source = "cos_similarity for consec. pairs")
pairs.demo <- read_rds("data/derivatives/demo-lmer/pairs-sim.rds") %>%
  mutate(source = "cos_sim for consec. pairs")
#####
# euc lmer
#####
euc <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m1.m2)[i]
  if (file.exists(paste0("data/derivatives/euc-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/euc-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
euc <- euc %>% 
  mutate(FDR = p.adjust(pval, method = "fdr"), 
         source = "normalized full Euclidean distance")
euc.demo <- read_rds("data/derivatives/demo-lmer/euc.rds") %>%
  mutate(source = "normalized full Euclidean distance")
#####
# vocab depth lmer
#####
v.depth <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m1.m2)[i]
  if (file.exists(paste0("data/derivatives/chulls-lmer/glm-all-union-", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/chulls-lmer/glm-all-union-", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
v.depth <- v.depth %>% 
  mutate(FDR = p.adjust(pval, method = "fdr"), 
         source = "vocabulary depth")
v.depth.demo <- read_rds("data/derivatives/demo-lmer/chulls.rds") %>%
  mutate(source = "vocabulary depth")
#####
# divergence lmer
#####
divergence <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m1.m2)[i]
  if (file.exists(paste0("data/derivatives/divergence-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/divergence-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
divergence <- divergence %>% 
  mutate(FDR = p.adjust(pval, method = "fdr"), 
         source = "divergence")
divergence.demo <- read_rds("data/derivatives/demo-lmer/divergence.rds") %>%
  mutate(source = "divergence")
#####
# community return lmer
#####
comm.returns <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m1.m2)[i]
  if (file.exists(paste0("data/derivatives/community-returns-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/community-returns-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
comm.returns <- comm.returns %>% 
  mutate(FDR = p.adjust(pval, method = "fdr"), 
         source = "community returns")
comm.returns.demo <- read_rds("data/derivatives/demo-lmer/comm-returns.rds") %>%
  mutate(source = "community returns")
#####
# community lifetime lmer
#####
comm.lifetime <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m1.m2)[i]
  if (file.exists(paste0("data/derivatives/community-lifetime-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/community-lifetime-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
comm.lifetime <- comm.lifetime %>% 
  mutate(FDR = p.adjust(pval, method = "fdr"), 
         source = "community lifetime")
comm.lifetime.demo <- read_rds("data/derivatives/demo-lmer/comm-lifetime.rds") %>%
  mutate(source = "community lifetime")
###
################################################################################
################################################################################
# combine all in one major df
all <- rbind(comm.lifetime,
             comm.returns,
             euc,
             divergence,
             pairs,
             v.depth %>% mutate(`d.f.` = NA))

all.demo <- rbind(comm.lifetime.demo,
                  comm.returns.demo,
                  euc.demo,
                  divergence.demo,
                  pairs.demo,
                  v.depth.demo %>% mutate(`d.f.` = NA))
# plot
p1 <- all %>%
  filter(fixed=="xx") %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  mutate(var = sub("_age_corrected_standard_score", "_NIH", var),
         cat2 = ifelse(grepl("NIH", var), "NIH-TB", "IQ"),
         var= sub("_NIH", "", var),
         var = factor(var, levels = unique(var))) %>%
  ggplot(aes(x=Estimate, y=var,)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  ggh4x::facet_grid2(rows = vars(cat2), cols = vars(source), scales = "free") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate", y="")
p1
p2 <- all.demo %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  ggplot(aes(x=Estimate, y=fixed)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  ggh4x::facet_grid2(cols = vars(source), scales = "free") +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey")) +
  labs(x = "Estimate", y="")
p2
patchwork::wrap_plots(p2,p1, ncol = 1, heights = c(1,5))
ggsave(filename = "figs/lmer-all-metrics-by-iq.png",
       width = 16, height = 14, units = "in", bg = "white", dpi = 360)
#

################################################################################
################################################################################
################################################################################
########################## CCA for all metrics with IQ #########################
################################################################################
################################################################################
################################################################################
# set up and clean
rm(list=ls());gc();source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files before
m1.m2 <- read_rds("data/derivatives/m1m2.rds") 
demo <- read_rds("data/raw/demo.rds")
#####
pairs <- read_rds("data/derivatives/lmer-inputs/cons-pairs.rds") %>% left_join(demo)
euc <- read_rds("data/derivatives/lmer-inputs/euc.rds") %>% left_join(demo)
divergence <- read_rds("data/derivatives/lmer-inputs/divergence.rds") %>% left_join(demo)
voc.depth <- read_rds("data/derivatives/lmer-inputs/chulls.rds") %>% left_join(demo)
comm.lifetime <- read_rds("data/derivatives/lmer-inputs/comm-lifetime.rds") %>% left_join(demo)
comm.returns <- read_rds("data/derivatives/lmer-inputs/comm-returns.rds") %>% left_join(demo)
#############
# maybe get rid of the random effects and get mean of residuals per participant?
library(lme4)
# pairs res
pairs.res <- cbind(pairs %>% select(te_id),
                   res = residuals(glm(cos_similarity ~ word + age + sex + age:sex, 
                                       data = pairs))) %>%
  group_by(te_id) %>%
  dplyr::summarise(cos_sim = mean(res)) %>%
  ungroup() %>%
  mutate(cos_sim = scale(cos_sim, scale = T, center = T)[,1])
# euc res
euc.res <- cbind(euc %>% select(te_id),
                 res = residuals(glm(full_euc_dist_normalized ~ word + age + sex + age:sex, 
                                     data = euc))) %>%
  group_by(te_id) %>%
  dplyr::summarise(euc = mean(res)) %>%
  ungroup() %>%
  mutate(euc = scale(euc, scale = T, center = T)[,1])
# voc depth res
vdepth.res <- cbind(voc.depth %>% select(te_id),
                    res = residuals(glm(vol_value ~ count_source + count_value + age + sex + age:sex, 
                                        data = voc.depth))) %>%
  group_by(te_id) %>%
  dplyr::summarise(v_depth = mean(res)) %>%
  ungroup() %>%
  mutate(v_depth = scale(v_depth, scale = T, center = T)[,1])
# divergence res
div.res <- cbind(divergence %>% select(te_id),
                 res = residuals(glm(global_divergence_normalized ~ word + word_count + age + sex + age:sex, 
                                     data = divergence))) %>%
  group_by(te_id) %>%
  dplyr::summarise(div = mean(res)) %>%
  ungroup() %>%
  mutate(div = scale(div, scale = T, center = T)[,1])
# comm returns res
comm_ret.res <- cbind(comm.returns %>% select(te_id),
                      res = residuals(glm(switches ~ word + archetype + age + sex + age:sex, 
                                          data = comm.returns))) %>%
  group_by(te_id) %>%
  dplyr::summarise(comm_ret = mean(res)) %>%
  ungroup() %>%
  mutate(comm_ret = scale(comm_ret, scale = T, center = T)[,1])
# comm lifetime res
comm_life.res <- cbind(comm.lifetime %>% select(te_id),
                       res = residuals(glm(mean_lifetime ~ word + archetype + age + sex + age:sex, 
                                           data = comm.lifetime))) %>%
  group_by(te_id) %>%
  dplyr::summarise(comm_life = mean(res)) %>%
  ungroup() %>%
  mutate(comm_life = scale(comm_life, scale = T, center = T)[,1])
###
# combine all in 1 big df
all.metrics <- inner_join(pairs.res,
                  inner_join(euc.res,
                             full_join(vdepth.res,
                                        inner_join(div.res,
                                                   inner_join(comm_life.res,
                                                              comm_ret.res)))))
all <- inner_join(m1.m2, all.metrics)
################################################################################
# correlation between my metrics
corr.table(all %>% select(colnames(all.metrics)[1:4], -te_id),
           all %>% select(colnames(all.metrics)[5:7])) %>%
  filter(V1!=V2) %>%
  ggplot(aes(x=V1, y=V2, fill=r, label = ifelse(pval<0.05, "*", ""))) +
  geom_tile()+geom_text()+
  redblack.col.gradient+my.guides+
  labs(x="", y="")
ggsave("figs/corr-between-embeddings-language-metrics.png", bg = "white",
       width = 5, height = 5, units = "in", dpi = 360)
################################################################################
################################################################################
# do CCA for two blocks: IQ, and measured language metrics
library(RGCCA)
c1 <- all %>% 
  select(colnames(m1.m2), -ends_with("id"))
c2 <- all %>% 
  select(colnames(all.metrics), -te_id)
cca.cv <- rgcca_cv(blocks = list(iq = c1,
                                 language = c2),
                   ncomp = 6, verbose = T, method = "sgcca",
                   response = 1, validation = "kfold", k = 10, n_cores = 1)
all.cca <- rgcca(cca.cv)
######
# extract loadings
cca.iq <- all.cca$Y$iq
cca.lang <- all.cca$Y$language
# make plots that correlate the loadings with the actual values
m11 <- cbind(c1,
             cca.iq)
p1 <- corr.table(m11 %>% select(starts_with("comp")),
           m11 %>% select(colnames(c1))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(grepl("comp[0-9]", V1),
         V2 %in% colnames(c1)) %>%
  mutate(V2 = sub("_age_corrected_standard_score", "_NIH", V2),
         cat2 = ifelse(grepl("NIH", V2), "NIH-TB", "IQ"),
         V2 = sub("_NIH", "", V2),
         comp = parse_number(V1),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=reorder(V1, comp), y=V2, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free") +
  labs(y = "", x = "", 
       caption = paste0("n(samples): ", nrow(m11),"\n")) +
  my.guides

m12 <- cbind(c2,cca.lang)
p2 <- corr.table(m12 %>% select(starts_with("comp")),
                 m12 %>% select(colnames(c2))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(grepl("comp[0-9]", V1),
         V2 %in% colnames(c2)) %>%
  mutate(comp = parse_number(V1),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=reorder(V1, comp), y=V2, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  # ggh4x::facet_grid2(rows = vars(cat2), scales = "free") +
  labs(y = "", x = "", 
       caption = paste0("n(samples): ", nrow(m12),"\n")) +
  my.guides
patchwork::wrap_plots(p1,p2, ncol = 1, heights = c(3,1))
ggsave("figs/cca-iq-lang-embeddings-metrics.png", bg = "white",
       width = 6, height = 10, units = "in", dpi=360)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
