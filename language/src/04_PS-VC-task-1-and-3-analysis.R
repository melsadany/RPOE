################################################################################
#            analyzing PS_VC 1&3 and its correlation with other tests          #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
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
  select(te_id=ID, task, task_order, word, text) %>%
  filter(word != text) %>% #drop the words that are exactly the same as the prompt word
  distinct() # only keep unique words and drop repeated by the same participant in the same task/word
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
################################################################################
# histogram of cosine similarity distribution for pairs
# get a df of pairs sim, just for consec pairs
w2_order <- pairs.sim %>% 
  select(1,2,w2_order=w_order,w2=w1) %>% distinct()
cons.pairs <- pairs.sim %>%
  left_join(w2_order, relationship = "many-to-many") %>%
  mutate(consec = ifelse(w2_order==w_order+1, T, F)) %>%
  filter(consec==T)
pairs.sim %>%
  left_join(w2_order, relationship = "many-to-many") %>%
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
  drop_na()
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=11:ncol(m125), .combine = rbind) %dopar% {
  var <- colnames(m125)[i]
  # predict cosine similarity of the pair using the IQ/NIH measure. 
  # adding participant's ID as a random variable, and the word/mini-task
  lm <- lmerTest::lmer(cos_similarity ~ xx + (1|te_id) + (1|word),
                       data = cbind(m125 %>% 
                                      select(cos_similarity, te_id, word),
                                    xx=m125[,i]) %>%
                         rename(xx=4))
  gc()
  # combine results in a df, and save
  df <- coef(summary(lm)) %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    mutate(confint_min = Estimate - `Std. Error`,
           confint_max = Estimate + `Std. Error`,
           pval = `Pr(>|t|)`,
           var = var)
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
lm.results %>%
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
                        "    lmer(cos_similarity ~ X + (1|te_id) + (1|word))", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables"))
ggsave(filename = "figs/lmer-cos-sim-consec-pairs-by-iq-random-id-and-word.png",
       width = 6, height = 8, units = "in", bg = "white", dpi = 360)
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
# make the euc in wide format
t1 <- euc %>%
  pivot_wider(names_from = word, values_from = full_euc_dist_normalized, id_cols = te_id)
#####
# predict normalized Euclidean distance traveled 
#####
# use the IQ/NIH-TB as a major predictor
# add the te_id and the task name as random variables
m123 <- inner_join(euc, m1.m2) 
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=6:ncol(m123), .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  # predict euclidean distance using the IQ/NIH measure. 
  # adding participant's ID as a random variable, and the word/mini-task
  lm <- lmerTest::lmer(full_euc_dist_normalized ~ xx + (1|te_id) + (1|word),
                       data = cbind(m123 %>% 
                                      select(full_euc_dist_normalized, te_id, word),
                                    xx=m123[,i]) %>%
                         rename(xx=4))
  gc()
  # combine results in a df, and save
  df <- coef(summary(lm)) %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    mutate(confint_min = Estimate - `Std. Error`,
           confint_max = Estimate + `Std. Error`,
           pval = `Pr(>|t|)`,
           var = var)
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
lm.results %>%
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
                        "    lmer(normalized_euc_distance ~ X + (1|te_id) + (1|word))", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables", "\n",
                        "Derivation of Euclidean distance was as follows:", "\n",
                        "    distance = traveled distance by participant in each task/word","\n",
                        "    (i.e., sum of distance between consecuitive pairs)","\n",
                        "    Normalized by word count said by a participant in this task/word","\n",
                        "    Only kept participants with at least 2 words in response to task/word"))
ggsave(filename = "figs/lmer-full-euc-distance-by-iq-random-id-and-word.png",
       width = 7, height = 8, units = "in", bg = "white", dpi = 360)
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
pairs.sim.1 <- read_rds(paste0("data/derivatives/pairs-sim-by-word-by-participant-task-1.rds"))
pairs.sim.3 <- read_rds(paste0("data/derivatives/pairs-sim-by-word-by-participant-task-3.rds"))
pairs.sim <- rbind(pairs.sim.1 %>% mutate(task=1), pairs.sim.3 %>% mutate(task=3)) %>%
  filter(w1 != w2)
rm(pairs.sim.1);rm(pairs.sim.3);gc()
# get a df of pairs sim, just for consec pairs
w2_order <- pairs.sim %>% 
  select(1,2,w2_order=w_order,w2=w1) %>% distinct()
cons.pairs <- pairs.sim %>%
  left_join(w2_order, relationship = "many-to-many") %>%
  mutate(consec = ifelse(w2_order==w_order+1, T, F)) %>%
  filter(consec==T)
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
# add the average cosine similarity per word for each participant
avg.sim <- cons.pairs %>%
  group_by(te_id, word) %>%
  dplyr::summarise(avg_cos_sim = mean(cos_similarity))
m123 <- left_join(m122, avg.sim)
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=9:35, .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  # predict divergence using the IQ/NIH measure. 
  # adding participant's ID as a random variable, and the word/mini-task
  lm <- lmerTest::lmer(global_divergence ~ xx + word_count +(1|te_id) + (1|word),
                       data = cbind(m123 %>% 
                                      select(global_divergence, te_id, word, word_count, avg_cos_sim) %>%
                                      mutate(global_divergence = scale(global_divergence, scale = T, center = T)[,1]),
                                    xx=m123[,i]))
  gc()
  # combine results in a df, and save
  df <- coef(summary(lm)) %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    mutate(confint_min = Estimate - `Std. Error`,
           confint_max = Estimate + `Std. Error`,
           pval = `Pr(>|t|)`,
           var = var)
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
lm.results %>%
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
                        "    lmer(z-standardized_divergence ~ X + word_count + (1|te_id) + (1|word))", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables", "\n",
                        "        and word_count is how many points/words were in the path", "\n",
                        "Derivation of divergence was as follows:", "\n",
                        "    divergence = (actual_path - optimal_path) in each task/letter","\n",
                        "    optimal path is the sequence which visits each item exactly once in an order ", "\n",
                        "        that minimizes the total semantic distance traveled","\n",
                        "    actual path is by following the order the participant said","\n",
                        "    (i.e., negative divergence reflects increasignly optimal word selection)", "\n", 
                        "    Only kept participants with at least 3 words in response to task/word"))
ggsave(filename = "figs/lmer-divergence-by-iq-and-wc-random-id-and-word.png",
       width = 7, height = 8, units = "in", bg = "white", dpi = 360)
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
  filter(!te_id %in% c("2E_040", "2E_081", "2E_057")) %>%
  ggplot(aes(x=vol_all, y=value)) +
  geom_point() +geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red") +
  facet_wrap(~measure, scales = "free") +
  labs(caption = paste0("no correction done for anything here", "\n",
                        "only dropped 3 outliers because of their calculated vocabulary depth"))
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
  filter(!te_id %in% c("2E_040", "2E_081", "2E_057")) %>%
  filter(vol_source != "all")
library(lmerTest)
registerDoMC(cores = 6)
lm.results <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  # predict divergence using the IQ/NIH measure. 
  # adding participant's ID as a random variable, and the word/mini-task
  lm <- lmerTest::lmer(vol_value ~ xx + count_value +(1|te_id) + (1|vol_source),
                       data = cbind(m123 %>% 
                                      select(vol_value, vol_source, te_id, count_value) %>%
                                      mutate(vol_value = scale(vol_value, scale = T, center = T)[,1]),
                                    xx=m123[,i]) %>%
                         rename(xx = 5))
  gc()
  # combine results in a df, and save
  df <- coef(summary(lm)) %>%
    as.data.frame() %>%
    rownames_to_column("fixed") %>%
    filter(fixed != "(Intercept)") %>%
    mutate(confint_min = Estimate - `Std. Error`,
           confint_max = Estimate + `Std. Error`,
           pval = `Pr(>|t|)`,
           var = var)
  write_rds(df, paste0("data/derivatives/chulls-lmer/", var, ".rds"))
  gc()
  return(df)
}
# combine the saved lmer results
lm.results <- foreach(i=3:29, .combine = rbind) %dopar% {
  var <- colnames(m123)[i]
  if (file.exists(paste0("data/derivatives/chulls-lmer/", var, ".rds"))) {
    df <- read_rds(paste0("data/derivatives/chulls-lmer/", var, ".rds"))
    return(df)
  } else {
    return(NULL)
  }
}
lm.results <- lm.results %>% mutate(FDR = p.adjust(pval, method = "fdr"))
# write_rds(lm.results, "data/derivatives/chulls-lmer/all-lmer-results.rds", compress = "gz")
# make plot for results
lm.results %>%
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
                        "    lmer(z-standardized_vocab-depth ~ X + word_count + (1|te_id) + (1|word))", "\n",
                        "    where X is a selected variable from the IQ or NIH-TB variables", "\n",
                        "        and word_count is how many points/words in participant's response in this task", "\n",
                        "Derivation of vocabulary depth was as follows:", "\n",
                        "    vocabulary_depth = volume enclosed between 3d semantic space of words said","\n",
                        "    765 word embeddings were derived from 'text' package and then", "\n",
                        "        UMAP was utilized to reduce dimensions for 3D","\n",
                        "    Only kept participants with at least 4 words in response to task/word"))
ggsave(filename = "figs/lmer-vocab-depth-by-iq-and-wc-random-id-and-word.png",
       width = 7, height = 8, units = "in", bg = "white", dpi = 360)
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
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
