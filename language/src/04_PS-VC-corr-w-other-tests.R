################################################################################
#                   checking correlation between PS_VC and other tests         #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(tuneR)
source("/Dedicated/jmichaelson-wdata/msmuhammad/workbench/customized-functions/getEmbedding.R")
softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk))) 
  }
  val <- exp(par - Lk)
  return(val)
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
################################################################################
# read tests data
nih.tb <- read_csv("data/derivatives/nih-tb_clean.csv")
iq <- read_csv("data/derivatives/wisc-and-wais_clean.csv")
################################################################################
# check correlation between NIH-TB and IQ
m1 <- nih.tb %>%
  select(te_id, dev_id, 
         ends_with("age_corrected_standard_score")) %>%
  drop_na() %>%
  mutate(PV_PS_age_corrected_standard_score = PV_age_corrected_standard_score - pattern_age_corrected_standard_score,
         abs_PV_PS_age_corrected_standard_score = abs(PV_age_corrected_standard_score - pattern_age_corrected_standard_score))
m2 <- iq %>%
  select(dev_id, te_id,
         paste0(c("PSI", "WM", "VCI"), "_composite_score"),
         # ends_with("composite_score"),
         FSIQ, 
         SI, VC, BD, VP, MR, 
         #FW, PS, IN, AR, 
         DS, CD, SS) %>%
  mutate(VCI_PSI = VCI_composite_score - PSI_composite_score,
         abs_VCI_PSI = abs(VCI_composite_score - PSI_composite_score))
m1.m2 <- inner_join(m1, m2) %>%
  mutate(te_id=ifelse(is.na(te_id), dev_id, te_id)) %>%
  rename(te_id = te_id)
corr.table(m1.m2 %>% select(any_of(colnames(m1)), - ends_with("_id")),
           m1.m2 %>% select(any_of(colnames(m2)), - ends_with("_id"))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(m1),
         V2 %in% colnames(m2)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "", V1),
         V2 = sub("_composite_score", "", V2)) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3)+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(y = "IQ", x = "NIH_TB", 
       caption = paste0("n(samples): ", nrow(m1.m2),"\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = "figs/corr-between-NIH-TB-IQ.png", bg="white",
       width = 6, height = 6, units = "in", dpi = 320)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# # dump
# ####
# # read response sound to task-4 faces1 of all participants 
# registerDoMC(cores = 1)
# all.ps.vc <- foreach(i = 1:nrow(p.of.int), .combine = rbind) %dopar% {
# # for(i in 1:nrow(p.of.int)) {
#   pid <- p.of.int$ID[i]
#   task <- readWave(paste0(project.dir, "/data/derivatives/PS-VC_participants-response/", 
#                           pid, "/", pid, "_task-1_alone_9.mp3"))
#                           # pid, "/", pid, "_task-4_faces1_30.mp3"))
#   # print(task)
#   # print(paste0("participant: ", pid, " has a record of: ", length(task@left)/task@samp.rate))
#   t <- data.frame(te_id = pid, value = as.numeric(task@left), sample = c(1:length(as.numeric(task@left))))
#   return(t)
# }
# all.ps.vc <- all.ps.vc %>%
#   pivot_wider(names_from = "sample", values_from = "value", id_cols = te_id)
# # check correlation between response and iq/nih 
# m123 <- inner_join(m1.m2, all.ps.vc) %>%
#   filter(dev_id != "4247_2") %>%
#   select(-c(AR, IN, PS, FW, starts_with("PRI_"), starts_with("FR_"), starts_with("VSI_")))
# corr.func(m123 %>% select(any_of(colnames(m1.m2)), -ends_with("id")),
#            m123 %>% select(any_of(colnames(all.ps.vc)), -te_id))
################################################################################
################################################################################

################################################################################
################################################################################
# correlation between PS-VC word responses and iq-nih
vc.transcription <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 4) %>%
  mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised)))) %>%
  mutate(word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  mutate(text = tolower(text)) %>%
  filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
  select(te_id=ID, task, task_order, word, text) %>%
  filter(word != text) %>% #drop the words that are exactly the same as the prompt word
  distinct() # only keep unique words and drop repeated by the same participant in the same task/word
# look at categories of words said
vc.analyzed <- cbind(vc.transcription,
             nrc = syuzhet::get_nrc_sentiment(vc.transcription$text),
             sentimentr::profanity(vc.transcription$text)%>%select(profanity_count),
             lingmatch = lingmatch::lma_meta(vc.transcription$text),
             lingmatch = lingmatch::lma_termcat(vc.transcription$text)) %>%
  select(-c(lingmatch.words, lingmatch.unique_words, lingmatch.clauses, lingmatch.sentences, 
            lingmatch.words_per_clause,
            lingmatch.words_per_sentence, lingmatch.characters_per_word, lingmatch.syllables_per_word,
            lingmatch.type_token_ratio))
# get openai word embeddings
tmp <- getEmbedding(vc.transcription$text)
all <- cbind(vc.analyzed, tmp)
# save?
write_rds(all, "data/derivatives/ps-vc-text-analyzed-and-embedded.rds")
all <- read_rds("data/derivatives/ps-vc-text-analyzed-and-embedded.rds")
#####
# count um, uh, eh, hmm, oh
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
  full_join(vc.transcription %>% distinct(te_id)) %>%
  mutate(ums_count = ifelse(is.na(ums_count), 0, ums_count))
# get words count per participant
word.count <- vc.transcription %>%
  group_by(te_id) %>%
  dplyr::summarise(word_count = n())
# number of characters
chr.wise <- vc.analyzed %>%
  pivot_longer(cols = c(lingmatch.characters, lingmatch.syllables, lingmatch.reading_grade), 
               names_to = "cat1") %>%
  group_by(te_id, cat1) %>%
  dplyr::summarise(avg = mean(value, na.omit = T)) %>%
  pivot_wider(names_from = "cat1", values_from = "avg", id_cols = "te_id")
# ratio of words category being said
word.wise <- vc.analyzed %>%
  pivot_longer(cols = c(starts_with("nrc"), profanity_count, 
                        lingmatch.sixltr, lingmatch.ppron, lingmatch.ipron, lingmatch.adverb, 
                        lingmatch.conj, lingmatch.auxverb, lingmatch.prep, lingmatch.negate, lingmatch.quant), 
               names_to = "cat2") %>%
  group_by(te_id, cat2) %>%
  dplyr::summarise(count = sum(value)) %>%
  left_join(word.count) %>%
  mutate(cat_ratio = count / word_count) %>%
  pivot_wider(names_from = "cat2", values_from = "cat_ratio", id_cols = "te_id")
# get average of waiting time between consecutive pairs of words
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
# save summ for PS-VC
ttt <- inner_join(inner_join(chr.wise, word.wise),
                  inner_join(word.count, 
                             inner_join(ums,wait.time2)))
write_rds(ttt, file = "data/derivatives/ps-vc-summary-data.rds")
###############
# get the correlation between nih-tb / IQ and word count/ums count/language features from PS-VC audio
m124 <- inner_join(m1.m2, 
                   inner_join(inner_join(chr.wise, word.wise),
                              inner_join(word.count, 
                                         inner_join(ums,wait.time2))))
pp <- corr.table(m124 %>% select(any_of(c(colnames(m1), colnames(m2))),
                           -ends_with("id")),
           m124 %>% select(colnames(chr.wise), 
                           colnames(word.wise), 
                           ums_count, word_count,
                           avg_wait,
                           -te_id),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1), colnames(m2)), 
         V2 %in% c("ums_count", "word_count", "avg_wait",colnames(chr.wise), colnames(word.wise))) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2))) %>%
  mutate(cat1 = ifelse(grepl(paste(c("characters", 
                                    "reading_grade",
                                    "syllables"), collapse = "|"), V2), "average", 
                      ifelse(V2 %in% c("ums_count", "word_count", "avg_wait"), "total", "ratio"))) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ")) %>%
  mutate(V1 = sub("_NIH", "", V1),
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
ggsave(pp, filename = paste0("figs/corr_iq-nih-PS-VC-language-features.png"),
       width = 6, height = 8, units = "in", dpi = 320, bg = "white")
################################################################################
all <- read_rds("data/derivatives/ps-vc-text-analyzed-and-embedded.rds") %>%
  mutate(text=tolower(text))
################################################################################
# # get cosine similarity using openai embeddings
# # drop this section, it's dumb
# # all <- all %>%
# #   mutate_at(.vars = vars(c(40:ncol(all))), .funs = function(x) scale(x, scale=T, center = T))
# # identify how far is the word mentioned from the task word
# task.emb.r <- cbind(ps.vc.metadata, getEmbedding(ps.vc.metadata$word))
# task.emb <- task.emb.r %>%
#   filter(task_num==1)
# # similarity using my openai embeddings
# all2 <- all[,40:ncol(all)]
# colnames(all2) <- paste0("Dim", colnames(all2), "_texts")
# task.emb2 <- full_join(all %>% select(word),
#                        task.emb) %>%
#   select(-c(1:5))
# colnames(task.emb2) <- paste0("Dim", colnames(task.emb2), "_texts")
# similarity2 <- text::textSimilarity(all2,
#                                     task.emb2)
######
# similarity using the text package embeddings and function
library(text)
emb.text <- textEmbed(all$text)
emb.text.m <- emb.text$texts$texts
emb.word <- textEmbed(all$word)
emb.word.m <- emb.word$texts$texts
save(emb.text, emb.text.m, emb.word, emb.word.m, file = "data/derivatives/word-embedding-from-text-package.rda")
# load("data/derivatives/word-embedding-from-text-package.rda")
similarity2 <- text::textSimilarity(emb.text.m,emb.word.m)
######
string.sim <- all %>%
  select(1:5) %>%
  mutate(similarity1 = stringdist::stringsim(text, word)) %>%
  mutate(similarity2 = similarity2) %>%
  filter(task == 1)
p10 <- string.sim %>%
  group_by(te_id, word) %>%
  dplyr::summarise(sim = mean(similarity2)) %>%
  ggplot(aes(x=word, y=te_id, fill = sim)) +
  geom_tile() +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1])+
  labs(y="ID", title = "average similarity of words said in comparison to the prompt word",
       caption = "cosine similarity calculated from:\n\t 768 embdedding vectors ectracted by text::textEmbed()")+
       # caption = "cosine similarity calculated from:\n\t 1536 embdedding vectors ectracted by openai")+
  my.guides
sim <- string.sim %>%
  group_by(te_id, word) %>%
  dplyr::summarise(sim = mean(similarity2)) %>%
  pivot_wider(names_from = "word", values_from = "sim")
sim2 <- string.sim %>%
  group_by(te_id) %>%
  dplyr::summarise(t1_avg_sim = mean(similarity2)) 
m125 <- inner_join(m1.m2, inner_join(sim, sim2))
p11 <- corr.table(m125 %>% select(colnames(m1), colnames(m2), -ends_with("id")),
           m125 %>% select(colnames(sim), colnames(sim2), -ends_with("id")),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2)),
         V2 %in% c(colnames(sim), colnames(sim2))) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         V2 = factor(V2, levels = unique(V2)),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1))) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(cols = vars(cat2),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "average similarity of words said",
       caption = paste0("n(samples): ", nrow(m125), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
p1011 <- patchwork::wrap_plots(p10,p11)
ggsave(p1011, filename = "figs/corr_iq-nih-PS-VC-word-similarity.png",
# ggsave(p1011, filename = "figs/corr_iq-nih-PS-VC-word-similarity-openai.png",
       width = 10, height = 9, units = "in", bg = "white", dpi = 320)
#####
# make a glm to predict IQ/NIH from the average cos sim to prompt words
# IQ ~ fly + face + window + etc...
m126 <- m125 %>%
  select(-c(PV_PS_age_corrected_standard_score, ends_with("VCI_PSI")))
coeff <- foreach(i=3:ncol(m1.m2), .combine = rbind) %dopar% {
  var <- colnames(m1.m2)[i]
  k <- which(colnames(m126) == var)
  y <- m126[,k] %>% unlist() %>% as.numeric()
  if (var %in% colnames(m126)) {
    summary(glm(y ~ ., 
                data = m126 %>%
                  mutate(y=as.numeric(y)) %>%
                  select(colnames(sim), -ends_with("id"), y),
                family = poisson()))$coefficients %>%
      as.data.frame() %>%
      rownames_to_column("var2") %>%
      rename(pval = 5) %>%
      mutate(var1 = var,
             confint_min = Estimate - `Std. Error`,
             confint_max = Estimate + `Std. Error`)
  }
}

coeff %>%
  mutate(x = sub("_age_corrected_standard_score", "_NIH", var1),
         sig = ifelse(pval < 0.05, "pval < 0.05", "pval >= 0.05")) %>%
  mutate(cat2 = ifelse(grepl("NIH", x), "NIH-TB", "IQ")) %>%
  mutate(x = sub("_NIH", "", x)) %>%
  ggplot(aes(y=var2, x =Estimate, alpha = sig), color = six.colors[1]) +
  geom_point(position = position_dodge(width = 0.6), size =1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.2) +
  scale_alpha_manual(values = c(1, 0.5)) +
  # scale_shape_manual(values = c(1, 2)) + 
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max), 
                 size = 0.8, height = 0, show.legend = F, 
                 position = position_dodge(width = 0.6)) +
  scale_color_manual(values = six.colors[1])  +
  ggh4x::facet_grid2(cols = vars(reorder(x, desc(cat2))), scales = "free_x") +
  labs(y="",
       caption = paste0("the estimates are derived from this lm formula:", "\n",
                        "   glm(y ~ all_words", "\n",
                        "   where y is a variable from the IQ/NIH-TB measures"))


#####
# try calculating the probability of being on target as follows:
# prob = 1 - (distance_to_prompt/max(distance_to_prompt))
registerDoMC(cores = 4)
distances <- foreach(i = 1:nrow(emb.text.m), .combine = rbind) %dopar% {
  data.frame(all[i,1:5],
             distance = as.numeric(dist(rbind(as.numeric(emb.text.m[i,]),
                                              as.numeric(emb.word.m[i,])))))
}
distances.f <- distances %>%
  filter(text != word, 
         task == 1) %>%
  group_by(word) %>%
  mutate(distance_n = softmax(distance),
         prob = 1 - (distance/max(distance)),
         prob_n = 1 - (distance_n/max(distance_n))) %>%
  ungroup()
dis <- distances.f %>%
  group_by(te_id, word) %>%
  dplyr::summarise(d = mean(distance_n)) %>%
  pivot_wider(names_from = "word", values_from = "d")
m126 <- inner_join(m1.m2, dis)
p11 <- corr.table(m126 %>% select(colnames(m1), colnames(m2), -ends_with("id")),
                  m126 %>% select(colnames(dis), -ends_with("id")),
                  method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2)),
         V2 %in% colnames(dis)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         V2 = factor(V2, levels = unique(V2)),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1))) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(cols = vars(cat2),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "average similarity of words said",
       caption = paste0("n(samples): ", nrow(m126), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
p11

################################################################################

################################################################################


################################################################################


################################################################################

