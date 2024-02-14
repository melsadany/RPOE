################################################################################
#                     analysis of language/words response in PS-VC             #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(text)
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
#  read the PS_VC task metadata
ps.vc.metadata <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 2) %>%
  filter(task_v==2) %>%
  select(task_num, word) %>%
  rownames_to_column("task_order") %>% mutate(task_order = as.numeric(task_order))
# keep participants of interest
participants.metadata <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 1)
p.of.int <- participants.metadata %>% 
  filter(work_on_2 != "F")
################################################################################
# read tests data, and combine nih-tb and iq
nih.tb <- read_csv("data/derivatives/nih-tb_clean.csv") %>%
  select(dev_id, 
         ends_with("age_corrected_standard_score")) %>%
  drop_na()
iq <- read_csv("data/derivatives/wisc-and-wais_clean.csv") %>%
  select(dev_id, te_id,
         paste0(c("PSI", "WM", "VCI"), "_composite_score"),
         # ends_with("composite_score"),
         FSIQ, 
         SI, VC, BD, VP, MR, 
         #FW, PS, IN, AR, 
         DS, CD, SS) %>%
  mutate(VCI_PSI = VCI_composite_score - PSI_composite_score,
         abs_VCI_PSI = abs(VCI_composite_score - PSI_composite_score))
m1.m2 <- inner_join(nih.tb, iq) %>%
  mutate(te_id=ifelse(is.na(te_id), dev_id, te_id)) %>%
  rename(te_id = te_id)
rm(nih.tb);rm(iq);gc()
################################################################################
# get the clean transcription for data, and their embeddings from openAI
all <- read_rds("data/derivatives/ps-vc-text-analyzed-and-embedded.rds")
################################################################################
# get the embeddings from text package
load("data/derivatives/word-embedding-from-text-package.rda")
emb.text.m <- cbind(all[,1:5], emb.text.m) %>% distinct(te_id,word,text, .keep_all = T) # keep unique words
################################################################################
################################################################################
###################### sentiment analysis by embeddings ########################
################################################################################
################################################################################
# get embeddings for main emotion words
emotions <- c("anger", "anticipation", "disgust", "fear", "joy", 
              "negative", "positive", "sadness", "surprise", "trust")
em.emb <- text::textEmbed(emotions)
em.emb.m <- em.emb$texts$texts
sentiment <- foreach(i = 1:nrow(emb.text.m), .combine = rbind) %dopar% {
  df1 <- emb.text.m[i,-c(1:5)]
  # # get the similarity between text, and emotion words embeddings
  # sim <- do.call(rbind,
  #                lapply(c(1:length(emotions)), function(x) text::textSimilarity(df1,em.emb.m[x,]))) %>%
  #   t()
  # colnames(sim) <- paste0("sim_", emotions)
  # get the distance between text, and emotion words embeddings
  dis <- do.call(rbind,
                 lapply(c(1:length(emotions)), function(x) dist(rbind(df1,em.emb.m[x,]), method = "euclidean"))) %>%
    t()
  colnames(dis) <- paste0("dis_", emotions)
  # diff <- do.call(rbind,
  #                 lapply(c(1:length(emotions)), function(x) t(df1)-t(em.emb.m[x,]))) %>%
  #   t()
  # colnames(diff) <- paste0("diff_", emotions)
  
  # combine results, and return
  cbind(emb.text.m[i,1:5],
        dis)
        # sim)
}
# get correlations between sim_emotions
corr.table(sentiment %>% select(contains(emotions[1:5])),
           sentiment %>% select(contains(emotions[6:10])),
           method = "spearman") %>%
  filter(V1 != V2) %>%
  ggplot(aes(x=V1, y=V2, fill=r, label=ifelse(pval<0.05, "*", "")))+
  geom_tile()+
  geom_text(size=2)+
  redblack.col.gradient+my.guides+null_labs
# ggsave(filename = "figs/corr-between-sim-with-emotion-words.png", bg = "white",
ggsave(filename = "figs/corr-between-dis-with-emotion-words.png", bg = "white",
       width = 6, height = 6, units = "in", dpi=320)
# average the sentiment per participant 
avg.sent <- sentiment %>%
  group_by(te_id) %>%
  # dplyr::summarise_at(.vars = vars(starts_with("sim_")), .funs = function(x) mean(x))
  dplyr::summarise_at(.vars = vars(starts_with("dis_")), .funs = function(x) mean(x))
# get correlation between IQ and average euc distance from emotion words
m123 <- inner_join(m1.m2, avg.sent)
corr.table(m123 %>% select(colnames(m1.m2), -ends_with("id")),
           m123 %>% select(starts_with("dis")),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(m1.m2), V2 %in% colnames(avg.sent)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ")) %>%
  mutate(V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = sub("dis_", "", V2)) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(cols = vars(cat2),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "",
       title = "correlation between average euc distance from emotion words and IQ",
       caption = paste0("Euclidean distance is calculated as the distance between word said and the emotion word","\n",
                        "The Euclidean distance was then averaged by participant","\n",
                        "n(samples): ", nrow(m123), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = "figs/corr-between-dis-with-emotion-words-and-iq.png", bg = "white",
       width = 8, height = 6, units = "in", dpi=320)
################################################################################
# get sentiment by syuzhet sentiment?
sentiment.2 <- cbind(emb.text.m[,1:5],
                     sentiment_syuzhet = syuzhet::get_sentiment(emb.text.m$text, method = "syuzhet")) 
avg.sent.2 <- sentiment.2 %>%
  group_by(te_id) %>%
  dplyr::summarise(sent_syuzhet = mean(sentiment_syuzhet))
m124 <- inner_join(m1.m2, avg.sent.2)
corr.table(m124 %>% select(colnames(m1.m2), -ends_with("id")),
           m124 %>% select(sent_syuzhet),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(m1.m2), V2 == "sent_syuzhet") %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ")) %>%
  mutate(V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1))) %>%
  ggplot(aes(x=V2, y=V1, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(rows = vars(cat2),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "",
       title = "correlation between sentiment and IQ",
       caption = paste0("n(samples): ", nrow(m124), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides + theme(plot.title = element_text(hjust = 1, size = 8),
                    axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
ggsave(filename = "figs/corr-between-syuzhet-sentiment-and-iq.png", bg = "white",
       width = 3, height = 6, units = "in", dpi=320)
################################################################################


################################################################################
################################################################################
################## word creativity by similarity and distance ##################
################################################################################
################################################################################
# creativity quotient per word defined as:
# cq1 = (1-word_freq) * distance_from_prompt
# cq2 = word_sim * distance_from_prompt

####
# for task 1
####
# get embeddings of prompt words
emb.word.2 <- text::textEmbed(unique(emb.text.m$word))
emb.word.2.m <- emb.word.2$texts$texts %>%
  mutate(word = unique(emb.text.m$word))
# do word freq dictionary based on the words said by participants
word.freq.p <- table(emb.text.m$text) %>% 
  as.data.frame() %>%
  rename(text = 1, count = 2) %>%
  mutate(freq = count/nrow(emb.text.m))
# get creativity quotient per word said
prompt.stats <- foreach(i = 1:nrow(emb.text.m), .combine = rbind) %dopar% {
  if (emb.text.m$task[i]==3) {
    return(NULL)
  }
  id <- emb.text.m$te_id[i]
  w0 <- emb.text.m$word[i]
  w1 <- emb.text.m$text[i]
  d <- dist(rbind(emb.text.m[i,-c(1:5)],
                  emb.word.2.m[which(emb.word.2.m$word==w0),-(ncol(emb.word.2.m))])) %>% as.numeric()
  f <- word.freq.p$freq[which(word.freq.p$text==w1)]
  s <- text::textSimilarity(emb.text.m[i,-c(1:5)],
                            emb.word.2.m[which(emb.word.2.m$word==w0),-(ncol(emb.word.2.m))], method = "cosine") %>% as.numeric()
  cq1 <- (1-f)*d
  cq2 <- s*d
  cbind(emb.text.m[i,1:5],
        d_from_prompt = d,
        freq_text = f,
        sim_to_prompt = s,
        cq1 = cq1,
        cq2 = cq2)
}
####################
# histogram of distribution of cq per participant
# cq1
p0 <- prompt.stats %>%
  ggplot(aes(x=cq1)) +
  geom_histogram()+
  facet_wrap(~te_id)
p00 <- prompt.stats %>%
  ggplot(aes(x=cq1)) +
  geom_histogram()+
  facet_wrap(~word)
# cq2
p01 <- prompt.stats %>%
  ggplot(aes(x=cq2)) +
  geom_histogram()+
  facet_wrap(~te_id)
p001 <- prompt.stats %>%
  ggplot(aes(x=cq2)) +
  geom_histogram()+
  facet_wrap(~word)
p000 <- prompt.stats %>%
  ggplot(aes(x=cq1, y=cq2))+
  geom_point()+
  geom_smooth(method = "loess")
patchwork::wrap_plots(patchwork::wrap_plots(p0,p00, ncol = 1, heights = c(5,4)),
                      patchwork::wrap_plots(p01,p001, ncol = 1, heights = c(5,4)), 
                      patchwork::wrap_plots(p000, patchwork::plot_spacer(), ncol = 1, heights = c(1,3)),
                      ncol = 3, widths = c(2,2,1))

ggsave(filename = "figs/distribution-of-creativity-quotient-task-1.png", bg = "white",
       width = 19, height = 8, units = "in", dpi=320)
###################
# correlate creativity quotient, word_freq, and distance from prompt to IQ
avg.stats <- prompt.stats %>%
  group_by(te_id) %>%
  dplyr::summarise_at(vars("d_from_prompt", "freq_text", "sim_to_prompt", starts_with("cq")), function(x) mean(x))
m126 <- inner_join(m1.m2, avg.stats)
pp1 <- corr.table(m126 %>% select(colnames(m1.m2), -ends_with("id")),
           m126 %>% select(colnames(avg.stats), -ends_with("id")),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2), colnames(avg.stats)), V2 %in% colnames(avg.stats),
         V1 != V2) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         cat2 = ifelse(V1 %in% colnames(avg.stats), "derived", cat2)) %>%
  mutate(V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V2, y=V1, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(rows = vars(cat2),
                     scales = "free", space = "free") +
  redblack.col.gradient +
  labs(x = "", y = "",
       title = "correlation between CQ and IQ",
       caption = paste0("results are by using task1/words only",
                        "n(samples): ", nrow(m126), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides + theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
####
# for task 3
####
# get embeddings of prompt words
emb.word.2 <- text::textEmbed(unique(emb.text.m$word))
emb.word.2.m <- emb.word.2$texts$texts %>%
  mutate(word = unique(emb.text.m$word))
# do word freq dictionary based on the words said by participants
word.freq.p <- table(emb.text.m$text) %>% 
  as.data.frame() %>%
  rename(text = 1, count = 2) %>%
  mutate(freq = count/nrow(emb.text.m))
# get creativity quotient per word said
prompt.stats <- foreach(i = 1:nrow(emb.text.m), .combine = rbind) %dopar% {
  if (emb.text.m$task[i]==1) {
    return(NULL)
  }
  id <- emb.text.m$te_id[i]
  w0 <- emb.text.m$word[i]
  w1 <- emb.text.m$text[i]
  d <- dist(rbind(emb.text.m[i,-c(1:5)],
                  emb.word.2.m[which(emb.word.2.m$word==w0),-(ncol(emb.word.2.m))])) %>% as.numeric()
  f <- word.freq.p$freq[which(word.freq.p$text==w1)]
  s <- text::textSimilarity(emb.text.m[i,-c(1:5)],
                            emb.word.2.m[which(emb.word.2.m$word==w0),-(ncol(emb.word.2.m))], method = "cosine") %>% as.numeric()
  cq1 <- (1-f)*d
  cq2 <- s*d
  cbind(emb.text.m[i,1:5],
        d_from_prompt = d,
        freq_text = f,
        sim_to_prompt = s,
        cq1 = cq1,
        cq2 = cq2)
}
####################
# histogram of distribution of cq per participant
# cq1
p0 <- prompt.stats %>%
  ggplot(aes(x=cq1)) +
  geom_histogram()+
  facet_wrap(~te_id)
p00 <- prompt.stats %>%
  ggplot(aes(x=cq1)) +
  geom_histogram()+
  facet_wrap(~word,nrow = 1)
# cq2
p01 <- prompt.stats %>%
  ggplot(aes(x=cq2)) +
  geom_histogram()+
  facet_wrap(~te_id)
p001 <- prompt.stats %>%
  ggplot(aes(x=cq2)) +
  geom_histogram()+
  facet_wrap(~word,nrow = 1)
p000 <- prompt.stats %>%
  ggplot(aes(x=cq1, y=cq2))+
  geom_point()+
  geom_smooth(method = "loess")
patchwork::wrap_plots(patchwork::wrap_plots(p0,p00, ncol = 1, heights = c(5,1)),
                      patchwork::wrap_plots(p01,p001, ncol = 1, heights = c(5,1)), 
                      patchwork::wrap_plots(p000, patchwork::plot_spacer(), ncol = 1, heights = c(1,2)),
                      ncol = 3, widths = c(2,2,1))

ggsave(filename = "figs/distribution-of-creativity-quotient-task-3.png", bg = "white",
       width = 19, height = 8, units = "in", dpi=320)
###################
# correlate creativity quotient, word_freq, and distance from prompt to IQ
avg.stats <- prompt.stats %>%
  group_by(te_id) %>%
  dplyr::summarise_at(vars("d_from_prompt", "freq_text", "sim_to_prompt", starts_with("cq")), function(x) mean(x))
m126 <- inner_join(m1.m2, avg.stats)
pp2 <- corr.table(m126 %>% select(colnames(m1.m2), -ends_with("id")),
           m126 %>% select(colnames(avg.stats), -ends_with("id")),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2), colnames(avg.stats)), V2 %in% colnames(avg.stats),
         V1 != V2) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         cat2 = ifelse(V1 %in% colnames(avg.stats), "derived", cat2)) %>%
  mutate(V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V2, y=V1, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(rows = vars(cat2),
                     scales = "free", space = "free") +
  redblack.col.gradient +
  labs(x = "", y = "",
       title = "correlation between CQ and IQ",
       caption = paste0("results are by using task1/words only",
                        "n(samples): ", nrow(m126), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides + theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
###
patchwork::wrap_plots(pp1+labs(subtitle = "task 1"),pp2+labs(subtitle = "task 3"),nrow = 1)
ggsave(filename = "figs/corr-between-cq-and-iq.png", bg = "white",
       width = 13, height = 8, units = "in", dpi=320)
################################################################################


################################################################################
################################################################################
########################## word frequency and rarity  ##########################
################################################################################
################################################################################
# do word freq dictionary based on the words said by participants
word.freq.p <- table(emb.text.m$text) %>% 
  as.data.frame() %>%
  rename(text = 1, count = 2) %>%
  mutate(freq = count/nrow(emb.text.m))
m125 <- inner_join(m1.m2,
                   left_join(emb.text.m[,c(1:5)],word.freq.p)%>%
                     group_by(te_id) %>%
                     dplyr::summarise(avg_freq = mean(freq)) %>% ungroup())
corr.table(m125 %>% select(colnames(m1.m2), -ends_with("id")),
           m125 %>% select(avg_freq),
           method = "spearman") %>%
  mutate(FDR=p.adjust(pval,"fdr")) %>%
  filter(V1 %in% colnames(m1.m2), V2 == "avg_freq") %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ")) %>%
  mutate(V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1))) %>%
  ggplot(aes(x=V2, y=V1, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(rows = vars(cat2),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "",
       title = "correlation between word freq and IQ",
       caption = paste0("n(samples): ", nrow(m124), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides + theme(plot.title = element_text(hjust = 1, size = 8),
                    axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
ggsave(filename = "figs/corr-between-word-freq-participant-referenced-and-iq.png", bg = "white",
       width = 3, height = 6, units = "in", dpi=320)
################################################################################


################################################################################
# word freq from  reference?
word.freq <- read_delim("/Dedicated/jmichaelson-wdata/msmuhammad/data/corpus/top60k.txt", 
                        delim = "\t", col_names = c("rank", "pos", "word", "total",
                                                    "spoken", "fiction", "magazine",
                                                    "newspaper", "academic"), skip = 1) %>%
  mutate(pos = str_remove_all(pos, pattern = " "),
         word = str_remove_all(word, pattern = " "),
         word = str_remove_all(word, pattern = "\\("),
         word = str_remove_all(word, pattern = "\\)")) %>%
  distinct(word, .keep_all = T)
# get singular of plural words
library(pluralize)
sin.emd.text.m <- emb.text.m %>%
  mutate(text = pluralize::singularize(text))%>%
  filter(text!=word,
         nchar(text)>1) %>%
  distinct(text,te_id,word, .keep_all = T) %>%
  mutate(one_w = ifelse(grepl(" ", text), F,T)) %>% filter(one_w == T)
table(sin.emd.text.m$text %in% word.freq$word)
####


# get the most common 20k words from wordnet
library(wordnet)
if(initDict()) {
  filter <- getTermFilter("ExactMatchFilter", "hot", TRUE)
  terms <- getIndexTerms("ADJECTIVE", 100, filter)
  synsets <- getSynsets(terms[[1]])
  related <- getRelatedSynsets(synsets[[1]], "!")
  sapply(related, getWord)
}
################################################################################

################################################################################
################################################################################
########################## closeness to target word ############################
################################################################################
################################################################################
# get a probability of being on target to the prompt word using embeddings as input
# and prompt word among inputs

# get embeddings of all words said by participants in response to a prompt
# combine that with the embeddings for the prompt word itself
# get 2 clusters out of this
  prompt.emb <- textEmbed(unique(all$word))
prompt.emb <- cbind(all %>%
                      distinct(word, task),
                    prompt.emb$texts$texts)
w <- unique(all$word)[1]
t1 <- emb.text.m %>%
  # filter(word == w) %>%
  filter(task ==1) %>%
  select(te_id, text, word, starts_with("Dim"))
center.emb <- prompt.emb %>%
  # filter(word == w) %>%
  filter(task ==1) %>%
  select(starts_with("Dim"))
t2 <- rbind(prompt.emb %>%
              # filter(word == w) %>%
              filter(task ==1) %>%
              mutate(te_id = "PROMPT", text = word) %>%
              select(te_id, text, word, starts_with("Dim")),
            t1)

t2 %>%
  mutate(t = ifelse(te_id=="PROMPT", T, F)) %>%
  ggplot(aes(x=Dim1_texts, y=Dim2_texts, color = t))+
  geom_point()+
  facet_wrap(~word)

km <- kmeans(t2[,-c(1:2)], 
             centers = 20)
t3 <- t2[,1:2] %>%
  mutate(cluster = km$cluster)  %>%
  mutate(
    Distance_to_Center = sqrt(rowSums((t1[,-c(1:2)] - as.vector(center.emb))^2)),
    Probability_to_Center = 1 - Distance_to_Center / max(Distance_to_Center)
  )
###


################################################################################

################################################################################
################################################################################
#########################  #########################
################################################################################
################################################################################
# 

################################################################################

################################################################################

################################################################################