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
# get the cosine similarity between each pair of words for each participant
# make pairs per task 1

t=3 #identify the task you want to work on. either 1 or 3
registerDoMC(cores = 4)
pairs.sim <- foreach(i = 1:length(unique(emb.text.m$te_id)), .combine = rbind) %dopar% {
  id <- unique(emb.text.m$te_id)[i]
  
  prompts <- unique(emb.text.m$word)
  p.sim.2 <- foreach(k = 1:length(prompts), .combine = rbind) %dopar% {
    w0 <- prompts[k]
    # t <- emb.text.m %>% filter(word == )
    tmp.emb <- emb.text.m %>% 
      filter(te_id ==id, task==t, word ==w0)
    if (nrow(tmp.emb) == 0) {
      return(NULL)
    }
    sim.df <- data.frame(te_id = id,
                         word = w0,
                         w_order = rep(c(1:nrow(tmp.emb)), each = nrow(tmp.emb)),
                         w1 = rep(tmp.emb$text, each = nrow(tmp.emb)),
                         w2 = rep(tmp.emb$text, nrow(tmp.emb)),
                         cos_similarity = NA)
    for (j in 1:nrow(sim.df)) {
      # j=1
      w1 <- sim.df$w1[j]
      w2 <- sim.df$w2[j]
      e1 <- tmp.emb %>%
        distinct(text, .keep_all = T) %>%
        filter(text == w1) %>%
        select(-c(1:5))
      e2 <- tmp.emb %>%
        distinct(text, .keep_all = T) %>%
        filter(text == w2) %>%
        select(-c(1:5))
      sim.df$cos_similarity[j] <- text::textSimilarity(e1,e2, method = "cosine")
    }
    return(sim.df)
  }
  return(p.sim.2)
}
write_rds(pairs.sim, paste0("data/derivatives/pairs-sim-by-word-by-participant-task-",t,".rds"))
pairs.sim.1 <- read_rds(paste0("data/derivatives/pairs-sim-by-word-by-participant-task-1.rds"))
pairs.sim.3 <- read_rds(paste0("data/derivatives/pairs-sim-by-word-by-participant-task-3.rds"))
pairs.sim <- rbind(pairs.sim.1 %>% mutate(task=1), pairs.sim.3 %>% mutate(task=3)) %>%
  filter(w1 != w2)
################################################################################
# make a heatmap for each participant with 
p <- list()
for(i in 1:length(unique(pairs.sim$te_id))) {
  id <- unique(pairs.sim$te_id)[i]
  p[[i]] <- pairs.sim %>%
    filter(te_id == id) %>%
    mutate(w1 = factor(w1, levels = unique(w1)),
           w2 = factor(w2, levels = unique(w2))) %>%
    ggplot(aes(x=w1,y=w2,fill=cos_similarity))+
    geom_tile()+
    scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
    # ggh4x::facet_grid2(rows = vars(word), cols = vars(te_id), scales = "free", space = "free") +
    facet_wrap(~word, ncol = 1, scales = "free") +
    theme(axis.text.y.left = element_blank(), axis.text.x.bottom = element_blank()) +
    my.guides +
    labs(x="", y="", title = id)
}
p.all <- patchwork::wrap_plots(p, nrow = 1)
ggsave(p.all, filename = "figs/pairs-similarity.png",
       width = 32, height = 25, units = "in", dpi = 320, bg = "white")
################################################################################
# histogram of cosine similarity distribution for pairs
w2_order <- pairs.sim %>% 
  select(1,2,w2_order=w_order,w2=w1) %>% distinct()
pairs.sim %>%
  left_join(w2_order, relationship = "many-to-many") %>%
  filter(!(w_order==1&w2_order==1)) %>%
  mutate(consec = ifelse(w_order==1&w2_order==2, T, F)) %>%
  ggplot(aes(x=cos_similarity, fill = consec)) +
  geom_histogram(bins = 100) +
  facet_wrap(~task) +
  scale_fill_manual(values = redblack.col, name = "consecuetive pairs only") +
  labs(title = "faceted by task")
ggsave(bg = "white", filename = "figs/distribution-of-cos-similarity-of-all-pairs.png",
       width = 4, height = 4, units = "in", dpi = 320)
################################################################################
# get average of consecutive pairs similarity per participant
# get the correlation between average consec pairs similarity and iq
cons.pairs <- pairs.sim %>%
  left_join(w2_order, relationship = "many-to-many") %>%
  filter(!(w_order==1&w2_order==1)) %>%
  mutate(consec = ifelse(w2_order==w_order+1, T, F)) %>%
  filter(consec == T)
cons.pairs.mean <- cons.pairs %>%
  group_by(te_id, task) %>%
  dplyr::summarise(avg_cons_pairs_cos_sim = mean(cos_similarity))
# get correlations for task 1
m123.1 <- inner_join(m1.m2, cons.pairs.mean%>%filter(task==1))
p1 <- corr.table(m123.1 %>% select(colnames(m1.m2), -ends_with("id")),
                 m123.1 %>% select(avg_cons_pairs_cos_sim)) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(m1.m2),
         V2 %in% colnames(cons.pairs.mean)) %>%
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
       title = paste0("task: ",1),
       caption = paste0("n(samples): ", nrow(m123.1), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides + theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
# do it for task 3
m123.3 <- inner_join(m1.m2, cons.pairs.mean%>%filter(task==3))
p3 <- corr.table(m123.3 %>% select(colnames(m1.m2), -ends_with("id")),
                 m123.3 %>% select(avg_cons_pairs_cos_sim)) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(m1.m2),
         V2 %in% colnames(cons.pairs.mean)) %>%
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
  theme(axis.text.y.left = element_blank()) +
  labs(x = "", y = "",
       title = paste0("task: ",3),
       caption = paste0("n(samples): ", nrow(m123.3), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides + theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))

p <- p1+p3
ggsave(p, filename = "figs/correlation-between-consec-pairs-avg-cos-sim-and-iq.png",
       width = 6, height = 5, units = "in", bg = "white", dpi = 320)

# can you plot the correlation between the consec. pair distance VS. the wait time between them?
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
                    w1 = wait.time$text[-nrow(wait.time)],
                    w2 = wait.time$text[-1],
                    w1_index = rownames(wait.time)[-nrow(wait.time)],
                    w2_index = rownames(wait.time)[-1],
                    wait=(wait.time$start[-1] - wait.time$end[-nrow(wait.time)])/1000) %>%
  filter(wait>0)
tmp <- inner_join(cons.pairs, wait.time2) %>%
  left_join(m1.m2) %>%
  drop_na(FSIQ)
tmp %>% 
  ggplot(aes(x=cos_similarity, y=wait)) +
  geom_point(size=1)+
  geom_smooth(method = "loess") +
  ggpubr::stat_cor() +
  labs(y="silence time between consec. pairs (secs)")
ggsave(filename = "figs/corr-between-consec-pairs-cos-similarity-and-thinking-time.png", bg = "white",
       width = 4, height = 4, units = "in", dpi = 360)
################################################################################
# table of how many words said by participant in each task
tmp <- all[,1:5] %>%
  distinct(te_id, word, text, .keep_all = T) %>%
  group_by(te_id, word) %>%
  dplyr::summarise(count = n()) %>%
  pivot_wider(names_from = word, values_from = count, id_cols = te_id)
table <- kableExtra::kable(tmp, format="html") %>%
  kableExtra::kable_styling(full_width = T, protect_latex = T)
table
# plot these stats
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
##############################################################################
# calculate the full euclidean distance traveled per task, and normalize it by number of words
# this also considers at least 5 words said per task to get the euc distance
euc <- foreach(i=1:length(unique(tmp$te_id)), .combine = rbind) %dopar% {
  id <- unique(tmp$te_id)[i]
  df1 <- tmp %>% 
    filter(te_id == id) %>%
    pivot_longer(cols = colnames(tmp)[-1]) %>%
    filter(value >=5)
  words.to.keep <- unique(df1$name)
  df2 <- emb.text.m %>%
    filter(te_id==id,
           word %in% words.to.keep)
  # loop over words here
  word.opt <- foreach(j = 1:length(words.to.keep), .combine = rbind) %dopar% {
    w0 <- words.to.keep[j]
    df3 <- pairs.sim %>%
      filter(te_id==id, word==w0) %>%
      pivot_wider(names_from = "w2", values_from = "cos_similarity") %>%
      select(-c(te_id, word, w_order, task)) %>%
      column_to_rownames("w1")
    df3 <- df3[,rownames(df3)]
    ######
    # get the overall euclidean distance and divide it by number of words
    text.to.look <- unique(rownames(df3))
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
# drop task/word that have less than 5 participants that match the min of 5 words criteria
task.word.valid.euc <- euc %>%
  filter(te_id %in% m1.m2$te_id) %>%
  group_by(word) %>%
  dplyr::summarise(count = n()) %>%
  filter(count>=5)
euc <- euc %>%
  filter(word %in% task.word.valid.euc$word)
# make the euc in wide format
t1 <- euc %>%
  pivot_wider(names_from = word, values_from = full_euc_dist_normalized, id_cols = te_id)
# get the average euc distance traveled by participant per task 1&3
overalls <- euc %>%
  mutate(task = ifelse(nchar(word)==1, 3,1)) %>%
  group_by(te_id, task) %>%
  dplyr::summarise(euc=mean(full_euc_dist_normalized)) %>%
  pivot_longer(cols = c(euc)) %>% 
  mutate(name2 = paste0("_average_",name,"_t", task)) %>%
  filter(grepl("euc", name2)) %>%
  pivot_wider(names_from = "name2", values_from = "value", id_cols = "te_id")
# combine with IQ, and get the correlations
m124 <- inner_join(m1.m2, inner_join(t1,overalls))
p1 <- corr.table(m124 %>% select(colnames(m1.m2), -ends_with("id")),
                 m124 %>% select(colnames(t1),colnames(overalls), -ends_with("id")),
                 method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(m1.m2), V2 %in% c(colnames(t1),colnames(overalls)))  %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         task = ifelse(nchar(V2)==1|grepl("t3",V2),"task 3","task 1")) %>%
  mutate(V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1))) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(cols = vars(cat2),
                     rows = vars(task),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "",
       title = "correlation between normalized Euclidean distance and IQ",
       caption = paste0("Euclidean distance is calculated as the overall traveled distance by participant in each task/word","\n",
                        "The Euclidean distance was then normalized by how many words said by a participant in this task/word","\n",
                        "The data was filtered to only keep participants with at least 5 words in response to task/word","\n",
                        "The task/word were filtered to keep ones with at least 5 participants matching the prev. criteria","\n",
                        # "n(samples): ", nrow(m124), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
# this is how many participant were kept for analysis per task/word in euc
p2 <- euc %>%
  group_by(word) %>%
  dplyr::summarise(count = n()) %>%
  mutate(task = ifelse(nchar(word)==1, "task 3", "task 1")) %>%
  ggplot(aes(x=word, y = count)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = c(5,10,15,20,25), linetype=2, color = "red")+
  ggh4x::facet_grid2(cols = vars(task), scales = "free", space = "free")+
  labs(x="", y="number of valid participants")
# distribution of euc distance per task/word
p3 <- euc %>%
  mutate(task = ifelse(nchar(word)==1,"task 3", "task 1"),
         word = factor(reorder(word, desc(task)))) %>%
  ggplot(aes(x=full_euc_dist_normalized))+
  geom_histogram()+
  facet_wrap(~word)

patchwork::wrap_plots(p1,p2,p3,ncol = 1,heights = c(2,1,2))
ggsave("figs/corr-between-euc-and-iq.png", bg = "white",
       width = 8, height = 14, units = "in", dpi = 320)
##############################################################################
# calculate the optimal and actual trajectories per participant for first 10 words per task/word
divergence <- foreach(i=1:length(unique(tmp$te_id)), .combine = rbind) %dopar% {
  id <- unique(tmp$te_id)[i]
  df1 <- tmp %>% 
    filter(te_id == id) %>%
    pivot_longer(cols = colnames(tmp)[-1]) %>%
    mutate(task = ifelse(nchar(name)==1,3,1))%>%filter(task==3)%>% # only look at task 3 here for divergence
    filter(value >=10)
  words.to.keep <- unique(df1$name)
  df2 <- emb.text.m %>%
    filter(te_id==id,
           word %in% words.to.keep)
  if (length(words.to.keep)==0) {
    return(NULL)
  }
  # loop over words here
  word.opt <- foreach(j = 1:length(words.to.keep), .combine = rbind) %dopar% {
    w0 <- words.to.keep[j]
    df3 <- pairs.sim %>%
      filter(te_id==id, word==w0) %>%
      pivot_wider(names_from = "w2", values_from = "cos_similarity") %>%
      select(-c(te_id, word, w_order, task)) %>%
      column_to_rownames("w1")
    df3 <- df3[,rownames(df3)]
    ######
    # for the optimal and actual path
    # you only need to keep the first 10 words said by task/word
    df3 <- df3[1:10,1:10]
    # get the optimal trajectory
    library(TSP)
    distance.matrix <- as.dist(1 - df3) # convert cosine similarity matrix to a distance matrix
    tsp.dist <- TSP(distance.matrix) # travelling salesman problem
    tsp.sol <- as.integer(solve_TSP(tsp.dist))
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
                      global_divergence = (act.dist - opt.dist)
                      # global_divergence_normalized = (act.dist - opt.dist)/nrow(df3)
                      )
    return(df6)
  }
  return(word.opt)
}
# drop task/word that have less than 5 participants that match the min of 5 words criteria
task.word.valid.div <- divergence %>%
  filter(te_id %in% m1.m2$te_id) %>%
  group_by(word) %>%
  dplyr::summarise(count = n()) %>%
  filter(count>=5)
divergence <- divergence %>%
  filter(word %in% task.word.valid.div$word)
# make the divergence in wide format
t1 <- divergence %>%
  pivot_wider(names_from = word, values_from = global_divergence, id_cols = te_id)
# get the average euc distance traveled by participant per task 1&3
overalls <- divergence %>%
  mutate(task = ifelse(nchar(word)==1, 3,1)) %>%
  group_by(te_id, task) %>%
  dplyr::summarise(div=mean(global_divergence)) %>%
  pivot_longer(cols = c(div)) %>% 
  mutate(name2 = paste0("_average_",name,"_t", task)) %>%
  filter(grepl("div", name2)) %>%
  pivot_wider(names_from = "name2", values_from = "value", id_cols = "te_id")
m124 <- inner_join(m1.m2, inner_join(t1,overalls))
p1 <- corr.table(m124 %>% select(colnames(m1.m2), -ends_with("id")),
           m124 %>% select(colnames(t1),colnames(overalls), -ends_with("id")),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(m1.m2), V2 %in% c(colnames(t1),colnames(overalls)))  %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         task = ifelse(nchar(V2)==1|grepl("t3",V2),"task 3","task 1")) %>%
  mutate(V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1))) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(cols = vars(cat2),
                     rows = vars(task),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "",
       title = "correlation between divergence and IQ",
       caption = paste0("Divergence is calculated as (actual_path - optimal_path) in each task/letter","\n",
                        "The data was filtered to only keep participants with at least 10 words in response to task/letter","\n",
                        "The task/letter were filtered to keep ones with at least 5 participants matching the prev. criteria","\n",
                        # "n(samples): ", nrow(m124), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
# this is how many participant were kept for analysis per task/word in divergence
p2 <- divergence %>%
  group_by(word) %>%
  dplyr::summarise(count = n()) %>%
  mutate(task = ifelse(nchar(word)==1, "task 3", "task 1")) %>%
  ggplot(aes(x=word, y = count)) +
  geom_bar(stat = "identity") +
  geom_hline(yintercept = c(5,10,15), linetype=2, color = "red")+
  ggh4x::facet_grid2(cols = vars(task), scales = "free", space = "free")+
  labs(x="", y="number of valid participants")
# distribution of divergence per task/word
p3 <- divergence %>%
  mutate(task = ifelse(nchar(word)==1,"task 3", "task 1"),
         word = factor(reorder(word, desc(task)))) %>%
  ggplot(aes(x=global_divergence))+
  geom_histogram()+
  facet_wrap(~word,nrow = 1)

patchwork::wrap_plots(p1,p2,p3,ncol = 1,heights = c(1,1,1))
ggsave("figs/corr-between-divergence-and-iq.png", bg = "white",
       width = 8, height = 10, units = "in", dpi = 320)
##############################################################################


##############################################################################
