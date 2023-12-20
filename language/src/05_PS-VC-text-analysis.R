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
ps.vc.metadata <- read_csv("data/derivatives/PS_VC-task-metadata.csv") %>%
  select(task_num, word) %>%
  rownames_to_column("task_order") %>% mutate(task_order = as.numeric(task_order))
# keep participants of interest
participants.metadata <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 1)
p.of.int <- participants.metadata %>% 
  filter(work_on_2 != "F")
################################################################################
# read tests data, and combine nih-tb and iq
nih.tb <- read_csv("data/derivatives/nih-tb_clean_120623.csv") %>%
  select(dev_id, 
         ends_with("age_corrected_standard_score")) %>%
  drop_na()
iq <- read_csv("data/derivatives/wisc-and-wais_clean_120623.csv") %>%
  select(dev_id, te_id,
         paste0(c("PSI", "WM", "VCI"), "_composite_score"),
         # ends_with("composite_score"),
         FSIQ, 
         SI, VC, BD, VP, MR, 
         #FW, PS, IN, AR, 
         DS, CD, SS)
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
pairs.sim <- rbind(pairs.sim.1 %>% mutate(task=1), pairs.sim.3 %>% mutate(task=3))
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
  mutate(consec = ifelse(w_order==1&w2_order==2, T, F)) %>%
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
       title = paste0("task: ",t),
       caption = paste0("n(samples): ", nrow(m123.1), "\n",
                        "**\t\tFDR<0.05", "\n",
                        "*\t\tpval<0.05")) +
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
       title = paste0("task: ",t),
       caption = paste0("n(samples): ", nrow(m123.3), "\n",
                        "**\t\tFDR<0.05", "\n",
                        "*\t\tpval<0.05")) +
  my.guides + theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))

p <- p1+p3
ggsave(p, filename = "figs/correlation-between-consec-pairs-avg-cos-sim-and-iq.png",
       width = 6, height = 5, units = "in", bg = "white", dpi = 320)
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
# calculate the optimal and actual trajectories per participant for first 5 words per task/word
divergence <- foreach(i=1:length(unique(tmp$te_id)), .combine = rbind) %dopar% {
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
    # you only need to keep the first 5 words said by task/word
    df3 <- df3[1:5,1:5]
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
                            w1_order = c(1:4),
                            w2 = colnames(df3)[-1],
                            w2_order = c(2:5),
                            distance = NA)
    for (m in 1:nrow(act.order)) { # get actual path distances
      act.order$distance[m] <- (1-df3[act.order$w1_order[m],act.order$w2_order[m]])
    }
    act.dist <- sum(act.order$distance) # this is the actual distance for this word for this participant
    df6 <- data.frame(te_id = id,
                      word = w0,
                      opt_dist = opt.dist,
                      act_dist = act.dist,
                      global_divergence = (act.dist - opt.dist))
    return(df6)
  }
  return(word.opt)
}

##############################################################################


##############################################################################
