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
m1.m2.all <- read_rds("data/derivatives/m1m2-all-all.rds") 
demo <- read_rds("data/raw/demo.rds")
ps.vc <- read_rds("data/derivatives/ps-vc-text-clean.rds")
#####
pairs <- read_rds("data/derivatives/cons-pairs.rds") %>% as.data.frame()
euc <- read_rds("data/derivatives/euc-distance-w-minimum-of-2-words-per-task.rds") %>% as.data.frame() 
divergence <- read_rds("data/derivatives/divergence-w-minimum-of-3-words-per-task.rds") %>% as.data.frame()
voc.depth <- read_rds("data/derivatives/chulls.rds") %>% as.data.frame() %>%
  select(te_id, vol_source, vol_value)
comm.returns <- read_rds("data/derivatives/comm-returns.rds") %>% as.data.frame()
comm.lifetime <- read_rds("data/derivatives/community-data.rds") %>% as.data.frame() %>%
  filter(visit_lifetime>0)
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
combined.metrics <- euc %>% 
  select(te_id, prompt = word, full_euc_dist_normalized) %>%
  full_join(divergence %>% 
              select(te_id, prompt = word, global_divergence)) %>%
  full_join(pairs  %>% 
              select(te_id, prompt = word, cos_similarity) %>%
              group_by(te_id, prompt) %>%
              dplyr::summarise(cos_similarity = mean(cos_similarity, na.rm = T))) %>%
  full_join(voc.depth  %>% 
              select(te_id, prompt = vol_source, vol_value)) %>%
  full_join(comm.lifetime  %>% 
              select(te_id, prompt = word, archetype, visit_lifetime) %>%
              group_by(te_id, prompt, archetype) %>%
              dplyr::summarise(tot_archetype_lifetime = sum(visit_lifetime, na.rm = T)) %>%
              ungroup() %>% group_by(te_id, prompt) %>%
              dplyr::summarise(avg_lifetime = mean(tot_archetype_lifetime, na.rm = T))) %>%
  full_join(comm.returns %>%
              select(te_id, prompt = word, archetype, switches) %>%
              group_by(te_id, prompt) %>%
              dplyr::summarise(avg_switches = sum(switches, na.rm = T))) %>%
  full_join(comm.returns %>%
              select(te_id, prompt = word, archetype) %>%
              distinct() %>% group_by(te_id, prompt) %>%
              dplyr::summarise(communities_count = n()))
summarized.metrics <- combined.metrics %>%
  ungroup() %>% group_by(te_id) %>%
  dplyr::summarise(normalized_euc = mean(full_euc_dist_normalized, na.rm = T),
                   divergence = mean(global_divergence, na.rm = T),
                   cos_sim = mean(cos_similarity, na.rm = T),
                   vocab_depth = mean(vol_value, na.rm = T),
                   community_lifetime = mean(avg_lifetime, na.rm = T),
                   community_returns = mean(avg_switches, na.rm = T),
                   community_count = mean(communities_count, na.rm = T)) %>%
  full_join(avg)
  

# correlate features with m1m2
m123 <- inner_join(m1.m2, summarized.metrics)
# complete.cases(m123)
corr.table(m123 %>% select(colnames(m1.m2), -ends_with("_id")),
           m123 %>% select(any_of(colnames(summarized.metrics)), -te_id),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(summarized.metrics),
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
# try to check how these metrics can predict IQ/NIH-TB, but do a lmer instead of spearman
m124 <- full_join(all, combined.metrics) %>%
  distinct() %>%
  full_join(m1.m2) %>% ungroup() %>% distinct()  %>%
  left_join(demo[,c(2,4,5)])
write_rds(m124, "data/derivatives/all-summarized-metrics.rds")
# make heatmap of correlation between vars
corr.table(m124 %>% 
             select(3:10),
           m124 %>%
             select(11:16)) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(m124)[3:16],
         V2 %in% colnames(m124)[3:16],
         pval > 0) %>%
  ggplot(aes(x=V2, y=V1, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, ".",""))))+
  geom_tile()+
  geom_text(color = "white", nudge_x = 0)+
  redblack.col.gradient +
  labs(y = "", x = "", 
       caption = paste0("    ** FDR < 0.05", "\n",
                        "    .   pval < 0.05")) +
  my.guides
ggsave("figs/corr-language-metrics-w-each-other.png", bg = "white",
       width = 9, height = 10, units = "in", dpi = 360)
##################################################
# get cleaner dataframe with vars of interest rescaled 
# 
m125 <- m124 %>%
  select(1:2, onset, full_euc_dist_normalized, global_divergence,
         cos_similarity, vol_value, avg_lifetime,
         tot_comments, rep_prompt, rep_words_per_prompt, rep_word_at_all,
         avg_switches, communities_count, count_all,
         17:ncol(m124)) %>%
  filter(global_divergence > 0) %>%
  drop_na(dev_id)
write_rds(m125, "data/derivatives/lmer-input-models-1-2-3.rds", compress = "gz")
# try the 3 different models for prediction and plot them side by side
# model 1: univariate for IQ and language
# model 2: multivariate IQ ~ all language
# model 3: multivariate language ~ all IQ
# lmer here
lmer.results <- foreach(i = 17:(ncol(m125)-2), .combine = rbind) %dopar% {
  y.var <- colnames(m125)[i]
  y <- scale(m125[,i] %>% as.matrix() %>% as.numeric(),scale = T, center = T)[,1]
  # do a one to one model (model 1: univariate)
  lmer.metric.one <- foreach(j = 3:15, .combine = rbind) %dopar% {
    x.var <- colnames(m125)[j]
    x <- scale(m125[,j] %>% as.matrix() %>% as.numeric(), scale = T, center = T)[,1]
    lm <- lmerTest::lmer(yy ~ xx + (1|prompt) + age + sex + age:sex, 
                         data = m125 %>% select(te_id, prompt, age, sex) %>% ungroup() %>%
                           mutate(yy = as.numeric(y), xx = as.numeric(x)))
    res <- jtools::summ(lm, confin = T, pval = T)$coeftable %>%
      as.data.frame() %>%
      rownames_to_column("predictor") %>%
      filter(predictor != "(Intercept)") %>%
      rename(Estimate = 2,confint_min = 3,confint_max = 4,pval = 7) %>%
      mutate(y = y.var,x = x.var)
    return(res)
  }
  
  # didn't work
  # make a model that uses all metrics together to predict IQ (model 2: multivariate)
  d2 <- m125 %>%
    select(age, sex, any_of(c(colnames(all), colnames(combined.metrics))))%>%
    mutate(yy = y) %>%
    drop_na(yy)
  lm2 <- lmerTest::lmer(yy ~ onset + tot_comments + rep_prompt +
                          rep_words_per_prompt + rep_word_at_all + count_all +
                          full_euc_dist_normalized + global_divergence + cos_similarity +
                          vol_value + avg_lifetime + avg_switches + communities_count + 
                          age + sex + age:sex +
                          (1|prompt),
                        data = d2 %>%
                          mutate_at(c(5:ncol(d2)), .funs = function(x) scale(x, scale = T, center = T)[,1]))
  res.2 <- jtools::summ(lm2, confin = T, pval = T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("predictor") %>%
    filter(predictor != "(Intercept)") %>%
    rename(Estimate = 2,confint_min = 3,confint_max = 4,pval = 7) %>%
    mutate(y = y.var,x = predictor,
           model_type = "model2")
  return(rbind(lmer.metric.one %>% mutate(model_type = "model1"),
               res.2))
}
#####
# make model 3
lmer.results.m3 <- foreach(i = 3:15, .combine = rbind) %dopar% {
  y.var <- colnames(m125)[i]
  y <- scale(m125[,i] %>% as.matrix() %>% as.numeric(),scale = T, center = T)[,1]
  # make a model that uses all IQ scores together to predict language (model 3: multivariate)
  d2 <- m125 %>%
    select(age, sex, prompt, any_of(colnames(m125)[17:ncol(m125)]))%>%
    mutate(yy = y) %>%
    drop_na(yy)
  lm2 <- lmerTest::lmer(yy ~ PSI_composite_score + VCI_composite_score + WM_composite_score +
                          VC+SI+BD+MR+CD+FSIQ+VP+DS+SS+
                          PV_age_corrected_standard_score+flkr_age_corrected_standard_score+
                          wm_age_corrected_standard_score+card_age_corrected_standard_score+
                          pattern_age_corrected_standard_score+picture_age_corrected_standard_score+
                          reading_age_corrected_standard_score+fluid_age_corrected_standard_score+
                          crystallized_age_corrected_standard_score+cognition_tot_age_corrected_standard_score+
                          cognition_EC_tot_age_corrected_standard_score+
                          age + sex + age:sex +
                          (1|prompt),
                        data = d2 %>%
                          mutate_at(c(4:ncol(d2)), .funs = function(x) scale(x, scale = T, center = T)[,1]))
  res.2 <- jtools::summ(lm2, confin = T, pval = T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("predictor") %>%
    filter(predictor != "(Intercept)") %>%
    rename(Estimate = 2,confint_min = 3,confint_max = 4,pval = 7) %>%
    mutate(y = y.var,x = predictor,
           model_type = "model3")
  return(res.2)
}
#####
# combine all results of 3 models together
all.lmer <- rbind(lmer.results, 
                  lmer.results.m3 %>% # flip x and y labels, to fit it with the big plot
                    mutate(xx = y, y = x, x=xx) %>% select(-xx))
write_rds(all.lmer, "data/derivatives/lmer-results-models-1-2-3.rds", compress = "gz")
#####
# plot
all.lmer %>%
  filter(! x %in% c("age", "sexM", "age:sexM"),
         ! predictor %in% c("age", "sexM", "age:sexM"),
         ! grepl("abs", y), ! y %in% c("VCI_PSI", "PV_PS_age_corrected_standard_score")) %>%
  mutate(sig = ifelse(pval<0.05, "pval < 0.05", "pval \u2265 0.05")) %>%
  mutate(var = sub("_age_corrected_standard_score", "_NIH", y),
         cat2 = ifelse(grepl("NIH", var), "NIH-TB", "IQ"),
         var= sub("_NIH", "", var),
         var = factor(var, levels = unique(var)),
         model_type = factor(model_type, levels = c("model1", "model2", "model3"))) %>%
  # filter(cat2=="IQ") %>%
  filter(model_type %in% c("model1", "model2")) %>%
  ggplot(aes(x=Estimate, y=reorder(var, desc(model_type)),color = model_type)) +
  geom_point(aes(alpha = sig),  position = position_dodge(width = 0.6), size =2.5, show.legend = F) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.2, color = "red") +
  scale_alpha_manual(values = c("pval < 0.05" = 1, "pval \u2265 0.05" = 0.3), name ="") +
  ggh4x::facet_grid2(rows = vars(cat2), cols = vars(x), scales = "free") +
  scale_color_manual(values = six.colors)+
  geom_errorbarh(aes(xmin = confint_min, xmax = confint_max, alpha = sig), 
                 linewidth = 0.4, height = 0, 
                 position = position_dodge(width = 0.6)) +
  theme(panel.grid = element_line(linewidth = 0.1, colour = "grey"),
        strip.text.y.right = element_text(angle = 0)) +
  labs(x = "Estimate", y="",
       caption = paste0("n(samples): ", length(unique(m124$te_id)), "\n","\n",
                        "model 1: univariate (IQ ~ language metric)", "\n",
                        "model 2: multivariate (IQ ~ all language metrics)"
                        # , "\n","model 3: multivariate (language ~ all IQ scores)"
                        ))
ggsave("figs/lmer-iq-nih-all-metrics-random-prompt-all-models.png", bg = "white",
       width = 20, height = 15, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
# trying to make a clean fancy plot for these results
library(fmsb)
data <- rbind(rep(0.6, length(unique(lmer.results$x))-3),
              rep(-0.3, length(unique(lmer.results$x))-3),
              lmer.results %>% 
                filter(model_type=="model2",
                       ! predictor %in% c("age", "sexM", "age:sexM")) %>%
                select(y, x, value = Estimate) %>%
                filter(grepl("VCI_comp", y)|
                         grepl("PSI_comp", y)|
                         grepl("WM_comp", y)) %>% 
                distinct() %>%
                pivot_wider(names_from = "x", values_from = "value") %>%
                column_to_rownames("y"))
# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# plot with default options:
lab <- c(-0.3, -.2, -.1, 0, 0.1,0.2,0.3,0.4,0.5)
radarchart(data, axistype=1, seg = length(lab)-1, 
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=lab, cglwd=0.8,
           vlcex=0.7)
legend(x=1.2, y=1, legend = sub("_composite_score", "", rownames(data[-c(1,2),])), 
       bty = "n", pch=20 , col=colors_in , 
       text.col = "grey", cex=0.7, pt.cex=2)
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
m125 <- read_rds("data/derivatives/lmer-input-models-1-2-3.rds")
# try building a regression tree to predict the IQ/NIH-TB 
library(rpart)
i.tree <- rpart(VCI_composite_score ~ ., data = m125 %>%
                  select(colnames(m125)[3:15], VCI_composite_score))
summary(i.tree)
i.tree$cptable
# try bagging instead
library(caret)
all.trees <- list()
all.trees <- foreach(i = 30:43, .combine = rbind) %dopar% {
  y.var <- colnames(m125)[i]
  y <- m125[,i] %>% as.matrix() %>% as.numeric()
  # CV bagged model
  i.tree <- rpart(
    y ~ ., 
    data = m125 %>%
      select(colnames(m125)[3:15], colnames(m125)[17:27]) %>%
      mutate(y = y) %>%
      mutate_all(.funs = function(x) scale(x, scale = T, center = T)[,1]))
  # get the most important features used for prediction
  df <- data.frame(y = y.var,
                   i.tree$variable.importance %>%
                     as.data.frame() %>%
                     rename(var_importance = 1) %>%
                     rownames_to_column("var"))
  return(df)
}
all.trees %>%
  ggplot(aes(x=var_importance, y = reorder(var, desc(var_importance)))) +
  geom_point()+
  facet_wrap(~y, scales = "free")
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# show how are we doing to predict main indices in comparison to NIH-TB
# model 4: multivariate IQ ~ all language metrics + NIH-TB
# lmer here
lmer.results.m4 <- foreach(i = 30:33, .combine = rbind) %dopar% {
  y.var <- colnames(m125)[i]
  y <- scale(m125[,i] %>% as.matrix() %>% as.numeric(),scale = T, center = T)[,1]
  # make a model that uses all metrics and NIH-TB together to predict IQ (model 4: multivariate)
  d2 <- m125 %>%
    select(age, sex, any_of(c(colnames(all), colnames(combined.metrics),
                              colnames(m125)[17:27])))%>%
    mutate(yy = y) %>%
    drop_na(yy)
  lm2 <- lmerTest::lmer(yy ~ onset + 
                          # tot_comments + rep_prompt +
                          # rep_words_per_prompt +
                          rep_word_at_all + count_all +
                          full_euc_dist_normalized + 
                          # global_divergence + cos_similarity +
                          # vol_value + avg_lifetime + avg_switches + communities_count +
                          PV_age_corrected_standard_score+
                          # flkr_age_corrected_standard_score+
                          wm_age_corrected_standard_score+
                          # card_age_corrected_standard_score+
                          pattern_age_corrected_standard_score+
                          # picture_age_corrected_standard_score+
                          reading_age_corrected_standard_score+
                          # fluid_age_corrected_standard_score+
                          # crystallized_age_corrected_standard_score+
                          cognition_tot_age_corrected_standard_score+
                          # cognition_EC_tot_age_corrected_standard_score+
                          age + sex + age:sex +
                          (1|prompt),
                        data = d2 %>%
                          mutate_at(c(5:ncol(d2)), .funs = function(x) scale(x, scale = T, center = T)[,1]))
  res.2 <- jtools::summ(lm2, confin = T, pval = T)$coeftable %>%
    as.data.frame() %>%
    rownames_to_column("predictor") %>%
    filter(predictor != "(Intercept)") %>%
    rename(Estimate = 2,confint_min = 3,confint_max = 4,pval = 7) %>%
    mutate(y = y.var,x = predictor,
           model_type = "model4")
  return(res.2)
}
# trying to make a clean fancy plot for these results
library(fmsb)
data <- rbind(rep(1, length(unique(lmer.results.m4$x))-3),
              rep(-0.5, length(unique(lmer.results.m4$x))-3),
              lmer.results.m4 %>% 
                filter(! predictor %in% c("age", "sexM", "age:sexM"),
                       y!="FSIQ") %>%
                select(y, x, value = Estimate) %>%
                distinct() %>%
                mutate(y = sub("_composite_score", "", y),
                       x = sub("_age_corrected_standard_score", "", x)) %>%
                filter(!grepl("EC_", x)) %>%
                pivot_wider(names_from = "x", values_from = "value") %>%
                column_to_rownames("y"))
# Color vector
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
# plot with default options:
lab <- c(-0.6, -0.4,-0.2, 0, 0.2,0.4,0.6,0.8,1)
radarchart(data, axistype=1, seg = length(lab)-1, 
           pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", caxislabels=lab, cglwd=0.8,
           vlcex=0.9)
legend(x=1, y=1, legend = sub("_composite_score", "", rownames(data[-c(1,2),])), 
       bty = "n", pch=20 , col=colors_in , 
       text.col = "grey", cex=0.7, pt.cex=2)


library(ggside)
m126 <- inner_join(m125 %>% select(te_id, VCI_composite_score, PSI_composite_score, WM_composite_score,
                                   ends_with("_standard_score")) %>%
                     distinct(),
                   summarized.metrics) 
pp1 <- m126 %>%
  pivot_longer(cols = c(count_all, PV_age_corrected_standard_score, 
                        normalized_euc, community_count,
                        reading_age_corrected_standard_score, 
                        cognition_tot_age_corrected_standard_score), names_to = "var") %>%
  mutate(cat = ifelse(grepl("standard_score", var), "NIH-TB", "derived metrics")) %>%
  ggplot(aes(x=value, y = VCI_composite_score)) +
  geom_point() + geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  facet_wrap(~reorder(var, desc(cat)), scales = "free_x")+
  labs(x="") +
  geom_rug(alpha = 0.6)+
  theme_linedraw()
pp2 <- m126 %>%
  pivot_longer(cols = c(count_all, pattern_age_corrected_standard_score, 
                        community_count, fluid_age_corrected_standard_score,
                        divergence, 
                        cognition_tot_age_corrected_standard_score), names_to = "var") %>%
  mutate(cat = ifelse(grepl("standard_score", var), "NIH-TB", "derived metrics")) %>%
  ggplot(aes(x=value, y = PSI_composite_score)) +
  geom_point() + geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  facet_wrap(~reorder(var, desc(cat)), scales = "free_x")+
  labs(x="") +
  geom_rug(alpha = 0.6)+
  theme_linedraw()
pp3 <- m126 %>%
  pivot_longer(cols = c(wm_age_corrected_standard_score,
                        divergence, rep_words_per_prompt,
                        cognition_tot_age_corrected_standard_score), names_to = "var") %>%
  mutate(cat = ifelse(grepl("standard_score", var), "NIH-TB", "derived metrics")
         # ,value = ifelse(var == "rep_words_per_prompt", log2(value), value)
         ) %>%
  ggplot(aes(x=value, y = WM_composite_score)) +
  geom_point() + geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  facet_wrap(~reorder(var, desc(cat)), scales = "free_x")+
  # facet_grid(rows = vars(cat), cols = vars(var), scales = "free_x") +
  labs(x="") +
  geom_rug(alpha = 0.6)+
  theme_linedraw()
patchwork::wrap_plots(pp1 + labs(title = "A"),
                      pp2 + labs(title = "B"),
                      patchwork::wrap_plots(pp3 + labs(title = "C"), 
                                            patchwork::plot_spacer(),
                                            ncol = 2, widths = c(2,1)), 
                      ncol = 1)
ggsave("figs/scatterplot-lang-metrics-and-NIH-TB-predicting-IQ-main.png", bg = "white",
       width = 12, height = 18, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
# do anova to prove that a model of using our language metric is better than the NIH-TB
m1<-glm(VCI_composite_score ~ count_all, data = m126)
m2<-glm(VCI_composite_score ~ cognition_tot_age_corrected_standard_score, data = m126)
summary(m1)$aic
summary(m2)$aic
################################################################################
################################################################################
################################################################################
################################################################################
