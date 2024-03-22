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
m1.m2 <- read_rds("data/derivatives/m1m2-age-sex-corrected.rds") 
m1.m2.all <- read_rds("data/derivatives/m1m2-all-all.rds") 
demo <- read_rds("data/raw/demo.rds")
ps.vc <- read_rds("data/derivatives/ps-vc-text-clean.rds")
#####
pairs.raw <- read_rds("data/derivatives/cons-pairs.rds") %>% as.data.frame()
euc.raw <- read_rds("data/derivatives/euc-distance-w-minimum-of-2-words-per-task.rds") %>% as.data.frame() 
divergence.raw <- read_rds("data/derivatives/divergence-w-minimum-of-3-words-per-task.rds") %>% as.data.frame()
voc.depth.raw <- read_rds("data/derivatives/chulls.rds") %>% as.data.frame() %>%
  select(te_id, vol_source, vol_value)
comm.returns.raw <- read_rds("data/derivatives/comm-returns.rds") %>% as.data.frame()
comm.lifetime.raw <- read_rds("data/derivatives/community-data.rds") %>% as.data.frame() %>%
  filter(visit_lifetime>0)
#############
################################################################################
################################################################################
# clean pairs similarity, and correct for age and sex
pairs <- pairs.raw %>%
  filter(consec == T) %>%
  mutate(prompt = word) %>%
  group_by(te_id, prompt) %>%
  dplyr::summarise(cosine_similarity_avg_raw = mean(cos_similarity, na.rm = T)) %>% 
  ungroup() %>%
  left_join(demo)
pairs$cosine_similarity_avg <- residuals(glm(cosine_similarity_avg_raw ~ age + sex + age:sex,
                                             data = pairs))
################################################################################
################################################################################
# clean euclidean distance, and correct for age and sex
euc <- euc.raw %>%
  left_join(demo) %>%
  mutate(prompt = word)
euc$path_euclidean_distance <- residuals(glm(full_euc_dist_normalized ~ age + sex + age:sex,
                                             data = euc))
################################################################################
################################################################################
# clean divergence, and correct for age + sex
divergence <- divergence.raw %>%
  left_join(demo) %>%
  mutate(global_divergence_raw = global_divergence,
         prompt = word)
divergence$global_divergence <- residuals(glm(global_divergence_normalized ~ age + sex + age:sex, 
                                              data = divergence))
################################################################################
################################################################################
# clean vocabulary depth, and correct for age + sex
voc.depth <- voc.depth.raw %>%
  filter(vol_value > 0) %>%
  group_by(te_id) %>%
  dplyr::summarise(vocabulary_depth_raw = mean(vol_value, na.rm = T)) %>%
  left_join(demo)
voc.depth$vocabulary_depth <- residuals(glm(vocabulary_depth_raw ~ age, data = voc.depth))
################################################################################
################################################################################
# clean community lifetime, and correct for age + sex
comm.lifetime <- comm.lifetime.raw  %>% 
  select(te_id, prompt = word, archetype, visit_lifetime) %>%
  mutate(visit_lifetime = visit_lifetime/1000) %>%
  group_by(te_id, prompt, archetype) %>%
  dplyr::summarise(tot_archetype_lifetime = sum(visit_lifetime, na.rm = T)) %>%
  ungroup() %>% group_by(te_id, prompt) %>%
  dplyr::summarise(avg_community_lifetime_raw = mean(tot_archetype_lifetime, na.rm = T)) %>%
  left_join(demo)
comm.lifetime$avg_community_lifetime <- residuals(glm(avg_community_lifetime_raw ~ age + sex + age:sex,
                                                      data = comm.lifetime))
################################################################################
################################################################################
# clean community returns, and correct for age+sex
comm.returns <- comm.returns.raw %>%
  select(te_id, prompt = word, archetype, switches) %>%
  group_by(te_id, prompt) %>%
  dplyr::summarise(avg_switches_raw = sum(switches, na.rm = T)) %>%
  left_join(demo)
comm.returns$community_switches <- residuals(glm(avg_switches_raw ~ age + sex + age:sex,
                                                 data = comm.returns))
################################################################################
################################################################################
# clean community count, and correct for age + sex
comm.count <- comm.returns.raw %>%
  select(te_id, prompt = word, archetype) %>%
  distinct() %>% group_by(te_id, prompt) %>%
  dplyr::summarise(communities_count_raw = n()) %>%
  left_join(demo)
comm.count$communities_count <- residuals(glm(communities_count_raw ~ age + sex + age:sex, 
                                              data = comm.count))
################################################################################
################################################################################
# get word count per prompt per participant, and correct for age + sex
count.words <- ps.vc %>%
  mutate(text = tolower(text)) %>%
  select(te_id, prompt = word, text) %>%
  distinct() %>%
  group_by(te_id, prompt) %>%
  dplyr::summarise(unique_word_count_raw = n()) %>%
  left_join(demo)
count.words$unique_word_count <- residuals(glm(unique_word_count_raw ~ age + sex + age:sex, data = count.words))
################################################################################
################################################################################
# get the latency time to say the first word per task
onset <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
  mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
                       ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
                              ifelse(grepl("W\\?", text_revised), NA, text_revised))),
         word = ifelse(is.na(word_revised), word, word_revised)) %>%
  drop_na(text) %>%
  mutate(text = tolower(text), word = tolower(word)) %>%
  filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
  select(te_id=ID, task, prompt = word, text, start, end) %>%
  group_by(te_id, prompt) %>%
  slice_head(n=2) %>%
  filter(prompt!=text) %>%
  slice_head(n=1) %>%
  mutate(onset = start/1000) %>%
  drop_na() %>%
  left_join(demo) %>%
  mutate(onset_raw = onset)
onset$onset <- residuals(glm(onset_raw ~ age + sex + age:sex, 
                             data = onset))
################################################################################
################################################################################
# identify number of comments said by participant
comments <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
  mutate(comment = ifelse(comment == "T", 1, 0), te_id = ID) %>%
  group_by(te_id) %>%
  dplyr::summarise(comments_made_binary = ifelse(sum(comment, na.rm = T)>0, 1, 0)) %>% 
  ungroup() %>%
  left_join(demo)
################################################################################
################################################################################
# # identify off-target in COWAT
# off.target <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
#   mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
#                        ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
#                               ifelse(grepl("W\\?", text_revised), NA, text_revised))),
#          word = ifelse(is.na(word_revised), word, word_revised)) %>%
#   drop_na(text) %>%
#   mutate(text = tolower(text)) %>%
#   filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
#   select(te_id=ID, task, word, text) %>%
#   filter(task == 3) %>%
#   mutate(off_target = ifelse(str_sub(text, start = 1, end = 1) == tolower(word), F, T)) %>%
#   group_by(te_id, word) %>%
#   dplyr::summarise(off_target = sum(off_target, na.rm = T))
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
  select(te_id=ID, task, prompt = word, text) %>%
  group_by(te_id) %>%
  dplyr::summarise(repeats_prompt = ifelse(sum(ifelse(text == tolower(prompt), T, F))>0, 1,0)) %>%
  left_join(demo)
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
  select(te_id=ID, task, prompt = word, text) %>%
  filter(nchar(prompt)==1) %>%
  group_by(te_id) %>%
  dplyr::summarise(rep_words_per_prompt_raw = ifelse(sum(duplicated(text), na.rm = T)>0, 1, 0)) %>%
  left_join(demo)
rep.words$repeats_words_per_prompt <- residuals(glm(rep_words_per_prompt_raw ~ age , data = rep.words))
# recycling words in all tasks
# rep.words.recycled <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 13) %>%
#   mutate(text = ifelse(is.na(text_revised), text, # this section is for the manually checked transcription
#                        ifelse(grepl("F", text_revised)&nchar(text_revised)==1, NA,
#                               ifelse(grepl("W\\?", text_revised), NA, text_revised))),
#          word = ifelse(is.na(word_revised), word, word_revised)) %>%
#   drop_na(text) %>%
#   mutate(text = tolower(text)) %>%
#   filter(!text %in% c("uh", "um", "oh", "eh", "hmm", "hmmm")) %>% # drop the uh/hmm/um from text analysis
#   select(te_id=ID, task, word, text) %>%
#   group_by(te_id) %>%
#   dplyr::summarise(count_all = n(),
#                    rep_word_at_all = sum(duplicated(text), na.rm = T))
################################################################################
################################################################################
################################################################################
# # combine all of these features in one df
# all <- full_join(onset %>% select(te_id, prompt, onset), 
#                  comments %>% select(te_id, comments_made_binary)) %>%
#   full_join(rep.prompt) %>%
#   full_join(rep.words %>% select(te_id, rep_words_per_prompt))
# avg <- all %>%
#   group_by(te_id) %>%
#   dplyr::summarise_at(.vars = colnames(all)[3:ncol(all)], .funs = function(x) mean(x, na.rm = T))
# avg.t1 <- all %>%
#   filter(nchar(prompt)>1) %>% select(-off_target) %>%
#   group_by(te_id) %>%
#   dplyr::summarise_at(.vars = vars(2:(ncol(all)-2)), .funs = function(x) mean(x, na.rm = T))
# avg.t3 <- all %>%
#   filter(nchar(prompt)==1) %>%
#   group_by(te_id) %>%
#   dplyr::summarise_at(.vars = colnames(all)[3:ncol(all)], .funs = function(x) mean(x, na.rm = T))
################################################################################
################################################################################
# combine all of these features in one df
combined.metrics <- euc %>% select(te_id, prompt, path_euclidean_distance) %>%
  full_join(divergence %>% select(te_id, prompt, global_divergence)) %>%
  full_join(pairs  %>% select(te_id, prompt, cosine_similarity_avg)) %>%
  full_join(voc.depth  %>% select(te_id, vocabulary_depth)) %>%
  full_join(comm.lifetime %>% select(te_id, prompt, avg_community_lifetime)) %>%
  full_join(comm.returns %>% select(te_id, prompt, community_switches)) %>%
  full_join(comm.count %>% select(te_id, prompt, communities_count)) %>%
  full_join(onset %>% select(te_id, prompt,onset) %>% mutate(prompt = ifelse(nchar(prompt)==1, toupper(prompt), prompt))) %>%
  full_join(comments %>% select(te_id, comments_made_binary)) %>%
  full_join(rep.prompt %>% select(te_id, repeats_prompt)) %>%
  full_join(rep.words %>% select(te_id, repeats_words_per_prompt)) %>%
  full_join(count.words %>% select(te_id, prompt, unique_word_count)) %>%
  filter(!(te_id == "2E_040" & nchar(prompt)>1))
# average these features per te_id
summarized.metrics <- combined.metrics %>%
  ungroup() %>% select(-prompt) %>% group_by(te_id) %>%
  dplyr::summarise_all(.funs = function(x) mean(x, na.rm = T))
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
                        "data was averaged from all prompts after being corrected for age and sex if necessary", "\n",
                        "    ** FDR < 0.05", "\n",
                        "    .   pval < 0.05")) +
  my.guides
ggsave("figs/corr-iq-nih-averaged-metrics.png", bg = "white",
       width = 8, height = 8, units = "in", dpi = 360)
################################################################################
# try to check how these metrics can predict IQ/NIH-TB, but do a lmer instead of spearman
m124 <- combined.metrics %>%
  distinct() %>%
  full_join(m1.m2) %>% ungroup() %>% distinct()  %>%
  left_join(demo[,c(2,4,5)])
write_rds(m124, "data/derivatives/all-summarized-metrics.rds")
# make heatmap of correlation between vars
corr.table(m124 %>% 
             select(3:10),
           m124 %>%
             select(11:14)) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(m124)[3:14],
         V2 %in% colnames(m124)[3:14],
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
  drop_na(dev_id)
write_rds(m125, "data/derivatives/lmer-input-models-1-2-3.rds", compress = "gz")
# try the 3 different models for prediction and plot them side by side
# model 1: univariate for IQ and language
# model 2: multivariate IQ ~ all language
# model 3: multivariate language ~ all IQ
# lmer here
lmer.results <- foreach(i = 16:(ncol(m125)-2), .combine = rbind) %dopar% {
  y.var <- colnames(m125)[i]
  y <- scale(m125[,i] %>% as.matrix() %>% as.numeric(),scale = T, center = T)[,1]
  # do a one to one model (model 1: univariate)
  lmer.metric.one <- foreach(j = 3:14, .combine = rbind) %dopar% {
    x.var <- colnames(m125)[j]
    x <- scale(m125[,j] %>% as.matrix() %>% as.numeric(), scale = T, center = T)[,1]
    lm <- lmerTest::lmer(yy ~ xx + (1|prompt), 
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
    select(age, sex, any_of(colnames(combined.metrics)))%>%
    mutate(yy = y) %>%
    drop_na(yy)
  lm2 <- lmerTest::lmer(yy ~ onset + comments_made_binary + repeats_prompt +
                          repeats_words_per_prompt + unique_word_count +
                          path_euclidean_distance + global_divergence + cosine_similarity_avg +
                          vocabulary_depth + avg_community_lifetime + community_switches + communities_count + 
                          # age + sex + age:sex +
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
lmer.results.m3 <- foreach(i = 3:14, .combine = rbind) %dopar% {
  y.var <- colnames(m125)[i]
  y <- scale(m125[,i] %>% as.matrix() %>% as.numeric(),scale = T, center = T)[,1]
  # make a model that uses all IQ scores together to predict language (model 3: multivariate)
  d2 <- m125 %>%
    select(age, sex, prompt, any_of(colnames(m125)[16:ncol(m125)]))%>%
    mutate(yy = y) %>%
    drop_na(yy)
  lm2 <- lmerTest::lmer(yy ~ PSI_composite_score + VCI_composite_score + WM_composite_score +
                          Vocabulary+Similarities+Block_Design+Matrix_Reasoning+Coding+FSIQ+Visual_Puzzles+Digit_Span+Symbol_Search+
                          picture_vocabulary_age_corrected_standard_score+flanker_inhibitory_control_age_corrected_standard_score+
                          list_sorting_wm_age_corrected_standard_score+dimensional_change_card_sort_age_corrected_standard_score+
                          pattern_comparison_PS_age_corrected_standard_score+picture_sequence_memory_test_age_corrected_standard_score+
                          oral_reading_recognition_age_corrected_standard_score+cognition_fluid_composite_age_corrected_standard_score+
                          cognition_crystallized_composite_age_corrected_standard_score+cognition_total_composite_age_corrected_standard_score+
                          cognition_early_childhood_composite_age_corrected_standard_score+
                          # age + sex + age:sex +
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
  # filter(model_type %in% c("model1", "model2")) %>%
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
                        , "\n","model 3: multivariate (language ~ all IQ scores)"
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
lmer.results.m4 <- foreach(i = 29:32, .combine = rbind) %dopar% {
  y.var <- colnames(m125)[i]
  y <- scale(m125[,i] %>% as.matrix() %>% as.numeric(),scale = T, center = T)[,1]
  # make a model that uses all metrics and NIH-TB together to predict IQ (model 4: multivariate)
  d2 <- m125 %>%
    select(-dev_id, -sex) %>%
    mutate(yy = y) %>%
    drop_na(yy)
  lm2 <- lmerTest::lmer(yy ~ onset + 
                          comments_made_binary + repeats_prompt +
                          repeats_words_per_prompt +
                          unique_word_count +
                          path_euclidean_distance + 
                          global_divergence +
                          cosine_similarity_avg +
                          vocabulary_depth + avg_community_lifetime +
                          community_switches + communities_count +
                          picture_vocabulary_age_corrected_standard_score+
                          # flanker_inhibitory_control_age_corrected_standard_score+
                          list_sorting_wm_age_corrected_standard_score+
                          # dimensional_change_card_sort_age_corrected_standard_score+
                          pattern_comparison_PS_age_corrected_standard_score+
                          # picture_sequence_memory_test_age_corrected_standard_score+
                          oral_reading_recognition_age_corrected_standard_score+
                          # cognition_fluid_composite_age_corrected_standard_score+
                          # cognition_crystallized_composite_age_corrected_standard_score+
                          cognition_total_composite_age_corrected_standard_score+
                          # cognition_early_childhood_composite_age_corrected_standard_score+
                          # age + sex + age:sex +
                          (1|prompt),
                        data = d2 %>%
                          mutate_at(c(3:ncol(d2)), .funs = function(x) scale(x, scale = T, center = T)[,1]))
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
legend(x=1.1, y=1, legend = sub("_composite_score", "", rownames(data[-c(1,2),])), 
       bty = "n", pch=20 , col=colors_in , 
       text.col = "grey", cex=0.7, pt.cex=2)
################################################################################
################################################################################
# show how are we doing to predict main indices in comparison to NIH-TB
# univariate scatterplots
library(ggside)
m126 <- inner_join(m125 %>% select(te_id, VCI_composite_score, PSI_composite_score, WM_composite_score,
                                   Coding,Digit_Span,Symbol_Search,VCI_PSI,FSIQ,
                                   ends_with("_standard_score")) %>%
                     distinct(),
                   summarized.metrics) 
pp1 <- m126 %>%
  pivot_longer(cols = c(unique_word_count, picture_vocabulary_age_corrected_standard_score, 
                        path_euclidean_distance, communities_count, 
                        oral_reading_recognition_age_corrected_standard_score, 
                        cognition_total_composite_age_corrected_standard_score), names_to = "var") %>%
  mutate(cat = ifelse(grepl("standard_score", var), "NIH-TB", "derived metrics")) %>%
  mutate(var = sub("_age_corrected_standard_score", "", var)) %>%
  ggplot(aes(x=value, y = VCI_composite_score)) +
  geom_point() + geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  facet_wrap(~reorder(var, desc(cat)), scales = "free_x")+
  labs(x="", y="Verbal Comprehension Index") +
  geom_rug(alpha = 0.6)+
  theme_linedraw()
pp2 <- m126 %>%
  pivot_longer(cols = c(communities_count, pattern_comparison_PS_age_corrected_standard_score, 
                        onset, cognition_fluid_composite_age_corrected_standard_score,
                        community_switches,
                        cognition_total_composite_age_corrected_standard_score), names_to = "var") %>%
  mutate(cat = ifelse(grepl("standard_score", var), "NIH-TB", "derived metrics")) %>%
  mutate(var = sub("_age_corrected_standard_score", "", var)) %>%
  ggplot(aes(x=value, y = Coding)) +
  geom_point() + geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  facet_wrap(~reorder(var, desc(cat)), scales = "free_x")+
  labs(x="", y = "Coding (Processing Speed)") +
  geom_rug(alpha = 0.6)+
  theme_linedraw()
pp3 <- m126 %>%
  pivot_longer(cols = c(list_sorting_wm_age_corrected_standard_score,
                        global_divergence, unique_word_count, vocabulary_depth, 
                        cognition_total_composite_age_corrected_standard_score), names_to = "var") %>%
  mutate(cat = ifelse(grepl("standard_score", var), "NIH-TB", "derived metrics")
         # ,value = ifelse(var == "rep_words_per_prompt", log2(value), value)
         ) %>%
  mutate(var = sub("_age_corrected_standard_score", "", var)) %>%
  ggplot(aes(x=value, y = WM_composite_score)) +
  geom_point() + geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  facet_wrap(~reorder(var, desc(cat)), scales = "free_x")+
  # facet_grid(rows = vars(cat), cols = vars(var), scales = "free_x") +
  labs(x="", y = "Working Memory Index") +
  geom_rug(alpha = 0.6)+
  theme_linedraw()
patchwork::wrap_plots(pp1 + labs(title = "A"),
                      pp2 + labs(title = "B"),
                      pp3 + labs(title = "C"),
                      ncol = 1)
ggsave("figs/scatterplot-lang-metrics-and-NIH-TB-predicting-IQ-main.png", bg = "white",
       width = 12, height = 18, units = "in", dpi = 360)
# discrepency
p1p <- m126 %>%
  pivot_longer(cols = c(picture_vocabulary_age_corrected_standard_score,
                        global_divergence, path_euclidean_distance,
                        cognition_total_composite_age_corrected_standard_score), names_to = "var") %>%
  mutate(cat = ifelse(grepl("standard_score", var), "NIH-TB", "derived metrics")) %>%
  mutate(var = sub("_age_corrected_standard_score", "", var)) %>%
  ggplot(aes(x=value, y = VCI_PSI)) +
  geom_point() + geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  facet_wrap(~reorder(var, desc(cat)), scales = "free_x")+
  labs(x="", y = "VCI-PSI") +
  geom_rug(alpha = 0.6)+
  theme_linedraw()
p2p <- m126 %>%
  mutate(comments_made_binary = ifelse(comments_made_binary==1, T, F)) %>%
  ggplot(aes(x=comments_made_binary, y = VCI_PSI, fill = comments_made_binary))+
  geom_violin(show.legend = F) + geom_boxplot(width = 0.2, fill = "white")+
  ggpubr::stat_compare_means(color = "red")+
  scale_fill_manual(values = six.colors[c(1,4)]) +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))+
  labs(x="making comments")
patchwork::wrap_plots(p1p,patchwork::wrap_plots(patchwork::plot_spacer(), p2p, ncol = 1),
                      widths = c(2,1))
ggsave("figs/scatterplot-lang-metrics-and-NIH-TB-predicting-discrepency.png", bg = "white",
       width = 10, height = 8, units = "in", dpi = 360)

####
# make a bigger plot for all, with keeping ones of interest
m126 %>%
  pivot_longer(cols = c(picture_vocabulary_age_corrected_standard_score,
                        cognition_total_composite_age_corrected_standard_score,
                        oral_reading_recognition_age_corrected_standard_score,
                        pattern_comparison_PS_age_corrected_standard_score,
                        cognition_fluid_composite_age_corrected_standard_score,
                        list_sorting_wm_age_corrected_standard_score,
                        communities_count, unique_word_count,
                        global_divergence, path_euclidean_distance,
                        community_switches, onset,
                        vocabulary_depth), names_to = "var") %>%
  pivot_longer(cols = c(VCI_composite_score, 
                        Coding, 
                        WM_composite_score,
                        FSIQ), names_to = "measure", values_to = "score") %>%
  select(var, value, measure, score) %>%
  mutate(cat = ifelse(grepl("standard_score", var), "NIH-TB", "derived metrics"),
         measure = factor(measure, levels = c("FSIQ", "VCI_composite_score", 
                                              "Coding", "WM_composite_score"))) %>%
  mutate(keep = ifelse((measure == "VCI_composite_score" & 
                          var %in% c("cognition_total_composite_age_corrected_standard_score",
                                     "oral_reading_recognition_age_corrected_standard_score",
                                     "picture_vocabulary_age_corrected_standard_score",
                                     "communities_count", "path_euclidean_distance", "unique_word_count")) |
                         (measure == "Coding" & 
                            var %in% c("cognition_total_composite_age_corrected_standard_score",
                                       "pattern_comparison_PS_age_corrected_standard_score",
                                       "cognition_fluid_composite_age_corrected_standard_score",
                                       "communities_count", "community_switches", "onset")) |
                         (measure == "WM_composite_score" & 
                            var %in% c("cognition_total_composite_age_corrected_standard_score",
                                       "list_sorting_wm_age_corrected_standard_score",
                                       "global_divergence",
                                       "unique_word_count", "vocabulary_depth")) |
                         (measure == "FSIQ" & 
                            var %in% c("cognition_total_composite_age_corrected_standard_score",
                                       "cognition_fluid_composite_age_corrected_standard_score",
                                       "communities_count", "global_divergence",
                                       "unique_word_count", "vocabulary_depth")), T, F)) %>%
  filter(keep == T) %>%
  mutate(var = sub("_age_corrected_standard_score", "", var)) %>%
  group_by(measure) %>% mutate(score = scale(score, scale = T, center = T)[,1]) %>% ungroup() %>%
  group_by(var) %>% mutate(value = scale(value, scale = T, center = T)[,1]) %>% ungroup() %>%
  ggplot(aes(x=score, y = value)) +
  # ggplot(aes(x=value, y = score)) +
  geom_point() + geom_smooth(method = "lm", color = six.colors[3]) +
  ggpubr::stat_cor(color = "red") +
  ggh4x::facet_grid2(cols = vars(measure), rows = vars(reorder(var, desc(cat))),
                     scales = "free", render_empty = F)+
  # ggh4x::facet_nested_wrap(~measure+cat+var, nrow = 4)+
  labs(x="Z-transformed derived metric score", y = "Z-transformed IQ score") +
  geom_rug(alpha = 0.6)+
  theme_linedraw() +
  theme(strip.text.y.right = element_text(angle = 0))
ggsave("figs/scatterplot-lang-metrics-and-NIH-TB-predicting-main-V2.png", bg = "white",
       width = 13, height = 20, units = "in", dpi = 360)
# ggsave("figs/scatterplot-lang-metrics-and-NIH-TB-predicting-main-V3.png", bg = "white",
#        width = 18, height = 12, units = "in", dpi = 360)
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
################################################################################
################################################################################
################################################################################
########################## CCA for all metrics with IQ #########################
################################################################################
################################################################################
################################################################################
summarized.metrics.2 <- summarized.metrics %>%
  left_join(demo)
  
# summarized.metrics.res <- summarized.metrics.2 %>% 
#   mutate_at(.vars = vars(colnames(summarized.metrics), -te_id), 
#             .funs = function(x) scale(residuals(glm(y ~ age + sex + age:sex,
#                                                     data = data.frame(y = scale(x, scale = T, center = T)[,1] %>% as.numeric() , 
#                                                                       age = summarized.metrics.2$age,
#                                                                       sex = summarized.metrics.2$sex))),
#                                       scale = T, center = T)[,1])
m127 <- inner_join(m1.m2, summarized.metrics.2)
################################################################################
################################################################################
# do CCA for two blocks: IQ, and measured language metrics
library(RGCCA)
c1 <- m127 %>% 
  select(colnames(m1.m2), -ends_with("id"))
c2 <- m127 %>% 
  select(any_of(colnames(summarized.metrics)), -te_id)
cca.cv <- rgcca_cv(blocks = list(iq = c1,
                                 language = c2),
                   ncomp = 10, verbose = T, method = "sgcca",
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
  labs(y = "", x = "") +
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
# screeplot
data.frame(CC = c(1:10), 
           iq_AVE = all.cca$AVE$AVE_X$iq *100,
           lang_AVE = all.cca$AVE$AVE_X$language *100) %>%
  pivot_longer(cols = c(2,3), names_to = "source") %>%
  ggplot(aes(x=CC, y = value))+
  geom_point()+
  geom_line()+
  facet_wrap(~source)+
  scale_x_continuous(labels = paste("C", c(1:10)), breaks = c(1:10)) +
  labs(y="average variance explained", x="") +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
ggsave("figs/CCA-screeplot.png", bg = "white",
       width = 10, height = 7, units = "in", dpi=360)
################################################################################
################################################################################
################################################################################
################################################################################
