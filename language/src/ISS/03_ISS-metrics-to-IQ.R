################################################################################
#            analyzing ISS features to predict IQ and NIH-TB scores            #
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
iss.feat <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 14) %>%
  drop_na(te_id) %>%
  mutate_at(.vars = c(4:10,12), .funs = function(x) as.numeric(x)) %>%
  rename(prompt3_section_3_score = prompt3_section_3) %>%
  left_join(demo)
################################################################################
################################################################################
################################################################################
# extract the bigrams and calculate their accuracy
l3 <- "the cafeteria doors, around which the seventh graders sometimes stand until the lunch bell rings, are always closed but never locked"
library(tidytext)
dd <- iss.feat %>%
  filter(task=="L3") %>%
  select(te_id, response) %>%
  mutate(response = tolower(response),
         response = str_remove_all(pattern = "\\\n", response),
         response = str_remove_all(pattern = "\\\r", response),
         response = gsub("[^[:alnum:] ]", "", response)) %>%
  unnest_tokens(word, response)
# Generate bigrams
dd.bigrams <- dd %>%
  group_by(te_id) %>%
  mutate(bigram = paste0(word, " ", lead(word))) %>%
  filter(!is.na(lead(word))) %>%
  ungroup()
prompt.bigrams <- data.frame(p = gsub("[^[:alnum:] ]", "", l3)) %>%
  unnest_tokens(word, p) %>%
  mutate(bigram = paste0(word, " ", lead(word))) %>%
  filter(!is.na(lead(word)))
# for each id, get number of bigrams available in prompt bigrams
bigrams.2 <- dd.bigrams %>%
  group_by(te_id) %>%
  filter(bigram %in% prompt.bigrams$bigram) %>%
  dplyr::summarise(accurate_bigram_score = n()) %>%
  mutate(accurate_bigram_percentage_score = accurate_bigram_score/nrow(prompt.bigrams))
####################
# combine the bigram accurace features with the rest
iss.feat <- left_join(iss.feat, bigrams.2)
################################################################################
################################################################################
################################################################################
# plot distributions, and correlations with demo
iss.feat %>%
  pivot_longer(cols = c(ends_with("score"), starts_with("error")), 
               names_to = "feature") %>%
  mutate(value = as.factor(value)) %>%
  filter(feature != "accurate_bigram_percentage_score") %>%
  ggplot(aes(x = value)) +
  geom_bar() +
  ggh4x::facet_grid2(rows = vars(task), cols = vars(feature), scales = "free",independent = T)+
  labs(x="score")
ggsave("figs/ISS/distribution-of-features.png", bg = "white",
       width = 14, height = 8, units = "in", dpi = 360)
#####
# check corr with demo
iss.feat %>%
  pivot_longer(cols = c(ends_with("score"), starts_with("error")), 
               names_to = "feature") %>%
  mutate(value = factor(value, levels = c(0:20))) %>%
  filter(feature != "accurate_bigram_percentage_score") %>%
  ggplot(aes(x = value, fill = sex)) +
  geom_bar() +
  scale_fill_manual(values = redblu.col) +
  ggh4x::facet_grid2(rows = vars(task), cols = vars(feature), scales = "free",independent = T)+
  labs(x="score")
ggsave("figs/ISS/distribution-of-features-w-sex-filling.png", bg = "white",
       width = 14, height = 8, units = "in", dpi = 360)
#####
# check age correlation?
iss.feat %>%
  pivot_longer(cols = c(ends_with("score"), starts_with("error")), 
               names_to = "feature") %>%
  mutate(value = factor(value, levels = c(0:20))) %>%
  filter(feature != "accurate_bigram_percentage_score") %>%
  ggplot(aes(x = value, y = age)) +
  geom_boxplot() +
  ggpubr::stat_compare_means(color = "red")+
  ggh4x::facet_grid2(rows = vars(task), cols = vars(feature), scales = "free",independent = T)+
  labs(x="score")
ggsave("figs/ISS/corr-of-features-w-age.png", bg = "white",
       width = 16, height = 8, units = "in", dpi = 360)
################################################################################
################################################################################
# correlate these scores with wm
# binary score from L2
p1 <- iss.feat %>%
  filter(task == "L2") %>%
  mutate(binary_score = as.factor(binary_score)) %>%
  left_join(m1.m2) %>%
  pivot_longer(cols = c(
    VCI_composite_score, PSI_composite_score,
    WM_composite_score, Digit_Span, picture_sequence_memory_test_age_corrected_standard_score),
    names_to = "iq", values_to = "val2") %>%
  filter(iq %in% c("WM_composite_score", "Digit_Span", "VCI_composite_score")) %>%
  ggplot(aes(x = binary_score, y = val2, fill=binary_score)) +
  geom_violin(show.legend = F) + geom_boxplot(width = 0.2, fill="white") + ggpubr::stat_compare_means(color = "red")+
  scale_fill_manual(values = boxplot.colors) +
  ggh4x::facet_grid2(cols = vars(iq), scales = "free",independent = T)+
  labs(y="IQ score",
       title = "correlations from SRT L2 data") +
  theme(axis.text.x.bottom = element_text(angle = 0, hjust = 0.5))
# scatterplot for L3 with errors of substitution and omission & scaled_score
p2 <- iss.feat %>%
  filter(task == "L3") %>%
  mutate(binary_score = ifelse(scaled_score>0, 1, 0)) %>%
  pivot_longer(cols = c(binary_score, scaled_score, starts_with("error"), accurate_bigram_score, prompt3_section_3_score), 
               names_to = "feature") %>%
  # mutate(value = factor(value, levels = c(0:17))) %>%
  mutate(value = as.numeric(value)) %>%
  left_join(m1.m2) %>%
  pivot_longer(cols = c(
    VCI_composite_score, PSI_composite_score,
    WM_composite_score, Digit_Span, picture_sequence_memory_test_age_corrected_standard_score),
    names_to = "iq", values_to = "val2") %>%
  filter(iq %in% c("Digit_Span", "WM_composite_score", "VCI_composite_score"),
         feature %in% c("scaled_score", "error_omission", "error_substitution", "accurate_bigram_score")) %>%
  ggplot(aes(y = value, x = val2)) +
  geom_point(position = "jitter") + geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red") +
  ggh4x::facet_grid2(cols = vars(iq), rows = vars(feature), scales = "free",independent = T)+
  labs(y="feature score", x = "IQ score",
       title = "correlations from SRT L3 data")
patchwork::wrap_plots(p2,
                      p1,
                      ncol = 1, heights = c(4,1))
ggsave("figs/ISS/corr-of-features-w-IQ.png", bg = "white",
       width = 10, height = 12, units = "in", dpi = 360)


iss.feat %>%
  group_by(te_id) %>% dplyr::summarise_at(.vars = c(colnames(iss.feat)[c(4:10, 17)]),
                                          .funs = function(x) mean(x, na.rm = T)) %>%
  pivot_longer(cols = c(binary_score, scaled_score, accurate_bigram_score,
                        error_omission, error_substitution), 
                        # starts_with("error")), 
               names_to = "feature") %>%
  mutate(value = as.numeric(value)) %>%
  left_join(m1.m2) %>%
  pivot_longer(cols = c(
    VCI_composite_score, PSI_composite_score,
    WM_composite_score, Digit_Span, picture_sequence_memory_test_age_corrected_standard_score),
    names_to = "iq", values_to = "val2") %>%
  # filter(iq %in% c("Digit_Span", "WM_composite_score", "VCI_composite_score"),
  #        feature %in% c("scaled_score", "error_omission", "error_substitution")) %>%
  ggplot(aes(y = value, x = val2)) +
  geom_point(position = "jitter") + geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red") +
  ggh4x::facet_grid2(cols = vars(iq), rows = vars(feature), scales = "free",independent = T)+
  labs(y="feature score", x = "IQ score",
       title = "correlations from SRT L3 data")
ggsave("figs/ISS/corr-of-features-w-IQ-extras-averaged-by-id.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
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