################################################################################
#            checking most&first used comm predict IQ and NIH-TB scores        #
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
comm.meta <- read_rds("data/derivatives/community-data.rds") %>% as.data.frame()
ps.vc <- read_rds("data/derivatives/ps-vc-text-analyzed.rds")
#############
################################################################################
################################################################################
comm.meta %>%
  left_join(m1.m2) %>%
    ggplot(aes(x=archetype, y = VCI_composite_score, fill = archetype)) +
  geom_violin()+
  geom_boxplot(width=0.2, fill="white")+
  ggpubr::stat_compare_means()+
  scale_fill_manual(values = ten.colors)
m123 <- left_join(comm.meta, m1.m2)
library(ggstatsplot)
ggbetweenstats(data = comm.meta %>%
                 left_join(m1.m2), 
               x = archetype,
               y = VCI_composite_score)

lapply(m123 %>% select(colnames(m1.m2), -ends_with("_id")), function(x) {
  ggbetweenstats(data = m123 %>%
                   mutate(y=as.numeric(x))%>%
                   group_by(te_id, word) %>%
                   slice_min(order_by = visit_start_time) %>%
                   ungroup(), 
                 x = archetype,
                 y = y, 
                 title = "archetype of first word used in response to prompt")
})
comm.meta %>%
  left_join(m1.m2) %>%
  group_by(te_id, word) %>%
  slice_min(order_by = visit_start_time) %>%
  ungroup() %>%
  pivot_longer(cols = c(colnames(m1.m2), -ends_with("_id")), values_to = "val", names_to = "measure") %>%
  ggplot(aes(x=archetype, y = val, fill=archetype))+
  geom_point()+
  geom_violin()+geom_boxplot(width = 0.2, fill="white")+ggpubr::stat_compare_means()+
  facet_wrap("measure", scales = "free")+
  scale_fill_manual(values = ten.colors)
################################################################################
colnames(ps.vc)
first.r <- ps.vc %>%
  group_by(te_id, word) %>%
  slice_min(order_by = start) %>%
  ungroup()
write_csv(first.r, "data/derivatives/first-response-per-prompt.csv")
################################################################################
################################################################################
# check the categories used from LIWC results
first.liwc <- read_csv("data/derivatives/first-response-per-prompt_LIWC-Analysis") %>%
  select(te_id, prompt = word, word = text, starts_with("nrc"), 44:62) %>%
  inner_join(m1.m2)
# examples
ex <- first.liwc %>%
  mutate_at(.vars = colnames(first.liwc)[4:32], .funs = function(x) factor(x)) %>%
  pivot_longer(cols = colnames(first.liwc)[4:32], names_to = "category", values_to = "score") %>%
  distinct(category, word, score) %>% filter(nchar(word)<12) %>%
  # pivot_wider(names_from = category, values_from = score, id_cols = word)
  group_by(category) %>% slice_max(n = 5, order_by = score, with_ties = F)
# plot
first.liwc %>%
  mutate_at(.vars = colnames(first.liwc)[4:32], .funs = function(x) factor(x)) %>%
  pivot_longer(cols = colnames(first.liwc)[4:32], names_to = "category", values_to = "score") %>%
  pivot_longer(cols = c(colnames(m1.m2), -ends_with("_id")), names_to = "measure", values_to = "cog_score") %>%
  mutate(measure = sub("_age_corrected_standard_score", "_NIH", measure),
         cat2 = ifelse(grepl("NIH", measure), "NIH-TB", "IQ"),
         measure = sub("_NIH", "", measure)) %>%
  filter(measure %in% c("VCI_composite_score", "PSI_composite_score", "WM_composite_score",
                        "FSIQ"),
         grepl("nrc", category)) %>%
  ggplot(aes(x=score, y = cog_score, fill = score)) +
  geom_violin()+geom_boxplot(width=0.1, fill="white")+
  ggpubr::stat_compare_means(color = "red", na.rm = T, size = 3) +
  ggh4x::facet_grid2(cols = vars(measure), rows = vars(category), scales = "free") +
  scale_fill_manual(values = boxplot.colors)+
  labs(title = "NRC categories of first word in response to a prompt")
ggsave("figs/nrc-cat-of-first-response.png", bg = "white",
       width = 9, height = 19, units = "in", dpi = 360)
# check the LIWC cat
first.liwc %>%
  mutate_at(.vars = colnames(first.liwc)[4:32], .funs = function(x) factor(x)) %>%
  pivot_longer(cols = colnames(first.liwc)[4:32], names_to = "category", values_to = "score") %>%
  pivot_longer(cols = c(colnames(m1.m2), -ends_with("_id")), names_to = "measure", values_to = "cog_score") %>%
  mutate(measure = sub("_age_corrected_standard_score", "_NIH", measure),
         cat2 = ifelse(grepl("NIH", measure), "NIH-TB", "IQ"),
         measure = sub("_NIH", "", measure)) %>%
  filter(measure %in% c("VCI_composite_score", "PSI_composite_score", "WM_composite_score",
                        "FSIQ"),
         !grepl("nrc", category)) %>%
  mutate(score = as.factor(ifelse(score == 0, 0, 1))) %>%
  ggplot(aes(x=score, y = cog_score, fill = score)) +
  geom_violin()+geom_boxplot(width=0.1, fill="white")+
  ggpubr::stat_compare_means(color = "red", na.rm = T, size = 3) +
  ggh4x::facet_grid2(cols = vars(measure), rows = vars(category), scales = "free") +
  scale_fill_manual(values = boxplot.colors)+
  labs(title = "LIWC categories of first word in response to a prompt")
ggsave("figs/liwc-cat-of-first-response.png", bg = "white",
       width = 9, height = 36, units = "in", dpi = 360)
# important ones with good numbers:
# perception
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
