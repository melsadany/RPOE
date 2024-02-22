################################################################################
#                  correlation between vitals and other features            #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/phenotypes"
setwd(project.dir)
################################################################################
################################################################################
# read tests data
nih.tb <- read_csv("../language/data/derivatives/nih-tb_clean.csv")
iq <- read_csv("../language/data/derivatives/wisc-and-wais_clean.csv")
################################################################################
# check correlation between NIH-TB and IQ
m1.m2 <- read_rds("../language/data/derivatives/m1m2.rds")
################################################################################
vitals <- readxl::read_xlsx("../language/data/raw/RPOE Stats.xlsx", sheet = 1)
colnames(vitals)[2:3] <- c("devGenes_id", "te_id")
vitals.clean <- vitals %>%
  select(devGenes_id, te_id,
         height = `Height (m)`, 
         weight = `Weight (kg)`, 
         HR = `Heart Rate/ Pulse Rate`, 
         Temp, D02, 
         head_circ = `Head Circ (in)`,
         HS = `Hand Strength`) %>%
  mutate_at(.vars = -c(1:2), .funs = function(x) as.numeric(x)) %>%
  mutate(height = height * 100)

m123 <- inner_join(vitals.clean, m1.m2)
corr.table(m123 %>%
             select(colnames(m1.m2), -ends_with("id")),
           m123 %>% select(colnames(vitals.clean), -ends_with("id"))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2)),
         V2 %in% c("height", "weight", "HR", "D02", "head_circ", "Temp", "HS")) %>%
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
  labs(x = "", y = "",
       caption = paste0("n(samples): ", nrow(m123), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave("figs/corr_iq-nih-vitals.png",
       width = 10, height = 8, units = "in", bg = "white", dpi = 320)
################################################################################
################################################################################
# combine nih percentiles and see correlation
nih.clean <- nih.tb %>%
  select(te_id, contains("percentile"), contains("age_corrected")) %>%
  rename_all(.funs = function(x) sub("_age_corrected_standard_score", "", x)) %>%
  rename_all(.funs = function(x) sub("_age_adjusted", "", x)) %>%
  rename_all(.funs = function(x) sub("_national", "", x))
m124 <- inner_join(vitals.clean,
                   nih.clean)
corr.table(m124 %>%
             select(colnames(nih.clean), -te_id, -devGenes_id),
           m124 %>% select(colnames(vitals.clean), -te_id, -devGenes_id)) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(nih.clean)),
         V2 %in% c(colnames(vitals.clean))) %>%
  mutate(cat2 = ifelse(grepl("percentile", V1), "NIH-TB age-adjusted percentiles", "NIH-TB age-adjusted scores"),
         V1 = sub("_percentile", "", V1),
         V1 = factor(V1, levels = unique(V1))) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(cols = vars(cat2),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "",
       caption = paste0("n(samples): ", nrow(m124), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave("figs/corr_nih-vitals.png",
       width = 10, height = 8, units = "in", bg = "white", dpi = 320)
m124 %>%
  pivot_longer(cols = colnames(vitals.clean)[-c(1:2)], names_to = "measure") %>%
  ggplot(aes(x=flkr_percentile, y=value)) +
  geom_point()+
  geom_smooth(method = "lm")+ ggpubr::stat_cor(color = "red") +
  facet_wrap(~measure, scales = "free_y")
m124 %>% 
  ggplot(aes(x=HS, y=weight)) +
  geom_point()+geom_smooth(method = "lm")+ ggpubr::stat_cor(color = "red")
m124 %>% 
  ggplot(aes(x=HS, y=height)) +
  geom_point()+geom_smooth(method = "lm")+ ggpubr::stat_cor(color = "red")
################################################################################
