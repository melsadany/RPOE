################################################################################
#              correlation between facial landmarks and other features         #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = lubridate::interval(dob, age.day) / lubridate::duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/photographs"
setwd(project.dir)
################################################################################
# read tests data
m1.m2 <- read_rds("../language/data/derivatives/m1m2.rds")
################################################################################
# get PS-VC performance summary
ps.summ <- read_rds("../language/data/derivatives/ps-vc-summary-data.rds") %>%
  select(te_id, starts_with("nrc"), contains("count"), "avg_wait") %>%
  ungroup()
################################################################################
# get participants' metadata
meta <- readxl::read_xlsx("../language/data/raw/RPOE_participants_metadata.xlsx", sheet = 1) %>%
  drop_na(DOB) %>%
  mutate(age = age(DOB, floor = F))
vitals <- readxl::read_xlsx("../language/data/raw/RPOE Stats.xlsx", sheet = 1) %>%
  select(devGenes_id = 2, height = 5, weight =6, head_circ=7) %>%
  mutate(height = as.numeric(height), weight = as.numeric(weight), head_circ = as.numeric(head_circ)) %>%
  drop_na() %>%
  mutate(bmi = weight/(height^2))
meta <- full_join(meta, vitals)
################################################################################
################################################################################
############################ correlation with distances ########################
################################################################################
################################################################################
# get pairs distances
int.pairs <- c("EB_R", "EB_L", "E_R", "E_L", "M_H", "N_V", "N_H_B", "M_V", "EB_C","EB_N_R", "EB_N_L", "EB_E_R", "EB_E_L","NT_E_R", "NT_E_L","EB_M_R", "EB_M_L","E_M_R", "E_M_L", "N_H_A")
pairs.dis <- read_csv("data/derivatives/pairs-distances.csv") %>%
  select(te_id, paste0("P_", int.pairs)) %>% # only keep distances of int
  left_join(meta %>% select(te_id, age, sex, bmi, Race))

# plot correlation bet distances and age
pairs.dis %>%
  pivot_longer(cols = starts_with("P_"), names_to = "pair", values_to = "v1") %>%
  ggplot(aes(x=age, y=v1)) +
  geom_point()+geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red")+
  facet_wrap(~pair, scales = "free") + labs(y="measured distance")
ggsave("figs/corr_facial-distances-and-age.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
# plot correlation bet distances and bmi
pairs.dis %>%
  pivot_longer(cols = starts_with("P_"), names_to = "pair", values_to = "v1") %>%
  ggplot(aes(x=bmi, y=v1)) +
  geom_point()+geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red")+
  facet_wrap(~pair, scales = "free") + labs(y="measured distance")
ggsave("figs/corr_facial-distances-and-bmi.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
# plot correlation bet distances and sex
pairs.dis %>%
  pivot_longer(cols = starts_with("P_"), names_to = "pair", values_to = "v1") %>%
  ggplot(aes(x=sex, y=v1)) +
  geom_violin(aes(fill = sex))+geom_boxplot(width = 0.1, fill = "white")+
  ggpubr::stat_compare_means(color = "red", size = 3)+
  facet_wrap(~pair, scales = "free") + labs(y="measured distance")
ggsave("figs/corr_facial-distances-and-sex.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
# plot correlation bet distances and race
pairs.dis %>%
  pivot_longer(cols = starts_with("P_"), names_to = "pair", values_to = "v1") %>%
  ggplot(aes(x=Race, y=v1, fill = Race)) +
  geom_violin()+geom_boxplot(width = 0.1, fill = "white")+
  ggpubr::stat_compare_means(color = "red", size = 3)+
  facet_wrap(~pair, scales = "free_y") + labs(y="measured distance")
ggsave("figs/corr_facial-distances-and-race.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)

# correct pairs distances for age, sex, and interaction
# 
res.pairs <- cbind(te_id = pairs.dis$te_id,
                   apply(pairs.dis %>% select(starts_with("P_")), MARGIN = 2, FUN = function(x) {
                     residuals(glm(y ~ age + sex + age:sex + bmi + Race, 
                                   data = cbind(pairs.dis %>% select(age, sex, bmi,Race) %>%
                                                  mutate(sex = as.factor(sex)),
                                                y = x)))
                     }) %>%
                     as.data.frame())
# if decided not to correct, run this
# res.pairs <- pairs.dis
################################################################################
# get correlations between distances and IQ/NIH-TB
m123 <- inner_join(m1.m2, res.pairs)
corr.table(m123 %>% select(any_of(c(colnames(m1), colnames(m2))),
                           -ends_with("id")),
           m123 %>% select(starts_with("P_")),
           method = "pearson") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1), colnames(m2)), 
         V2 %in% colnames(pairs.dis)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = sub("P_", "", V2),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=reorder(V2, desc(V2)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  facet_wrap(~cat2, scales = "free", nrow = 1) +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "pair # of two facial landmarks points",
       caption = paste0("n(samples): ", nrow(m123), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_facial-distances-IQ-",
                         # "no-correction",
                         # "age-sex-inter-bmi-corrected",
                         "age-sex-inter-bmi-race-corrected",
                         ".png"),
       width = 8, height = 8, units = "in", dpi = 320, bg = "white")
################################################################################

################################################################################
################################################################################
############################## correlation with areas ##########################
################################################################################
################################################################################
# get facial areas
areas <- read_csv("data/derivatives/facial.areas.csv") %>%
  left_join(meta %>% select(te_id, age, sex, bmi, Race))


# plot correlation bet areas and age
areas %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area", values_to = "v1") %>%
  ggplot(aes(x=age, y=v1)) +
  geom_point()+ geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red")+
  facet_wrap(~area, scales = "free") + labs(y="measured area")
ggsave("figs/corr_facial-areas-and-age.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
# plot correlation bet areas and bmi
areas %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area", values_to = "v1") %>%
  ggplot(aes(x=bmi, y=v1)) +
  geom_point()+ geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red")+
  facet_wrap(~area, scales = "free") + labs(y="measured area")
ggsave("figs/corr_facial-areas-and-bmi.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
# plot correlation bet areas and sex
areas %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area", values_to = "v1") %>%
  ggplot(aes(x=sex, y=v1)) +
  geom_violin(aes(fill = sex))+geom_boxplot(width = 0.1, fill = "white")+
  ggpubr::stat_compare_means(color = "red", size = 3)+
  facet_wrap(~area, scales = "free") + labs(y="measured area")
ggsave("figs/corr_facial-areas-and-sex.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)
# plot correlation bet areas and race
areas %>%
  pivot_longer(cols = starts_with("A_"), names_to = "area", values_to = "v1") %>%
  ggplot(aes(x=Race, y=v1, fill = Race)) +
  geom_violin()+geom_boxplot(width = 0.1, fill = "white")+
  ggpubr::stat_compare_means(color = "red", size = 3)+
  facet_wrap(~area, scales = "free_y") + labs(y="measured area")
ggsave("figs/corr_facial-areas-and-race.png", bg = "white",
       width = 12, height = 12, units = "in", dpi = 360)


# correct areas for age, sex, and interaction
res.areas <- cbind(te_id = areas$te_id,
                   apply(areas %>% select(starts_with("A_")), MARGIN = 2, FUN = function(x) {
                     residuals(glm(y ~ age + sex + age:sex + bmi + Race, 
                                   data = cbind(areas %>% select(age, sex, bmi, Race) %>%
                                                  mutate(sex = as.factor(sex)),
                                                y = x)))
                     }) %>%
                     as.data.frame()) %>%
  # get asymmetry score as the average difference between R and left areas
  mutate(asym = ((A_ES_R + A_E_R + A_CHK_I_R + A_N_R + A_CHK_O_R + A_M_R) - 
                   (A_ES_L + A_E_L + A_CHK_I_L + A_N_L + A_CHK_O_L + A_M_L))) 
# if decided not to correct, run this
# res.areas <- areas %>%
#   mutate(asym = ((A_ES_R + A_E_R + A_CHK_I_R + A_N_R + A_CHK_O_R + A_M_R) - 
#                    (A_ES_L + A_E_L + A_CHK_I_L + A_N_L + A_CHK_O_L + A_M_L)))
################################################################################
# get correlations between areas and IQ/NIH-TB
m124 <- inner_join(m1.m2, res.areas)
corr.table(m124 %>% select(any_of(c(colnames(m1), colnames(m2))),
                           -ends_with("id")),
           m124 %>% select(starts_with("A_"), "asym"),
           method = "pearson") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1), colnames(m2)), 
         V2 %in% colnames(res.areas)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = sub("A_", "", V2),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=reorder(V2, desc(V2)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  facet_wrap(~cat2, scales = "free", nrow = 1) +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "region's area",
       caption = paste0("n(samples): ", nrow(m124), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_facial-areas-IQ",
                         # "no-correction",
                         # "age-sex-inter-bmi-corrected",
                         "age-sex-inter-bmi-race-corrected",
                         ".png"),
       width = 8, height = 8, units = "in", dpi = 320, bg = "white")
# make scatterplots?
p <- m124 %>% 
  mutate_at(.vars = vars(starts_with("A_"), "asym"), .funs = function(x) scale(x, scale = T, center = T)[,1]) %>%
  pivot_longer(cols = c(colnames(m1.m2), -ends_with("id")), names_to = "iq", values_to = "iq_score") %>%
  pivot_longer(cols = c(starts_with("A_"), "asym"), names_to = "facial_area", values_to = "area") %>%
  mutate(iq = sub("_age_corrected_standard_score", "_NIH", iq)) %>%
  filter(grepl("ES", facial_area), 
         grepl(paste(c("BD", "cognition", "FSIQ", "card", "picture", "VCI", "PSI", "VC", "VP"), collapse = "|"), iq)) %>%
  ggplot(aes(x=iq_score, y=area)) +
  geom_point()+
  geom_smooth(method = "lm")+
  labs(y = "z-scaled area") +
  ggpubr::stat_cor(color = "red", size = 3) +
  ggh4x::facet_grid2(rows = vars(facial_area), cols = vars(iq), scales = "free")
ggsave(p, filename = paste0("figs/corr_facial-areas-IQ_scatter",
                            # "no-correction",
                            # "age-sex-inter-bmi-corrected",
                            "age-sex-inter-bmi-race-corrected",
                            ".png"), bg = "white",
       width = 20, height = 6, units = "in", dpi = 360)
# get correlatin between areas and PS-VC performance
m125 <- inner_join(ps.summ, res.areas)
corr.table(m125 %>% select(any_of(colnames(ps.summ)),
                           -ends_with("id")),
           m125 %>% select(starts_with("A_"), "asym"),
           method = "pearson") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(ps.summ), "age", "bmi"), 
         V2 %in% colnames(res.areas),
         V1 != V2) %>%
  mutate(V2 = sub("A_", "", V2),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=reorder(V2, desc(V2)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "region's area",
       caption = paste0("n(samples): ", nrow(m125), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_facial-areas-PS-VC",
                         # "no-correction",
                         # "age-sex-inter-bmi-corrected",
                         "age-sex-inter-bmi-race-corrected",
                         ".png"),
       width = 8, height = 8, units = "in", dpi = 320, bg = "white")

################################################################################
################################################################################
################################################################################
# head_circ with age, sex, bmi, race
meta <- meta %>% drop_na(head_circ)
res.head.circ <- residuals(glm(head_circ ~ sex, 
                               data = meta %>% 
                                 select(age, sex, bmi, Race, head_circ) %>%
                                 mutate(sex = as.factor(sex))))
meta$corrected_head_circ = res.head.circ
# correlate IQ with head_circ
m125 <- inner_join(meta, m1.m2) %>%
  pivot_longer(cols = c(colnames(m1.m2), -ends_with("id")), 
               names_to = "measure") %>%
  mutate(measure = sub("_age_corrected_standard_score", "_NIH", measure),
         cat2 = ifelse(grepl("NIH", measure), "NIH-TB", "IQ"),
         measure = sub("_NIH", "", measure))
m125 %>%
  ggplot(aes(x=value, y=corrected_head_circ)) +
  geom_point() + geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  facet_wrap(~measure, scales = "free") +
  labs(x = "score", y = "head circumference",
       caption = paste0("n(samples): ", length(unique(m125$te_id)), "\n",
                        "head circumference was corrected for sex"))
ggsave("figs/corr_head-circumference-IQ.png", bg = "white",
       width = 12, height = 13, units = "in", dpi = 360)
# correlate areas with head_circ
m126 <- inner_join(meta, areas) %>%
  pivot_longer(cols = starts_with("A_"), 
               names_to = "area") %>%
  mutate(area = sub("A_", "", area))
m126 %>%
  ggplot(aes(x=value, y=corrected_head_circ)) +
  geom_point() + geom_smooth(method = "lm") +
  ggpubr::stat_cor(color = "red") +
  facet_wrap(~area, scales = "free") +
  labs(x = "measured area", y = "head circumference",
       caption = paste0("n(samples): ", length(unique(m126$te_id)), "\n",
                        "head circumference was corrected for sex"))
ggsave("figs/corr_facial-areas-head-circumference.png", bg = "white",
       width = 8, height = 7, units = "in", dpi = 360)
################################################################################


################################################################################


################################################################################


################################################################################