################################################################################
#                         extract facial landmarks and features                #
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
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
################################################################################
# get participants' metadata
meta <- readxl::read_xlsx("../language/data/raw/RPOE_participants_metadata.xlsx", sheet = 1) %>%
  drop_na(DOB) %>%
  mutate(age = age(DOB, floor = F))
vitals <- readxl::read_xlsx("../language/data/raw/RPOE Stats.xlsx", sheet = 1) %>%
  select(devGenes_id = 2, height = 5, weight =6) %>%
  mutate(height = as.numeric(height), weight = as.numeric(weight)) %>%
  drop_na() %>%
  mutate(bmi = weight/(height^2))
meta <- full_join(meta, vitals)
################################################################################
# read int transcription
int <- read_csv("data/derivatives/interview_transcription/all-transcriptions.csv")
# read LIWC results
liwc.22 <- cbind(te_id = int$te_id,
                 read_csv("data/derivatives/LIWC/int-transcriptions-LIWC.csv"))
narr <- cbind(te_id = int$te_id,
              read_csv("data/derivatives/LIWC/int-transcriptions-Narrative.csv"))
################################################################################
# read tests data
nih.tb <- read_csv("../language/data/derivatives/nih-tb_clean.csv")
iq <- read_csv("../language/data/derivatives/wisc-and-wais_clean.csv")
m1 <- nih.tb %>%
  select(te_id, dev_id, 
         ends_with("age_corrected_standard_score")) %>%
  drop_na() %>%
  mutate(PV_PS_age_corrected_standard_score = PV_age_corrected_standard_score - pattern_age_corrected_standard_score,
         abs_PV_PS_age_corrected_standard_score = abs(PV_age_corrected_standard_score - pattern_age_corrected_standard_score))
m2 <- iq %>%
  select(dev_id, te_id,
         paste0(c("PSI", "WM", "VCI"), "_composite_score"),
         FSIQ, 
         SI, VC, BD, VP, MR, 
         #FW, PS, IN, AR, 
         DS, CD, SS) %>%
  mutate(VCI_PSI = VCI_composite_score - PSI_composite_score,
         abs_VCI_PSI = abs(VCI_composite_score - PSI_composite_score))
m1.m2 <- inner_join(m1, m2) %>%
  mutate(te_id=ifelse(is.na(te_id), dev_id, te_id)) %>%
  rename(te_id = te_id)
rm(nih.tb);rm(iq);gc()
################################################################################
# dist of data
narr %>% 
  pivot_longer(cols = colnames(narr)[-c(1:2)], names_to = "var") %>%
  ggplot(aes(x=value)) +
  geom_histogram()+
  facet_wrap(~var, scales = "free")
ggsave(filename = paste0("figs/narrativity-distribution-",
                         "no-correction",
                         ".png"),
       width = 10, height = 8, units = "in", dpi = 320, bg = "white")
################################################################################
################################################################################
# combine narrative measures with IQ, and check corr
m123 <- inner_join(m1.m2, narr)
corr.table(m123 %>% select(colnames(m1.m2),
                           -ends_with("_id")),
           m123 %>% select(starts_with("Narrativity"),
                           starts_with("Peak"), 
                           starts_with("Valle"),
                           "WC")) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1), colnames(m2)), 
         V2 %in% colnames(narr)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=reorder(V2, desc(V2)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  facet_wrap(~cat2, scales = "free_x", nrow = 1) +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "Narrativity measure",
       caption = paste0("n(samples): ", nrow(m123), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_narrativity-IQ-",
                         "no-correction",
                         ".png"),
       width = 10, height = 8, units = "in", dpi = 320, bg = "white")
# plot scatter for interesting ones
inner_join(m2,narr) %>%
  pivot_longer(cols = c(colnames(m2), -ends_with("id")), names_to = "iq", values_to = "val1") %>%
  pivot_longer(cols = c(starts_with("Narrativity"), starts_with("Valley")), names_to = "narr", values_to = "val2") %>%
  mutate(iq = sub("_age_corrected_standard_score", "", iq)) %>%
  ggplot(aes(x=val1, y=val2)) +
  geom_point(size=2) +
  geom_smooth(method = "lm") + ggpubr::stat_cor(color = "red") +
  ggh4x::facet_grid2(cols = vars(iq), rows = vars(narr), scales = "free")+
  labs(caption = paste0("n(samples): ", nrow(inner_join(m2,narr))))
ggsave(filename = paste0("figs/corr_narrativity-",
                         # "NIH-",
                         "IQ-",
                         "no-correction",
                         "-scatter.png"),
       width = 25, height = 12, units = "in", dpi = 320, bg = "white")

################################################################################
################################################################################
# combine LIWC.22 measures with IQ, and check corr
m124 <- inner_join(m1.m2, liwc.22[,-1])
# get categories of liwc
catt <- read_csv("/wdata/msmuhammad/data/LIWC/liwc-categories.csv")
corr.table(m124 %>% select(colnames(m1.m2),
                           -ends_with("_id")),
           m124 %>% select(colnames(liwc.22)[5:ncol(liwc.22)])) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1), colnames(m2)), 
         V2 %in% colnames(liwc.22)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(reorder(V1, desc(V1))))) %>%
  left_join(catt %>% rename(V2 = feature, cat1 = category)) %>%
  filter(! V2 %in% c("i", "we", "you", "shehe", "they", "article", "number")) %>%
  mutate(cat1 = ifelse(is.na(cat1), "summary", cat1),
         cat1 = factor(cat1, levels = unique(catt$category))) %>%
  ggplot(aes(x=V1, y=reorder(V2, desc(V2)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  # facet_wrap(~cat2, scales = "free_x", nrow = 1) +
  ggh4x::facet_grid2(rows = vars(cat1), cols = vars(cat2), scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "LIWC measure",
       caption = paste0("n(samples): ", nrow(m124), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides +
  theme(strip.text.y.right = element_text(angle = 0))
ggsave(filename = paste0("figs/corr_LIWC-IQ-",
                         "no-correction",
                         ".png"),
       width = 10, height = 24, units = "in", dpi = 320, bg = "white")
################################################################################
################################################################################
# get correlation between narr scores, and age &sex
narr.as <- inner_join(meta %>% select(devGenes_id, te_id, age, sex, bmi, Race),
                      narr)
narr.as %>%
  pivot_longer(cols = c(starts_with("Narrativity"), starts_with("Valley")), 
               names_to = "narr", values_to = "val1") %>%
  pivot_longer(cols = c(age, bmi), names_to = "ab", values_to = "val2") %>%
  ggplot(aes(x=val1, y=val2)) +
  geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color="red")+
  ggh4x::facet_grid2(rows = vars(ab), cols = vars(narr), scales = "free")
ggsave(filename = paste0("figs/narrativity-correlation-w-age-bmi-",
                         "scatter",
                         ".png"),
       width = 13, height = 6, units = "in", dpi = 320, bg = "white")
# get corr w sex
narr.as %>%
  pivot_longer(cols = c(starts_with("Narrativity"), starts_with("Valley")), 
               names_to = "narr", values_to = "val1") %>%
  ggplot(aes(x=sex, y=val1, fill = sex)) +
  geom_violin()+geom_boxplot(width = 0.1, fill = "white")+ggpubr::stat_compare_means(color="red")+
  facet_wrap(~narr, scales = "free")
ggsave(filename = paste0("figs/narrativity-correlation-w-sex-",
                         "violin",
                         ".png"),
       width = 8, height = 8, units = "in", dpi = 320, bg = "white")
# get corr w race
narr.as %>%
  pivot_longer(cols = c(starts_with("Narrativity"), starts_with("Valley")), 
               names_to = "narr", values_to = "val1") %>%
  ggplot(aes(x=Race, y=val1, fill = Race)) +
  geom_violin()+geom_boxplot(width = 0.1, fill = "white")+
  facet_wrap(~narr, scales = "free")
################################################################################
################################################################################
# plot the narrative arc measures
narr %>%
  pivot_longer(cols = c(starts_with("CogTension_"), 
                        starts_with("Staging_"),
                        starts_with("PlotProg_")), 
               names_to = "narr", values_to = "val") %>%
  mutate(segment = parse_number(narr),
         arc = sub("_.*", "", narr)) %>%
  ggplot(aes(x=segment, y=val, color = te_id)) +
  geom_line(alpha = 0.4, show.legend = F) +
  # ggh4x::facet_grid2(rows = vars(te_id), cols = vars(arc), scales = "free") +
  facet_wrap(~arc, scales = "free") +
  theme(strip.text.y.right = element_text(angle = 0))
ggsave(filename = paste0("figs/narrativity-arc-lines",
                         # "-by-participant",
                         ".png"),
       # width = 8, height = 20, units = "in", dpi = 320, bg = "white")
       width = 8, height = 4, units = "in", dpi = 320, bg = "white")
################################################################################


################################################################################


################################################################################


################################################################################