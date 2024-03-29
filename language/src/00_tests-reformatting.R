################################################################################
#                       tests reformatting for colnames and data               #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
# before running this script, make sure you downloaded the most updated xlsx file 
# move the file to the data/raw directory
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
# read ID mappings
ids <- readxl::read_xlsx("data/raw/RPOE_participants_metadata.xlsx", sheet = 1)%>%
  mutate(dev_id = devGenes_id)
################################################################################
# read NIH-TB data
nih.tb.names <- c("te_id", "dev_id", "nih_tb_date", "nih_tb_time","sex", "asd_dx","adhd_dx",
                  "picture_vocabulary_uncorrected_standard_score", "picture_vocabulary_age_corrected_standard_score", "PV_national_percentile_age_adjusted", "PV_fully_corrected_t_score",
                  "flkr_raw_score", "flkr_computed_score", "flkr_uncorrected_standard_score", "flanker_inhibitory_control_age_corrected_standard_score", "flkr_national_percentile_age_adjusted", "flkr_fully_corrected_t_score",
                  "wm_raw_score", "wm_uncorrected_standard_score", "list_sorting_wm_age_corrected_standard_score", "wm_national_percentile_age_adjusted", "wm_fully_corrected_t_score",
                  "card_raw_score", "card_computed_score", "card_uncorrected_standard_score","dimensional_change_card_sort_age_corrected_standard_score", "card_national_percentile_age_adjusted","card_fully_corrected_t_score",
                  "pattern_raw_score", "pattern_computed_score", "pattern_uncorrected_standard_score","pattern_comparison_PS_age_corrected_standard_score", "pattern_national_percentile_age_adjusted","pattern_fully_corrected_t_score",
                  "picture_raw_score", "picture_theta", "picture_se" ,"picture_computed_score", "picture_uncorrected_standard_score","picture_sequence_memory_test_age_corrected_standard_score", "picture_national_percentile_age_adjusted","picture_fully_corrected_t_score",
                  "reading_theta", "reading_se" ,"reading_uncorrected_standard_score","oral_reading_recognition_age_corrected_standard_score", "reading_national_percentile_age_adjusted","reading_fully_corrected_t_score",
                  "fluid_uncorrected_standard_score","cognition_fluid_composite_age_corrected_standard_score", "fluid_national_percentile_age_adjusted","fluid_fully_corrected_t_score",
                  "crystallized_uncorrected_standard_score","cognition_crystallized_composite_age_corrected_standard_score", "crystallized_national_percentile_age_adjusted","crystallized_fully_corrected_t_score",
                  "cognition_tot_uncorrected_standard_score","cognition_total_composite_age_corrected_standard_score", "cognition_tot_national_percentile_age_adjusted","cognition_tot_fully_corrected_t_score",
                  "cognition_EC_tot_uncorrected_standard_score","cognition_early_childhood_composite_age_corrected_standard_score", "cognition_EC_tot_national_percentile_age_adjusted","cognition_EC_tot_fully_corrected_t_score"
                  )
nih.tb <- readxl::read_xlsx("data/raw/RPOE_ALL_Michaelson_lab.xlsx", skip = 2,
                         sheet = 4, col_names = nih.tb.names) %>% 
  select(-te_id) %>%
  left_join(ids %>% select(te_id, dev_id))
################################################################################
# IQ testing
iq.combined.names <- c(
  "dev_id", "te_id", "iq_date", "iq_time", "sex", "DOB", "gender", "iq_type", "gift_card",
  "asd_dx", "adhd_dx", "report_requested", "report_proofed", "report_sent", 
  "VCI_PSI", "VCI_composite_score", "VCI_PR", "VCI_CI",
  "VSI_composite_score", "VSI_PR", "VSI_CI",
  "FR_composite_score", "FR_PR", "FR_CI",
  "WM_composite_score", "WM_PR", "WM_CI",
  "PSI_composite_score", "PSI_PR", "PSI_CI",
  "FSIQ", "FSIQ_PR", "FSIQ_CI",
  "SI", "VC", "BD", "VP", "MR", "FW", "DS", "PS", "CD", "SS"
)
wisc.names <- c(
  "te_id", "dev_id", "iq_date", "iq_time", "sex",
  "report_requested", "report_proofed", "report_sent",
  "VCI_composite_score", "VCI_PR", "VCI_CI",
  "VSI_composite_score", "VSI_PR", "VSI_CI",
  "FR_composite_score", "FR_PR", "FR_CI",
  "WM_composite_score", "WM_PR", "WM_CI",
  "PSI_composite_score", "PSI_PR", "PSI_CI",
  "FSIQ", "FSIQ_PR", "FSIQ_CI",
  "Similarities", "Vocabulary", "Block_Design", 
  "Visual_Puzzles", "Matrix_Reasoning", "Figure_Weights", 
  "Digit_Span", "Picture_Span", "Coding", "Symbol_Search"
)
wais.names <- c(
  "te_id", "dev_id", "iq_date", "iq_time", "sex", "gender",
  # "iq_type",
  "report_requested", "report_proofed", "report_sent",
  "VCI_PSI",
  "VCI_composite_score", "VCI_PR", "VCI_CI",
  "PRI_composite_score", "PRI_PR", "PRI_CI",
  # "WMI_composite_score", "WMI_PR", "WMI_CI",
  "WM_composite_score", "WM_PR", "WM_CI",
  "PSI_composite_score", "PSI_PR", "PSI_CI",
  "FSIQ", "FSIQ_PR", "FSIQ_CI",
  "Similarities", "Vocabulary", "Information", "Block_Design", 
  "Matrix_Reasoning", "Visual_Puzzles", "Digit_Span", 
  "Arithmetic", "Symbol_Search", "Coding"
)
iq.combined <- readxl::read_xlsx("data/raw/RPOE_ALL_Michaelson_lab.xlsx", skip = 2,
                               sheet = 3, col_names = iq.combined.names) %>%
  drop_na(dev_id) %>%
  mutate_at(.vars = vars(ends_with("PR")), .funs = function(x) 
    ifelse(is.character(x), readr::parse_number(x), x)) %>% 
  select(-te_id) %>%
  left_join(ids %>% select(te_id, dev_id))
wisc <- readxl::read_xlsx("data/raw/RPOE_ALL_Michaelson_lab.xlsx", skip = 2,
                          sheet = 1, col_names = wisc.names) %>%
  drop_na(dev_id, FSIQ) %>%
  mutate(iq_type = "WISC") %>%
  mutate_at(.vars = vars(ends_with("PR")), .funs = function(x) 
    ifelse(is.character(x), readr::parse_number(x), x)) %>% 
  select(-te_id) %>%
  left_join(ids %>% select(te_id, dev_id))
wais <- readxl::read_xlsx("data/raw/RPOE_ALL_Michaelson_lab.xlsx", skip = 2,
                          sheet = 2, col_names = wais.names) %>%
  drop_na(dev_id) %>%
  mutate(iq_type = "WAIS") %>%
  mutate_at(.vars = vars(ends_with("PR")), .funs = function(x) 
    ifelse(is.character(x), readr::parse_number(x), x)) %>% 
  select(-te_id) %>%
  left_join(ids %>% select(te_id, dev_id))
wisc.wais <- full_join(wisc, wais%>%select(-iq_date)) %>%
  select(-c(iq_date, iq_time)) %>%
  left_join(iq.combined %>% select(dev_id, `te_id`))
################################################################################
# save clean data
write_csv(nih.tb, "data/derivatives/nih-tb_clean.csv")
write_csv(wais, "data/derivatives/wais_clean.csv")
write_csv(wisc, "data/derivatives/wisc_clean.csv")
write_csv(wisc.wais, "data/derivatives/wisc-and-wais_clean.csv")
################################################################################
################################################################################
# read tests data, and clean
nih.tb <- read_csv("data/derivatives/nih-tb_clean.csv")
iq <- read_csv("data/derivatives/wisc-and-wais_clean.csv")
m1 <- nih.tb %>%
  select(te_id, dev_id, ends_with("age_corrected_standard_score")) %>%
  drop_na() %>%
  mutate(PV_PS_age_corrected_standard_score = ((0.5*picture_vocabulary_age_corrected_standard_score) + (0.5*oral_reading_recognition_age_corrected_standard_score)) - pattern_comparison_PS_age_corrected_standard_score,
         abs_PV_PS_age_corrected_standard_score = abs(PV_PS_age_corrected_standard_score))
m2 <- iq %>%
  select(dev_id, te_id,
         paste0(c("PSI", "WM", "VCI"), "_composite_score"),
         FSIQ, Similarities, Vocabulary, Block_Design, Visual_Puzzles, 
         Matrix_Reasoning, Digit_Span, Coding, Symbol_Search) %>%
  mutate(VCI_PSI = VCI_composite_score - PSI_composite_score,
         abs_VCI_PSI = abs(VCI_composite_score - PSI_composite_score))
m1.m2 <- inner_join(m1, m2) %>%
  mutate(te_id=ifelse(is.na(te_id), dev_id, te_id)) %>%
  rename(te_id = te_id)
write_rds(m1.m2, "data/derivatives/m1m2.rds")
# correct for age and sex
demo <- read_rds("data/raw/demo.rds")
m1.m2.corr <- m1.m2 %>% 
  left_join(demo %>% select(te_id, age, sex)) %>%
  relocate(c(age, sex),.after = dev_id)
m1.m2.corr <- m1.m2.corr %>%
  mutate_at(.vars = vars(-c(te_id, dev_id, age, sex)), .funs = function(x) {
    residuals(glm(x ~ age + sex + age:sex, data = m1.m2.corr))
  })
write_rds(m1.m2.corr %>% select(-age, -sex), "data/derivatives/m1m2-age-sex-corrected.rds")
# plot corr betweeb IQ and NIH-TB
corr.table(m1.m2 %>% select(any_of(colnames(m1)), - ends_with("_id")),
           m1.m2 %>% select(any_of(colnames(m2)), - ends_with("_id"))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  # filter(V1 %in% colnames(m1),
         # V2 %in% colnames(m2)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "NIH", V1),
         V2 = sub("_composite_score", "", V2),
         V2 = sub("_age_corrected_standard_score", "NIH", V2),
         V1 = sub("_composite_score", "", V1),
         cat1 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         cat2 = ifelse(grepl("NIH", V2), "NIH-TB", "IQ"),
         V1 = sub("NIH", "", V1),
         V2 = sub("NIH", "", V2),
         V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2))) %>%
  drop_na() %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text()+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  ggh4x::facet_grid2(cols = vars(cat1), rows = vars(cat2), scales = "free") +
  labs(y = "", x = "", 
       caption = paste0("n(samples): ", nrow(m1.m2),"\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = "figs/corr-between-NIH-TB-IQ.png", bg="white",
       width = 12, height = 12, units = "in", dpi = 320)
# rm(nih.tb);rm(iq);rm(m1);rm(m2);gc
# ggheatmap
corr <- corr.table(m1.m2 %>% select(any_of(colnames(m1)), - ends_with("_id")),
                   m1.m2 %>% select(any_of(colnames(m2)), - ends_with("_id"))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "NIH", V1),
         V2 = sub("_composite_score", "", V2),
         V2 = sub("_age_corrected_standard_score", "NIH", V2),
         V1 = sub("_composite_score", "", V1),
         cat1 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         cat2 = ifelse(grepl("NIH", V2), "NIH-TB", "IQ"),
         V1 = sub("NIH", "", V1),
         V2 = sub("NIH", "", V2),
         V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2))) %>%
  filter(!grepl("abs", V1),
         !grepl("abs", V2),
         ! V1 %in% c("PV_PS", "VCI_PSI"),
         ! V2 %in% c("PV_PS", "VCI_PSI")) %>%
  drop_na() %>%
  # filter(cat1 == "IQ", cat2 == "NIH-TB") %>%
  pivot_wider(names_from = "V2", values_from = r, id_cols = "V1") %>%
  column_to_rownames("V1")
corr <- corr[colnames(corr),]
heatmap(corr%>%as.matrix(), symm = T, col=colorRampPalette(brewer.pal(8, "Greys"))((25)), margins = c(12,12))
################################################################################
################################################################################
################################################################################
# plot distribution of scores
m1.m2 %>%
  pivot_longer(cols = colnames(m1.m2)[-c(1:2)], names_to = "var") %>%
  mutate(var = sub("_age_corrected_standard_score", "", var),
         var = sub("_composite_score", "", var)) %>%
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(~var, scales = "free")
ggsave("figs/distribution-of-IQ-NIH.png", bg = "white",
       width = 12, height = 10, units = "in", dpi = 360)
nih.tb %>%
  pivot_longer(cols = contains("percentile"), names_to = "measure") %>%
  mutate(measure = sub("_national_percentile_age_adjusted", "", measure)) %>%
  ggplot(aes(x=value))+
  geom_histogram()+
  facet_wrap(~measure)
ggsave("figs/distribution-of-NIH-percentiles.png", bg = "white",
       width = 12, height = 10, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
# get some stats of the data to add in a table
demo <- read_rds("data/raw/demo.rds")
m123 <- inner_join(m1.m2, demo)
table(m123$sex)
data.frame(count = nrow(m123),
           male = paste0(sum(m123$sex=="M"), " (", round(sum(m123$sex=="M")/nrow(m123),2)*100, "%)"),
           female = paste0(sum(m123$sex=="F"), " (", round(sum(m123$sex=="F")/nrow(m123),2)*100, "%)"),
           age = paste0(round(mean(m123$age),2), " (", round(sd(m123$age),2), ")"),
           FSIQ = paste0(round(mean(m123$FSIQ),2), " (", round(sd(m123$FSIQ),2), ")"),
           VCI = paste0(round(mean(m123$VCI_composite_score),2), " (", round(sd(m123$VCI_composite_score),2), ")"),
           PSI = paste0(round(mean(m123$PSI_composite_score),2), " (", round(sd(m123$PSI_composite_score),2), ")"),
           WM = paste0(round(mean(m123$WM_composite_score),2), " (", round(sd(m123$WM_composite_score),2), ")"))
################################################################################
# PS-VC summary

################################################################################
################################################################################


