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
                  "PV_uncorrected_standard_score", "PV_age_corrected_standard_score", "PV_national_percentile_age_adjusted", "PV_fully_corrected_t_score",
                  "flkr_raw_score", "flkr_computed_score", "flkr_uncorrected_standard_score", "flkr_age_corrected_standard_score", "flkr_national_percentile_age_adjusted", "flkr_fully_corrected_t_score",
                  "wm_raw_score", "wm_uncorrected_standard_score", "wm_age_corrected_standard_score", "wm_national_percentile_age_adjusted", "wm_fully_corrected_t_score",
                  "card_raw_score", "card_computed_score", "card_uncorrected_standard_score","card_age_corrected_standard_score", "card_national_percentile_age_adjusted","card_fully_corrected_t_score",
                  "pattern_raw_score", "pattern_computed_score", "pattern_uncorrected_standard_score","pattern_age_corrected_standard_score", "pattern_national_percentile_age_adjusted","pattern_fully_corrected_t_score",
                  "picture_raw_score", "picture_theta", "picture_se" ,"picture_computed_score", "picture_uncorrected_standard_score","picture_age_corrected_standard_score", "picture_national_percentile_age_adjusted","picture_fully_corrected_t_score",
                  "reading_theta", "reading_se" ,"reading_uncorrected_standard_score","reading_age_corrected_standard_score", "reading_national_percentile_age_adjusted","reading_fully_corrected_t_score",
                  "fluid_uncorrected_standard_score","fluid_age_corrected_standard_score", "fluid_national_percentile_age_adjusted","fluid_fully_corrected_t_score",
                  "crystallized_uncorrected_standard_score","crystallized_age_corrected_standard_score", "crystallized_national_percentile_age_adjusted","crystallized_fully_corrected_t_score",
                  "cognition_tot_uncorrected_standard_score","cognition_tot_age_corrected_standard_score", "cognition_tot_national_percentile_age_adjusted","cognition_tot_fully_corrected_t_score",
                  "cognition_EC_tot_uncorrected_standard_score","cognition_EC_tot_age_corrected_standard_score", "cognition_EC_tot_national_percentile_age_adjusted","cognition_EC_tot_fully_corrected_t_score"
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
  "SI", "VC", "BD", "VP", "MR", "FW", "DS", "PS", "CD", "SS"
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
  "SI", "VC", "IN", "BD", "MR", "VP", "DS", "AR", "SS", "CD"
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

################################################################################

################################################################################


