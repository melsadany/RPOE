################################################################################
#                 correlation between hand scans and other features            #
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
m1.m2 <- inner_join(m1.m2, meta %>% select(te_id, age, bmi))
rm(m1);rm(m2);gc()
################################################################################
# read hand scans measurements
hs <- readxl::read_xlsx("../language/data/raw/RPOE_meta.xlsx", sheet = 9)[-1,] %>%
  pivot_longer(cols = c(6:15), names_to = "finger", values_to = "length") %>%
  mutate(hand = sub("_[0-9]", "", finger),
         finger_num = parse_number(finger)) %>%
  drop_na(length)
hs.avg <- hs %>%
  group_by(te_id, finger_num) %>%
  dplyr::summarise(avg_length = mean(as.numeric(length), na.rm = T)) %>%
  pivot_wider(names_from = finger_num, values_from = avg_length, id_cols = te_id, names_prefix = "F_") %>%
  mutate(F_2D_4D = F_4/F_2)
################################################################################
m123 <- inner_join(m1.m2, hs.avg)
corr.table(m123 %>% select(c(colnames(m1.m2), -ends_with("id"))),
           m123 %>% select(starts_with("F_"))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2)), 
         V2 %in% colnames(hs.avg)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         cat2 = ifelse(V1 %in% c("age", "bmi"), "demo", cat2),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         # V2 = sub("F_", "", V2),
         V2 = case_when(V2=="F_1"~"little finger",
                        V2=="F_2"~"ring finger",
                        V2=="F_3"~"middle finger",
                        V2=="F_4"~"index finger",
                        V2=="F_5"~"thumb",
                        V2=="F_2D_4D"~"index-to-ring ratio"),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=reorder(V2, desc(V2)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(cols = vars(cat2), scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "finger",
       caption = paste0("n(samples): ", nrow(m123), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_hand-scans-IQ",
                         ".png"),
       width = 8, height = 8, units = "in", dpi = 320, bg = "white")
################################################################################


################################################################################


################################################################################


################################################################################


################################################################################



################################################################################
