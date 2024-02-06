################################################################################
#                 RF to predict IQ from face/hand scans measurments            #
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
rm(m1);rm(m2);gc()
################################################################################
# get hand scans measurments
hs <- readxl::read_xlsx("../language/data/raw/RPOE_meta.xlsx", sheet = 9)[-1,] %>%
  pivot_longer(cols = c(6:15), names_to = "finger", values_to = "length") %>%
  mutate(hand = sub("_[0-9]", "", finger),
         finger_num = parse_number(finger)) %>%
  drop_na(length) %>%
  left_join(meta %>% select(te_id, height)) %>%
  group_by(te_id, finger_num) %>%
  dplyr::summarise(avg_length = mean(as.numeric(length)/(height*10000), na.rm = T)) %>%
  pivot_wider(names_from = finger_num, values_from = avg_length, id_cols = te_id, names_prefix = "F_") %>%
  mutate(F_2D_4D = F_4/F_2)
################################################################################
# get the facial measures
# get pairs distances
int.pairs <- c("EB_R", "EB_L", "E_R", "E_L", "M_H", "N_V", "N_H_B", "M_V", "EB_C","EB_N_R", "EB_N_L", "EB_E_R", "EB_E_L","NT_E_R", "NT_E_L","EB_M_R", "EB_M_L","E_M_R", "E_M_L", "N_H_A")
pairs.dis <- read_csv("../photographs/data/derivatives/pairs-distances.csv") %>%
  select(te_id, paste0("P_", int.pairs)) # only keep distances of int
# get facial areas
areas <- read_csv("../photographs/data/derivatives/facial.areas.csv") %>%
  mutate(asym = ((A_ES_R + A_E_R + A_CHK_I_R + A_N_R + A_CHK_O_R + A_M_R) - 
                   (A_ES_L + A_E_L + A_CHK_I_L + A_N_L + A_CHK_O_L + A_M_L)))
facial <- inner_join(pairs.dis, areas)
################################################################################
# combine and get ready
measurements <- inner_join(facial, hs)
all.one <- inner_join(m1.m2 %>% select(te_id, -dev_id, FSIQ, any_of(colnames(nih.tb)),ends_with("composite_score")), 
                      measurements)
################################################################################
# build a RF to predict FSIQ from measurements
library(randomForest)
# train to predict FSIQ
rf <- randomForest::randomForest(x = all.one %>% select(colnames(measurements), 
                                                        any_of(colnames(nih.tb)), 
                                                        -ends_with("id")) %>%
                                   rename_at(.vars = vars(ends_with("_age_corrected_standard_score")), 
                                             .funs = function(x) sub("_age_corrected_standard_score", "_NIH", x)),
                                 y = all.one$FSIQ,
                                 nodesize=1, nPerm = 10, ntree = 100)
p1 <- cbind(y=rf$y, predicted=rf$predicted) %>% as.data.frame() %>%
  ggplot(aes(x=y, y=predicted))+geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color="red")+
  labs(title = "RF prediction for FSIQ")
p2 <- rf$importance %>% as.data.frame() %>% rownames_to_column("var") %>%
  ggplot(aes(x=IncNodePurity, y=reorder(var, desc(IncNodePurity))))+geom_point()+labs(y="")
p1+p2
# train to predict VCI
rf2 <- randomForest::randomForest(x = all.one %>% select(colnames(measurements), 
                                                         any_of(colnames(nih.tb)), 
                                                         -ends_with("id")) %>%
                                    rename_at(.vars = vars(ends_with("_age_corrected_standard_score")), 
                                              .funs = function(x) sub("_age_corrected_standard_score", "_NIH", x)),
                                  y = all.one$VCI_composite_score)
p3 <- cbind(y=rf2$y, predicted=rf2$predicted) %>% as.data.frame() %>%
  ggplot(aes(x=y, y=predicted))+geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color="red")+
  labs(title = "RF prediction for VCI")
p4 <- rf2$importance %>% as.data.frame() %>% rownames_to_column("var") %>%
  ggplot(aes(x=IncNodePurity, y=reorder(var, desc(IncNodePurity))))+geom_point()+labs(y="")
# train to predict PCI
rf3 <- randomForest::randomForest(x = all.one %>% select(colnames(measurements), 
                                                         any_of(colnames(nih.tb)), 
                                                         -ends_with("id")) %>%
                                    rename_at(.vars = vars(ends_with("_age_corrected_standard_score")), 
                                              .funs = function(x) sub("_age_corrected_standard_score", "_NIH", x)),
                                  y = all.one$PSI_composite_score)
p5 <- cbind(y=rf3$y, predicted=rf3$predicted) %>% as.data.frame() %>%
  ggplot(aes(x=y, y=predicted))+geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color="red")+
  labs(title = "RF prediction for PSI")
p6 <- rf3$importance %>% as.data.frame() %>% rownames_to_column("var") %>%
  ggplot(aes(x=IncNodePurity, y=reorder(var, desc(IncNodePurity))))+geom_point()+labs(y="")
# train to predict WM
rf4 <- randomForest::randomForest(x = all.one %>% select(colnames(measurements), 
                                                         any_of(colnames(nih.tb)), 
                                                         -ends_with("id")) %>%
                                    rename_at(.vars = vars(ends_with("_age_corrected_standard_score")), 
                                              .funs = function(x) sub("_age_corrected_standard_score", "_NIH", x)),
                                  y = all.one$WM_composite_score)
p7 <- cbind(y=rf4$y, predicted=rf4$predicted) %>% as.data.frame() %>%
  ggplot(aes(x=y, y=predicted))+geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color="red")+
  labs(title = "RF prediction for WM")
p8 <- rf4$importance %>% as.data.frame() %>% rownames_to_column("var") %>%
  ggplot(aes(x=IncNodePurity, y=reorder(var, desc(IncNodePurity))))+geom_point()+labs(y="")
patchwork::wrap_plots(p1,p2,p3,p4,p5,p6,p7,p8, nrow = 2, 
                      byrow = F, heights = c(1,3))
ggsave(filename = "figs/RF-predictions.png", bg = "white",
       width = 15, height = 10, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
