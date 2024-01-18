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
         # ends_with("composite_score"),
         FSIQ, 
         SI, VC, BD, VP, MR, 
         #FW, PS, IN, AR, 
         DS, CD, SS) %>%
  mutate(VCI_PSI = VCI_composite_score - PSI_composite_score,
         abs_VCI_PSI = abs(VCI_composite_score - PSI_composite_score))
m1.m2 <- inner_join(m1, m2) %>%
  mutate(te_id=ifelse(is.na(te_id), dev_id, te_id)) %>%
  rename(te_id = te_id)
################################################################################
# get participants' metadata
meta <- readxl::read_xlsx("../language/data/raw/RPOE_participants_metadata.xlsx", sheet = 1) %>%
  drop_na(DOB) %>%
  mutate(age = age(DOB, floor = F))
# get pairs distances
int.pairs <- c("EB_R", "EB_L", "E_R", "E_L", "M_H", "N_V", "N_H", "M_V", "EB_C","EB_N_R", "EB_N_L", "EB_E_R", "EB_E_L","NT_E_R", "NT_E_L","EB_M_R", "EB_M_L","E_M_R", "E_M_L")
pairs.dis <- read_csv("data/derivatives/pairs-distances.csv") %>%
  select(te_id, paste0("P_", int.pairs)) %>% # only keep distances of int
  left_join(meta %>% select(te_id, age, sex))
# correct pairs distances for age, sex, and interaction
# 
res.pairs <- cbind(te_id = pairs.dis$te_id,
                   apply(pairs.dis %>% select(starts_with("P_")), MARGIN = 2, FUN = function(x) {
                     residuals(glm(y ~ age + sex + age:sex, 
                                   data = cbind(pairs.dis %>% select(age, sex) %>%
                                                  mutate(sex = as.factor(sex)),
                                                y = x)))
                     }) %>%
                     as.data.frame())
  
################################################################################
# get correlations between distances and IQ/NIH-TB
m123 <- inner_join(m1.m2, res.pairs)
corr.table(m123 %>% select(any_of(c(colnames(m1), colnames(m2))),
                           -ends_with("id")),
           m123 %>% select(any_of(colnames(pairs.dis)),
                           -te_id),
           method = "pearson") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1), colnames(m2)), 
         V2 %in% colnames(pairs.dis)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         # cat1 = ifelse(!grepl(paste(c("E", "M", "N"), collapse = "|"), V2), "all","int"),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = sub("P_", "", V2),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=reorder(V2, desc(V2)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  # ggh4x::facet_grid2(rows = vars(cat1), 
  #                    cols = vars(cat2),
  #                    scales = "free", space = "free") +
  facet_wrap(~cat2, scales = "free", nrow = 1) +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "pair # of two facial landmarks points",
       caption = paste0("n(samples): ", nrow(m123), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_facial-landmarks-pairs-distance-IQ.png"),
       width = 8, height = 8, units = "in", dpi = 320, bg = "white")
################################################################################


################################################################################


################################################################################


################################################################################


################################################################################