################################################################################
#                        CCA for facial landmarks, IQ, and PS-VC               #
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
  select(devGenes_id = 2, height = 5, weight =6) %>%
  mutate(height = as.numeric(height), weight = as.numeric(weight)) %>%
  drop_na() %>%
  mutate(bmi = weight/(height^2))
meta <- full_join(meta, vitals)
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
################################################################################
# get cbcl, and ysr
cbcl <- read_csv("../behavior/data/derivatives/cbcl-clean-median-imputed.csv") %>%
  left_join(meta %>% select(devGenes_id, te_id, age, sex, bmi, Race))
################################################################################
# prepare CCA matrices
# int.ids <- intersect(m1.m2$te_id, intersect(ps.summ$te_id, intersect(pairs.dis$te_id, areas$te_id)))
int.ids <- intersect(m1.m2$te_id, intersect(cbcl$te_id, intersect(pairs.dis$te_id, areas$te_id)))
c1 <- m1.m2 %>% select(-dev_id) %>% column_to_rownames("te_id")
c2 <- inner_join(pairs.dis, areas) %>% column_to_rownames("te_id")
c3 <- ps.summ %>% column_to_rownames("te_id")
# c3 <- cbcl %>% select(te_id, ends_with("tot")) %>% drop_na(te_id) %>% column_to_rownames("te_id")
c1 <- c1[int.ids,]; c2 <- c2[int.ids, ]; c3 <- c3[int.ids,]
################################################################################
# do cca
library(RGCCA)
cca.cv <- rgcca_cv(blocks = list(iq = c1,
                                 psvc = c3,
                                 facial = c2),
                              ncomp = 5, verbose = T, method = "sgcca",
                              response = 1, validation = "kfold", k = 10, n_cores = 1)
all.cca <- rgcca(cca.cv)
################################################################################
cca.iq <- all.cca$Y$iq
cca.psvc <- all.cca$Y$psvc
cca.face <- all.cca$Y$facial

m11 <- cbind(c1,cca.iq)
p1 <- corr.table(m11 %>% select(starts_with("comp")),
           m11 %>% select(colnames(c1))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(grepl("comp[0-9]", V1),
         V2 %in% colnames(c1)) %>%
  mutate(V2 = sub("_age_corrected_standard_score", "_NIH", V2),
         cat2 = ifelse(grepl("NIH", V2), "NIH-TB", "IQ"),
         V2 = sub("_NIH", "", V2),
         comp = parse_number(V1),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=reorder(V1, comp), y=V2, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free") +
  labs(y = "", x = "", 
       caption = paste0("n(samples): ", nrow(m11),"\n")) +
  my.guides

m12 <- cbind(c3,cca.psvc)
p2 <- corr.table(m12 %>% select(starts_with("comp")),
           m12 %>% select(colnames(c3))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(grepl("comp[0-9]", V1),
         V2 %in% colnames(c3)) %>%
  mutate(comp = parse_number(V1),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=reorder(V1, comp), y=V2, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  # ggh4x::facet_grid2(rows = vars(cat2), scales = "free") +
  labs(y = "", x = "", 
       caption = paste0("n(samples): ", nrow(m12),"\n")) +
  my.guides

m13 <- cbind(c2,cca.face)
p3 <- corr.table(m13 %>% select(starts_with("comp")),
           m13 %>% select(colnames(c2))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(grepl("comp[0-9]", V1),
         V2 %in% colnames(c2)) %>%
  mutate(comp = parse_number(V1),
         V2 = factor(V2, levels = unique(V2)),
         cat2 = ifelse(grepl("A_", V2), "Area", "Distance"),
         V2 = sub("A_", "", V2),
         V2 = sub("P_", "", V2)) %>%
  ggplot(aes(x=reorder(V1, comp), y=V2, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free", space = "free") +
  labs(y = "", x = "", 
       caption = paste0("n(samples): ", nrow(m13),"\n")) +
  my.guides

patchwork::wrap_plots(p1,p2,p3)
ggsave(filename = "figs/CCA-IQ-face-psvc.png", bg="white",
       width = 9, height = 8, units = "in", dpi = 320)
patchwork::wrap_plots(p1,p3)
ggsave(filename = "figs/CCA-IQ-face.png", bg="white",
       width = 9, height = 8, units = "in", dpi = 320)



################################################################################


################################################################################


################################################################################


################################################################################
