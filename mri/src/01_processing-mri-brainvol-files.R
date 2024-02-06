################################################################################
#                    extract brainvol stats from freesurfer output             #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/mri"
setwd(project.dir)
################################################################################
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
################################################################################
# list participants
mri.meta <- data.frame(folder = list.files("/Dedicated/jmichaelson-sdata/MRI/RPOE/RPOE_MR/derivatives",
                                           recursive = F, full.names = T, pattern = "sub-")) %>%
  mutate(te_id = sub("sub-", "", basename(folder))) %>%
  filter(te_id != "2E_031_230908bak", # drop that bak folder
         te_id %in% paste0("2E_0", c(31, 34:100))) # drop older participants
dirs <- list.dirs(mri.meta$folder, full.names = F, recursive = F)
mri.meta <- mri.meta %>%
  mutate(dir2 = dirs[grepl("sub-", dirs)],
         file = paste0(folder,"/", dir2, "/stats/brainvol.stats"),
         processed = ifelse(file.exists(file), T, F)) %>%
  filter(processed == T)
################################################################################
# read files and extract data
registerDoMC(cores = 3)
stats.c <- foreach(i = 1:nrow(mri.meta), .combine = rbind) %dopar% {
  id <- mri.meta$te_id[i]
  t <- data.frame(lines = read_lines(mri.meta$file[i])) %>%
    mutate(measure = sub(",.*", "", lines),
           measure = sub("# Measure ", "", measure),
           value = sub("mm^3", "", lines),
           value = parse_number(value),
           te_id = id) %>%
    pivot_wider(names_from = measure, values_from = value, id_cols = te_id)
  return(t)
}
write_csv(stats.c, "data/derivatives/brainvol-stats.csv")
# stats.c <- read_csv("data/derivatives/brainvol-stats.csv")
################################################################################
# get corr with IQ
m123 <- inner_join(m1.m2, stats.c)
corr.table(m123 %>% select(colnames(m1.m2),
                           -ends_with("id")),
           m123 %>% select(colnames(stats.c)[-1]))  %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2)), 
         V2 %in% colnames(stats.c)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = sub("P_", "", V2),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=reorder(V2, desc(V2)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(cols = vars(cat2), scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "",
       caption = paste0("n(samples): ", nrow(m123), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_brainvol-IQ-",
                         ".png"),
       width = 8, height = 8, units = "in", dpi = 320, bg = "white")
################################################################################
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
m124 <- inner_join(stats.c,
                   inner_join(pairs.dis, areas))
corr.table(m124 %>% select(colnames(stats.c)[-1]),
           m124 %>% select(c(colnames(areas), 
                             colnames(pairs.dis), -ends_with("id")))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% colnames(stats.c)[-1],
         V2 %in% c(colnames(areas), colnames(pairs.dis))) %>%
  mutate(V2 = factor(V2, levels = unique(V2)),
         cat2 = ifelse(grepl("A_", V2), "Area", "Distance"),
         V2 = sub("A_", "", V2),
         V2 = sub("P_", "", V2)) %>%
  ggplot(aes(x=reorder(V1, desc(V1)), y=V2, fill = r, label = ifelse(FDR<0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size=3, color = "white") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  ggh4x::facet_grid2(rows = vars(cat2), scales = "free", space = "free") +
  labs(y = "", x = "", 
       caption = paste0("n(samples): ", nrow(m124),"\n")) +
  my.guides
ggsave(filename = paste0("figs/corr_brainvol-face-",
                         ".png"),
       width = 8, height = 8, units = "in", dpi = 320, bg = "white")
# make scatterplots
m124 %>%
  pivot_longer(cols = c("A_M_R", "A_M_L", "P_M_V", "P_EB_N_L", "P_EB_E_L", "P_EB_E_R"),
               names_to = "face") %>%
  ggplot(aes(x=VentricleChoroidVol, y=value)) +
  geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color="red")+
  facet_wrap(~face, scales = "free")
ggsave(filename = paste0("figs/corr_brainvol-face-scatter",
                         ".png"),
       width = 8, height = 8, units = "in", dpi = 320, bg = "white")
################################################################################

################################################################################

################################################################################

################################################################################

################################################################################

################################################################################


