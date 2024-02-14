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
vitals <- readxl::read_xlsx("../language/data/raw/RPOE Stats.xlsx", sheet = 1)
colnames(vitals)[2:3] <- c("devGenes_id", "te_id")
m123 <- inner_join(vitals, m1.m2)
corr.table(m123 %>%
             select(colnames(m1.m2), -ends_with("id")),
           m123 %>% select(height = `Height (m)`, weight = `Weight (kg)`, 
                           HR, Temp, D02, head_circ = `Head Circ (in)`) %>%
             mutate_all(.funs = function(x) as.numeric(x))) %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2)),
         V2 %in% c("height", "weight", "HR", "D02", "head_circ", "Temp")) %>%
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
  labs(x = "", y = "average similarity of words said",
       caption = paste0("n(samples): ", nrow(m123), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave("figs/corr_iq-nih-vitals.png",
       width = 10, height = 8, units = "in", bg = "white", dpi = 320)
################################################################################
################################################################################
################################################################################
