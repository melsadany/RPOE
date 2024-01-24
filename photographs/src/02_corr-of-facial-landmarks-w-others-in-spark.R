################################################################################
#       correlation between facial landmarks and other features in spark       #
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
################################################################################
# get the facial distance pairs and areas
int.pairs <- c("EB_R", "EB_L", "E_R", "E_L", "M_H", "N_V", "N_H", "M_V", "EB_C","EB_N_R", "EB_N_L", "EB_E_R", "EB_E_L","NT_E_R", "NT_E_L","EB_M_R", "EB_M_L","E_M_R", "E_M_L")
pairs.dis <- read_csv("data/derivatives/pairs-distances-in-spark.csv") %>%
  select(PID, paste0("P_", int.pairs))
areas <- read_csv("data/derivatives/facial.areas-in-spark.csv")
################################################################################
################################################################################
# read tests data
# surprisingly, they have NOTHING in any of the collected data!!!
# core <- read_csv("/sdata/Simons/SPARK/DATA/phenotypes/SPARK_collection_v10_2023-07-17/c") %>%
#   mutate(PID = subject_sp_id) %>%
#   filter(PID %in% pairs.dis$PID)
# get PGS
pgs <- read_csv("/wdata/lcasten/spark/prs/HapMap3_plus/LDPred2-inf-full/gathered_LDPred2-inf_pc_corrected_long.csv") %>%
  filter(IID %in% pairs.dis$PID)
################################################################################
# corr between pairs distances, and PGS
m12 <- inner_join(pgs %>% 
                    pivot_wider(names_from = "pgs_name", values_from = "pgs_pc_corrected", id_cols = "IID"),
                  pairs.dis %>% rename(IID = PID))
corr.table(m12 %>% select(pgs$pgs_name),
           m12 %>% select(starts_with("P_")),
           method = "pearson") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(pgs$pgs_name), 
         V2 %in% colnames(pairs.dis)) %>%
  mutate(V2 = sub("P_", "", V2)) %>%
  left_join(pgs %>% select(V1 = pgs_name, V3 = clean_name)) %>%
  ggplot(aes(x=reorder(V2, desc(V2)), y=reorder(V3, desc(V3)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  # facet_wrap(~cat2, scales = "free", nrow = 1) +
  # ggh4x::facet_grid2(rows = vars(V3), scales = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(y = "", x = "pair # of two facial landmarks points",
       caption = paste0("n(samples): ", nrow(m12), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_facial-distances-pgs-",
                         "in-spark",
                         ".png"),
       width = 10, height = 24, units = "in", dpi = 320, bg = "white")
################################################################################
# corr between facial areas and PGS
m13 <- inner_join(pgs %>% 
                    pivot_wider(names_from = "pgs_name", values_from = "pgs_pc_corrected", id_cols = "IID"),
                  areas %>% rename(IID = PID))
corr.table(m13 %>% select(pgs$pgs_name),
           m13 %>% select(starts_with("A_")),
           method = "pearson") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(pgs$pgs_name), 
         V2 %in% colnames(areas)) %>%
  mutate(V2 = sub("A_", "", V2)) %>%
  left_join(pgs %>% select(V1 = pgs_name, V3 = clean_name)) %>%
  ggplot(aes(x=reorder(V2, desc(V2)), y=reorder(V3, desc(V3)), fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  # facet_wrap(~cat2, scales = "free", nrow = 1) +
  # ggh4x::facet_grid2(rows = vars(V3), scales = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(y = "", x = "facial area",
       caption = paste0("n(samples): ", nrow(m13), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides
ggsave(filename = paste0("figs/corr_facial-areas-pgs-",
                         "in-spark",
                         ".png"),
       width = 10, height = 24, units = "in", dpi = 320, bg = "white")
# plot scatter for ones of int
p1 <- m13 %>% ggplot(aes(x=A_N_R, y=Regular_Pub_Attendance)) + geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color = "red")
p2 <- m13 %>% ggplot(aes(x=A_N_R, y=`hatoum2023-executive_functioning`)) + geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color = "red")
p3 <- m13 %>% ggplot(aes(x=A_N_L, y=schoolE2.better_language_than_math)) + geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color = "red")
p4 <- m13 %>% ggplot(aes(x=A_ES_R, y=`cog_reaction_time-UKB-2020`)) + geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color = "red")
p5 <- m13 %>% ggplot(aes(x=A_CHK_O_R, y=`PGI-MIGRAINE1_excl_23andMe_single_gwide_sumstats.txt`)) + geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color = "red")
p6 <- m13 %>% ggplot(aes(x=A_CHK_O_R, y=schoolE3.better_oral_than_written_exams)) + geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color = "red")
p7 <- m13 %>% ggplot(aes(x=A_CHK_O_L, y=schoolE2.better_language_than_math)) + geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color = "red")
p8 <- m13 %>% ggplot(aes(x=A_CHK_I_R, y=Regular_Pub_Attendance)) + geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color = "red")
p9 <- m13 %>% ggplot(aes(x=A_CHK_I_L, y=schoolE2.better_language_than_math)) + geom_point(size=2)+geom_smooth(method = "lm")+ggpubr::stat_cor(color = "red")
patchwork::wrap_plots(p1,p2,p3,p4,p5,p6,p7,p8,p9)
ggsave(filename = paste0("figs/corr_facial-areas-pgs-",
                         "in-spark-scatter",
                         ".png"),
       width = 10, height = 12, units = "in", dpi = 320, bg = "white")
################################################################################


################################################################################


################################################################################


################################################################################


################################################################################


################################################################################


################################################################################