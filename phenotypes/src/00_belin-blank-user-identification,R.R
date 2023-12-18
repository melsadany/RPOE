################################################################################
#                   identify users with difference in their scales             #
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
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/phenotypes"
setwd(project.dir)
################################################################################
users.meta <- read_csv("/sdata/devGenes/2e_belin_blank/2e_processed_data_final.csv")
users.filtered <- users.meta %>%
  select(2:9, 25:28, adhd_or_asd,
         starts_with("WPPSI")&ends_with("score"),
         starts_with("WISC")&ends_with("score"),
         starts_with("WAIS")&ends_with("score"))

# wpps.III <- users.filtered %>%
#   mutate(age = age(Date.of.Birth)) %>%
#   select(Record.ID, age, sex = Sex..Assigned.at.Birth.,adhd_or_asd,
#          WPPSI.III.Verbal.Index.Score, WPPSI.III.Processing.Speed.Index.Score, WPPSI.III.Full.Index.Score) %>%
#   drop_na(starts_with("WPPS")) %>%
#   mutate(V_P = WPPSI.III.Verbal.Index.Score-WPPSI.III.Processing.Speed.Index.Score,
#          cat = "WPPSI-III")
# wpps %>% 
#   ggplot(aes(x=V_P, fill = adhd_or_asd)) +
#   geom_histogram() +
#   ggh4x::facet_grid2(rows = vars(sex), scales = "free", space = "free", independent = T) +
#   labs(x= "Verbal Index Score - Processing Speed Index Score")

wisc.IV <- users.filtered %>%
  mutate(age = age(Date.of.Birth)) %>%
  select(Record.ID, age, sex = Sex..Assigned.at.Birth.,adhd_or_asd,
         VC = WISC.IV.Verbal.Comprehension.Index.Score, PS = WISC.IV.Processing.Speed.Index.Score, FSIQ=WISC.IV.Full.Scale.IQ.Index.Score) %>%
  drop_na(VC,PS) %>%
  mutate(V_P = VC-PS,
         wisc = "WISC-IV")
wisc.V <- users.filtered %>%
  mutate(age = age(Date.of.Birth)) %>%
  select(Record.ID, age, sex = Sex..Assigned.at.Birth.,adhd_or_asd,
         VC=WISC.V.Verbal.Comprehension.Index.Score, PS=WISC.V.Processing.Speed.Index.Score, FSIQ=WISC.V.Full.Scale.Index.Score) %>%
  drop_na(VC,PS) %>%
  mutate(V_P = VC-PS,
         wisc = "WISC-V")
wisc <- rbind(wisc.IV %>% select(1:4, V_P, wisc, FSIQ), wisc.V %>% select(1:4, V_P, wisc, FSIQ))

p1 <- wisc %>% 
  ggplot(aes(x=V_P, fill = adhd_or_asd)) +
  geom_histogram() +
  ggh4x::facet_grid2(rows = vars(sex), cols = vars(wisc), scales = "free", space = "free", independent = T) +
  scale_fill_manual(values = ten.colors) +
  labs(x= "VCI Score - PSI Score", title = "labeled by diagnosis group")


p2 <- wisc %>% 
  mutate(age_cat = ifelse(age <= 10, "<=10",
                          ifelse(age <=20, "11-20",
                                 ifelse(age <=30, "21-30",
                                        ifelse(age <=40, "31-40",">40")))),
         age_cat = factor(age_cat, levels = c("<=10", "11-20", "21-30", "31-40", ">40"))) %>%
  ggplot(aes(x=V_P, fill = age_cat)) +
  geom_histogram() +
  ggh4x::facet_grid2(rows = vars(sex), cols = vars(wisc), scales = "free", space = "free", independent = T) +
  scale_fill_manual(values = ten.colors) +
  labs(x= "VCI Score - PSI Score", title = "labeled by age group")

p3 <- wisc %>% 
  mutate(FSIQ_cat = ifelse(FSIQ <= 80, "<=80",
                          ifelse(FSIQ <=90, "81-90",
                                 ifelse(FSIQ <=100, "91-100",
                                        ifelse(FSIQ <=110, "101-110",
                                               ifelse(FSIQ <=120, "111-120",
                                                      ifelse(FSIQ <=130, "121-130",
                                                             ifelse(FSIQ <=140, "131-140",">140"))))))),
         FSIQ_cat = factor(FSIQ_cat, levels = c("<=80", "81-90", "91-100", "101-110", "111-120", "121-130", "131-140", ">140"))) %>%
  ggplot(aes(x=V_P, fill = FSIQ_cat)) +
  geom_histogram() +
  ggh4x::facet_grid2(rows = vars(sex), cols = vars(wisc), scales = "free", space = "free", independent = T) +
  scale_fill_manual(values = ten.colors) +
  labs(x= "VCI Score - PSI Score", title = "labeled by FSIQ group")
wisc.p <- patchwork::wrap_plots(p1,p2,p3, ncol = 3)
ggsave(wisc.p, filename = "figs/WISC-VC-PS-stats.png", bg = "white",
       width = 16, height = 7, units = "in", dpi = 320)

####



wais.III <- users.filtered %>%
  mutate(age = age(Date.of.Birth)) %>%
  select(Record.ID, age, sex = Sex..Assigned.at.Birth.,adhd_or_asd,
         VC=WAIS.III.Verbal.Comprehension.Index.Score, PS=WAIS.III.Processing.Speed.Index.Score, FSIQ=WAIS.III.Full.Scale.Index.Score) %>%
  drop_na(VC,PS) %>%
  mutate(V_P = VC-PS,
         wais = "WAIS-III")
wais.IV <- users.filtered %>%
  mutate(age = age(Date.of.Birth)) %>%
  select(Record.ID, age, sex = Sex..Assigned.at.Birth.,adhd_or_asd,
         VC=WAIS.IV.Verbal.Comprehension.Index.Score, PS=WAIS.IV.Processing.Speed.Index.Score, FSIQ=WAIS.IV.Full.Scale.Index.Score) %>%
  drop_na(VC,PS) %>%
  mutate(V_P = VC-PS,
         wais = "WAIS-IV")
wais <- rbind(wais.III %>% select(1:4, V_P, wais, FSIQ), wais.IV %>% select(1:4, V_P, wais, FSIQ))
p1 <- wais %>% 
  ggplot(aes(x=V_P, fill = adhd_or_asd)) +
  geom_histogram() +
  ggh4x::facet_grid2(rows = vars(sex), cols = vars(wais), scales = "free", space = "free", independent = T) +
  scale_fill_manual(values = ten.colors) +
  labs(x= "VCI Score - PSI Score", title = "labeled by diagnosis group")


p2 <- wais %>% 
  mutate(age_cat = ifelse(age <= 10, "<=10",
                          ifelse(age <=20, "11-20",
                                 ifelse(age <=30, "21-30",
                                        ifelse(age <=40, "31-40",">40")))),
         age_cat = factor(age_cat, levels = c("<=10", "11-20", "21-30", "31-40", ">40"))) %>%
  ggplot(aes(x=V_P, fill = age_cat)) +
  geom_histogram() +
  ggh4x::facet_grid2(rows = vars(sex), cols = vars(wais), scales = "free", space = "free", independent = T) +
  scale_fill_manual(values = ten.colors) +
  labs(x= "VCI Score - PSI Score", title = "labeled by age group")

p3 <- wais %>% 
  mutate(FSIQ_cat = ifelse(FSIQ <= 80, "<=80",
                           ifelse(FSIQ <=90, "81-90",
                                  ifelse(FSIQ <=100, "91-100",
                                         ifelse(FSIQ <=110, "101-110",
                                                ifelse(FSIQ <=120, "111-120",
                                                       ifelse(FSIQ <=130, "121-130",
                                                              ifelse(FSIQ <=140, "131-140",">140"))))))),
         FSIQ_cat = factor(FSIQ_cat, levels = c("<=80", "81-90", "91-100", "101-110", "111-120", "121-130", "131-140", ">140"))) %>%
  ggplot(aes(x=V_P, fill = FSIQ_cat)) +
  geom_histogram() +
  ggh4x::facet_grid2(rows = vars(sex), cols = vars(wais), scales = "free", space = "free", independent = T) +
  scale_fill_manual(values = ten.colors) +
  labs(x= "VCI Score - PSI Score", title = "labeled by FSIQ group")
wais.p <- patchwork::wrap_plots(p1,p2,p3, ncol = 3)
ggsave(wais.p, filename = "figs/WAIS-VC-PS-stats.png", bg = "white",
       width = 16, height = 7, units = "in", dpi = 320)

#####