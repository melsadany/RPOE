################################################################################
#                   checking correlation between PS_VC and other tests         #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(tuneR)
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
#  read the PS_VC task metadata
ps.vc.metadata.r <- read_csv("data/derivatives/PS_VC-task-metadata.csv")
ps.vc.metadata <- ps.vc.metadata.r %>%
  mutate(start_in_sec = start_in_sec - ps.vc.metadata.r$start_in_sec[1],
         end_in_sec = end_in_sec - ps.vc.metadata.r$start_in_sec[1]) %>%
  select(task_num, word, start_in_sec, end_in_sec)

# keep participants of interest
participants.metadata <- read_csv("data/derivatives/PS_VC-participants-metadata.csv")
p.of.int <- participants.metadata %>% 
  filter(work_on == T) %>%
  mutate(start = as.numeric(sub("^\\d{2}:(\\d{2}):\\d{2}$", "\\1", first_beep)),
         duration_m = as.numeric(sub("^([^:]+).*", "\\1", duration)),
         duration_s = as.numeric(sub("^\\d{2}:(\\d{2}):\\d{2}$", "\\1", duration)),
         duration_in_sec = (60*duration_m)+duration_s) %>%
  select(ID, starts_with("duration"), start, audio, video, `all tasks completed`)
################################################################################
# read tests data
nih.tb <- read_csv("data/derivatives/nih-tb_clean_111523.csv")
iq <- read_csv("data/derivatives/wisc-and-wais_clean_111523.csv")
################################################################################
# check correlation between NIH-TB and IQ
m1 <- nih.tb %>%
  select(dev_id, 
         ends_with("age_corrected_standard_score"))
m2 <- iq %>%
  select(dev_id, `2e_id`,
         ends_with("composite_score"),FSIQ, 
         SI, VC, BD, VP, MR, FW, DS, PS, CD, SS, IN, AR)
m1.m2 <- inner_join(m1, m2) %>%
  mutate(`2e_id`=ifelse(is.na(`2e_id`), dev_id, `2e_id`)) %>%
  rename(te_id = `2e_id`)
corr.table(m1.m2 %>% select(any_of(colnames(m1)), - ends_with("_id")),
           m1.m2 %>% select(any_of(colnames(m2)), - ends_with("_id"))) %>%
  filter(V1 %in% colnames(m1),
         V2 %in% colnames(m2)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "", V1),
         V2 = sub("_composite_score", "", V2)) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(pval<0.05, "*","")))+
  geom_tile()+
  geom_text(size = 3)+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(y = "IQ", x = "NIH_TB") +
  my.guides
################################################################################
# read response sound to task-4 faces1 of all participants 
registerDoMC(cores = 1)
all.ps.vc <- foreach(i = 1:nrow(p.of.int), .combine = rbind) %dopar% {
# for(i in 1:nrow(p.of.int)) {
  pid <- p.of.int$ID[i]
  task <- readWave(paste0(project.dir, "/data/derivatives/PS-VC_participants-response/", 
                         pid, "/", pid, "_task-4_faces1_30.mp3"))
  # print(task)
  # print(paste0("participant: ", pid, " has a record of: ", length(task@left)/task@samp.rate))
  t <- data.frame(te_id = pid, value = as.numeric(task@left), sample = c(1:length(as.numeric(task@left))))
  return(t)
}
all.ps.vc <- all.ps.vc %>%
  pivot_wider(names_from = "sample", values_from = "value", id_cols = te_id)
# check correlation between response and iq/nih 
m123 <- inner_join(m1.m2, all.ps.vc) %>%
  filter(dev_id != "4247_2") %>%
  select(-c(AR, IN, PS, FW, starts_with("PRI_"), starts_with("FR_"), starts_with("VSI_")))
corr.func(m123 %>% select(any_of(colnames(m1.m2)), -ends_with("id")),
           m123 %>% select(any_of(colnames(all.ps.vc)), -te_id))
################################################################################

################################################################################

################################################################################


