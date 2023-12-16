################################################################################
#                            processing surfboard output                       #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
# read the metadata for tasks
ps.vc.metadata.r <- read_csv("data/derivatives/PS_VC-task-metadata.csv")
ps.vc.metadata <- ps.vc.metadata.r %>%
  mutate(start_in_sec = start_in_sec - ps.vc.metadata.r$start_in_sec[1],
         end_in_sec = end_in_sec - ps.vc.metadata.r$start_in_sec[1]) %>%
  select(task_num, word, start_in_sec, end_in_sec)
# identify surfboard output files, and select task 2 only
files.meta <- data.frame(ID = rep(list.dirs("data/derivatives/PS-VC_participants-response", recursive = F,full.names = F), each = 35)) %>% 
  mutate(task_num = rep(c(ps.vc.metadata$task_num,"FULL"), length(list.dirs("data/derivatives/PS-VC_participants-response", recursive = F))),
         task_num2 = rep(c(1:35), length(list.dirs("data/derivatives/PS-VC_participants-response", recursive = F, full.names = F))),
         word = rep(c(ps.vc.metadata$word,"FULL"), length(list.dirs("data/derivatives/PS-VC_participants-response", recursive = F))),
         file = paste0("data/derivatives/PS-VC_participants-response/", ID,
                       "_task-", task_num, "_", word, "_", task_num2, ".wav"))  %>%
  filter(task_num==2)
surfboard.all <- read_csv("data/derivatives/surfboard_out/ALL_task-2.csv")
surfboard.all <- cbind(files.meta, surfboard.all)
# save combined csv
write_csv(surfboard.all, "data/derivatives/surfboard_audio_features_ALL.csv")
################################################################################