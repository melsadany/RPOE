################################################################################
#               transcription of audio files from PS verbal screen             #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
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
################################################################################
# make your list of participants and their cropped audio files
all.files <- data.frame(file = list.files(p.files$file, pattern = "task", full.names = T)) %>%
  mutate(task = sub("_.*", "", sub(".*task-", "", basename(file))),
         word = sub("_.*", "", sub(".*task-[0-9]_", "", basename(file))),
         ID = sub("_task.*", "", basename(file)))

# run whisper on these files. whisper runs only on cropped tasks, not the full audio
registerDoMC(cores = 6)
foreach(i = 1:nrow(all.files)) %dopar% {
  n.dir <- paste0(project.dir, "/", "data/derivatives/PS-VC_transcription/", all.files$ID[i])
  system(paste0("mkdir -p ", n.dir))
  cmd <- paste("whisper",
               all.files$file[i], 
               "--model large-v3",
               "--language English",
               "--verbose True",
               "--output_format tsv",
               "--threads 1",
               "--output_dir", n.dir, 
               sep = " ")
  system(cmd)
}
# clean whisper files and only keep the tsv files
for(j in 1:length(unique(all.files$ID))) {
  for(k in c("json", "srt", "txt", "vtt")){
    cmd <- paste0("rm -rf ", project.dir, "/", "data/derivatives/PS-VC_transcription/", unique(all.files$ID)[j], "/*.", k)
    system(cmd)
  }
}
