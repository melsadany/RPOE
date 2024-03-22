################################################################################
#               transcription of audio files from the speech sample            #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
#  read the ISS task metadata
ps.vc.metadata.r <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 2) %>%
  # filter(task_v==1)
  filter(task_v==2)
ps.vc.metadata <- ps.vc.metadata.r %>%
  mutate(start_in_sec = start_in_sec - ps.vc.metadata.r$start_in_sec[1],
         end_in_sec = end_in_sec - ps.vc.metadata.r$start_in_sec[1]) %>%
  select(task_num, word, start_in_sec, end_in_sec)
################################################################################
# make your list of participants and their cropped audio files
p.files <- data.frame(file = list.dirs("data/derivatives/ISS_participants-response", recursive = F, full.names = T))
all.files <- data.frame(file = list.files(p.files$file, pattern = "full", full.names = T)) %>%
  mutate(te_id = sub(".*-cropped_", "", sub("\\.wav", "", file)))
######
# if running for a new/specific participant
pid <- c("2E_094")
all.files <- all.files %>%
  filter(te_id %in% pid)
######
# run whisper on these files. whisper runs only on cropped tasks, not the full audio
registerDoMC(cores = 6)
foreach(i = 1:nrow(all.files)) %dopar% {
# foreach(i = 266:272) %dopar% {
  n.dir <- paste0(project.dir, "/", "data/derivatives/ISS_transcription/", all.files$ID[i])
  system(paste0("mkdir -p ", n.dir))
  cmd <- paste("whisper_timestamped",
               all.files$file[i], 
               "--model large-v3",
               "--language en",
               "--accurate",
               "--punctuations_with_words False",
               "--verbose True",
               "--detect_disfluencies True",
               "--output_format tsv",
               "--threads 1",
               "--output_dir", n.dir, 
               sep = " ")
  # print(cmd)
  system(cmd)
}
################################################################################
################################################################################
# combine whisper transcription files per participants
whisper.files <- all.files %>%
  mutate(file = sub("participants-response", "transcription", sub("\\.wav", ".wav.tsv", file)),
         file = sub("2E_0[0-9]+/", "", file))
registerDoMC(cores = 3)
all.transcriptions <- foreach(i = 1:nrow(whisper.files), .combine = rbind) %dopar% {
  if (file.exists(whisper.files$file[i])) {
    t <- read_tsv(whisper.files$file[i]) %>%
      mutate(te_id = whisper.files$te_id[i])
    return(t)
  } else {
    return(NULL)
  }
}

t13.transcriptions <- left_join(all.transcriptions, 
                                ps.vc.metadata %>% mutate(task_order = c(1:nrow(ps.vc.metadata))) %>% select(-task_num), 
                                relationship = "many-to-many") %>%
  filter(task %in% c("1","3")) %>%
  mutate(text = str_replace_all(text, "\\.", ""),
         text = str_replace_all(text, "\\.\\.\\.", ""),
         text = tolower(text))  %>% # Remove ...
  filter(text != "[*]",
         text != ".",
         nchar(text) > 0) %>%
  group_by(ID, task_order) %>%
  arrange(.by_group = T)
t24.transcriptions <- left_join(all.transcriptions, 
                                ps.vc.metadata %>% mutate(task_order = c(1:nrow(ps.vc.metadata))) %>% select(-task_num), 
                                relationship = "many-to-many") %>%
  filter(task %in% c("2","4"))  %>%
  group_by(ID, task_order) %>%
  arrange(.by_group = T)
# save combined transcriptions
write_tsv(t13.transcriptions, "data/derivatives/PS-VC_transcription/task-1-and-3-all-together-whisper-transcription-raw-timestamped.tsv")
write_tsv(t24.transcriptions, "data/derivatives/PS-VC_transcription/task-2-and-4-all-together-whisper-transcription-raw-timestamped.tsv")
################################################################################
# after saving the files combined, you should manually revise the transcription
# read the revised version and keep it for downstream analysis

################################################################################
# # extract "um"
# ums <- t13.transcriptions %>%
#   filter(grepl("um", text, ignore.case = T)&nchar(text)==2) %>%
#   group_by(word, ID) %>%
#   dplyr::summarise(count = n())
################################################################################
################################################################################


################################################################################


################################################################################