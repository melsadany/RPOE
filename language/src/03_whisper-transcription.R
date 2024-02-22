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
ps.vc.metadata.r <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 2) %>%
  # filter(task_v==1)
  filter(task_v==2)
ps.vc.metadata <- ps.vc.metadata.r %>%
  mutate(start_in_sec = start_in_sec - ps.vc.metadata.r$start_in_sec[1],
         end_in_sec = end_in_sec - ps.vc.metadata.r$start_in_sec[1]) %>%
  select(task_num, word, start_in_sec, end_in_sec)
################################################################################
# make your list of participants and their cropped audio files
p.files <- data.frame(file = list.dirs("data/derivatives/PS-VC_participants-response", recursive = F, full.names = T))
all.files <- data.frame(file = list.files(p.files$file, pattern = "task", full.names = T)) %>%
  mutate(task = sub("_.*", "", sub(".*task-", "", basename(file))),
         word = sub("_.*", "", sub(".*task-[0-9]_", "", basename(file))),
         ID = sub("_task.*", "", basename(file)))
# save the file paths to run whisper on argon bash for loop?
write_lines(all.files$file, "data/derivatives/cropped-audio-files-for-whisper")

######
# if running for a new/specific participant
pid <- c("2E_094")
all.files <- all.files %>%
  filter(ID %in% pid)
######

# run whisper on these files. whisper runs only on cropped tasks, not the full audio
registerDoMC(cores = 6)
foreach(i = 1:nrow(all.files)) %dopar% {
# foreach(i = 266:272) %dopar% {
  n.dir <- paste0(project.dir, "/", "data/derivatives/PS-VC_transcription/", all.files$ID[i])
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
  system(cmd)
}
# clean whisper files and only keep the tsv files
for(j in 1:length(unique(all.files$ID))) {
  for(k in c("json", "srt", "txt", "vtt")){
    cmd <- paste0("rm -rf ", project.dir, "/", "data/derivatives/PS-VC_transcription/", unique(all.files$ID)[j], "/*.", k)
    system(cmd)
  }
}
################################################################################
################################################################################
# combine whisper transcription files per participants
whisper.files <- all.files %>%
  mutate(ID = sub("_task.*", "", basename(file))) %>%
  mutate(file = sub("participants-response", "transcription", sub("\\.wav", ".wav.words.tsv", file)))
registerDoMC(cores = 3)
all.transcriptions <- foreach(i = 1:nrow(whisper.files), .combine = rbind) %dopar% {
  t <- read_tsv(whisper.files$file[i]) %>%
    mutate(task = whisper.files$task[i],
           word = whisper.files$word[i],
           ID = whisper.files$ID[i])
  return(t)
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