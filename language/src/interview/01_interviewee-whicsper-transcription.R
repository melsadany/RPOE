################################################################################
#               transcription of audio files from interview audios             #
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
# keep participants of interest
participants.metadata <- readxl::read_excel("data/raw/RPOE_meta.xlsx", sheet = 7) %>%
  drop_na(splitted_audio)
p.of.int <- participants.metadata %>% 
  mutate(splitted_audio = ifelse(splitted_audio == "T", T, F)) %>%
  filter(splitted_audio == T) 
################################################################################
# get file paths for subject audio files
registerDoMC(cores = 3)
files <- foreach(i = 1:nrow(p.of.int), .combine = rbind) %dopar% {
  data.frame(file = list.files(paste0("/Dedicated/jmichaelson-sdata/MRI/RPOE/",
                                      p.of.int$te_id[i], "/phenotype/interview/split"), 
                               pattern = "SUBJECT_audio", full.names = T)) %>%
    mutate(te_id = p.of.int$te_id[i]) %>%
    filter(grepl("\\.m4a", file))
}
################################################################################
# run the whisper transcription
registerDoMC(cores = 5)
for(i in 1:nrow(files)) {
  n.dir <- paste0(project.dir, "/", "data/derivatives/interview_transcription/", files$te_id[i])
  system(paste0("mkdir -p ", n.dir))
  cmd <- paste("whisper_timestamped",
               files$file[i], 
               "--model large-v3",
               "--language en",
               "--accurate",
               "--punctuations_with_words False",
               "--verbose True",
               "--detect_disfluencies True",
               "--output_format tsv",
               "--threads 30",
               "--output_dir", n.dir, 
               sep = " ")
  print(paste0(paste0("mkdir -p ", n.dir),";",cmd))
  # system(cmd)
}
################################################################################
# combine the interview transcription for revision
int.f <- data.frame(dir = list.dirs("data/derivatives/interview_transcription", recursive = F)) %>%
  mutate(te_id = basename(dir)) %>%
  # filter(!te_id %in% c("2E_043", "2E_041", "2E_084", "2E_085", "2E_086", "2E_087", "2E_088")) %>%
  mutate(file = list.files(paste0(dir, "/"), pattern = "\\.m4a\\.tsv", full.names = T))
int.all <- foreach(i=1:nrow(int.f), .combine = rbind) %dopar% {
  df <- read_tsv(int.f$file[i]) %>%
    filter(nchar(text) > 1)
  data.frame(text = df$text,
             te_id = int.f$te_id[i])
}
write_csv(int.all, "data/derivatives/interview_transcription/all-transcriptions-long.csv")
################################################################################
# combine the interview transcription for LIWC?
int.f <- data.frame(dir = list.dirs("data/derivatives/interview_transcription", recursive = F)) %>%
  mutate(te_id = basename(dir),
         file = list.files(paste0(dir, "/"), pattern = "\\.m4a\\.tsv", full.names = T))
int.all <- foreach(i=1:nrow(int.f), .combine = rbind) %dopar% {
  df <- read_tsv(int.f$file[i]) %>%
    filter(nchar(text) > 1)
  data.frame(text = df$text,
             te_id = int.f$te_id[i]) %>%
    group_by(te_id) %>%
    dplyr::summarise(text = paste(text, collapse = ". "))
}
write_csv(int.all, "data/derivatives/interview_transcription/all-transcriptions.csv")
################################################################################



################################################################################
################################################################################
