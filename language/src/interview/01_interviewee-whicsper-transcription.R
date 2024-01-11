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
               "--threads 10",
               "--output_dir", n.dir, 
               sep = " ")
  print(paste0(paste0("mkdir -p ", n.dir),";",cmd))
  # system(cmd)
}

