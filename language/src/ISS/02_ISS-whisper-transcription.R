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

# get the sentence repetition transcription
srt.transcription <- all.transcriptions %>%
  group_by(te_id) %>%
  slice_head(n=20)


# save transcriptions
write_tsv(srt.transcription, "data/derivatives/ISS_transcription/SRT-transcription-raw-timestamped.tsv")
################################################################################
# after saving the files combined, you should manually revise the transcription
# read the revised version and keep it for downstream analysis
################################################################################
################################################################################


################################################################################


################################################################################