################################################################################
#                 processing audio files from the ISS and cropping             #
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
#  read the ISS task metadata
iss.metadata <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 11) %>%
  mutate(start_in_sec = start_in_sec-29,
         end_in_sec = end_in_sec-29)

# keep participants of interest
participants.metadata <- readxl::read_excel("data/raw/RPOE_meta.xlsx", sheet = 12)
p.of.int <- participants.metadata %>% 
  filter(task_version == "1.04") %>%
  mutate(start_in_sec = start_min*60 + start_sec,
         end_in_sec = end_min*60 + end_sec) %>%
  select(te_id, devGenes_id, start_in_sec, end_in_sec)
################################################################################
################################################################################
# copy iss file to my dir, make the mp3 file, and crop per task
data.dir <- "/Dedicated/jmichaelson-sdata/MRI/RPOE"
my.data.dir <- paste0(project.dir, "/data/raw/ISS")
system(paste0("mkdir -p ", my.data.dir))
for (i in 1:nrow(p.of.int)) {
  # i = 1
  pid <- p.of.int$te_id[i]
  raw.a.file <- gsub(" ", fixed = T, "\\ ", 
                     list.files(paste0(data.dir, "/", pid, 
                                       "/phenotype/speech_sample"),
                                pattern = ".mp3", full.names = T))
  a.file <- paste0(my.data.dir, "/", pid, ".mp3")
  if (!file.exists(a.file) && length(raw.a.file) == 0) { # make mp3 file if the raw is only video
    v.file <- gsub(" ", fixed = T, "\\ ", 
                   list.files(paste0(data.dir, "/", pid, "/phenotype/speech_sample"),
                              full.names = T, pattern = ".mp4")[1])
    system(paste0("ffmpeg -i ", v.file, 
                  " -ar 44100 -ac 2 ", 
                  my.data.dir, "/", pid, ".mp3"))
    a.file <- paste0(my.data.dir, "/", pid, ".mp3")
  }else if(!file.exists(a.file)) { # copy the mp3 file, if it doesn't exist in my dir
    system(str_c("cp ", raw.a.file," ", a.file))
  }
  # read the full mp3, and crop the task only
  aud <- readMP3(a.file)
  aud.cropped <- extractWave(aud, xunit = "time",
                             from = p.of.int$start_in_sec[i], 
                             to = p.of.int$end_in_sec[i])
  # save full task response
  system(paste0("mkdir -p ", project.dir, 
                "/data/derivatives/ISS_participants-response/",
                pid))
  writeWave(Wave(left = as.numeric(aud.cropped@left), 
                 samp.rate = 44100, 
                 bit = 16, 
                 pcm=T), 
            filename = paste0(project.dir,
                              "/data/derivatives/ISS_participants-response/",
                              pid, "/", "full-ISS-cropped_",pid, ".wav"), 
            extensible = T)
  # get responses cropped
  # for (j in 1:nrow(iss.metadata)) {
  #   # j = 1
  #   task <- paste0(pid,
  #                  "_task-", iss.metadata$task_num[j], 
  #                  "_", iss.metadata$word[j], "_", j)
  #   t.aud <- extractWave(aud.cropped, 
  #                        from = iss.metadata$start_in_sec[j], 
  #                        to = iss.metadata$end_in_sec[j], 
  #                        xunit = "time")
  #   # plot(t.aud)
  #   # play(t.aud, "play")
  #   # saving the task output
  #   writeWave(Wave(left = as.numeric(t.aud@left), 
  #                  samp.rate = 44100, 
  #                  bit = 16, 
  #                  pcm=T), 
  #             filename = paste0(project.dir,
  #                               "/data/derivatives/ISS_participants-response/",
  #                               pid, "/", task, ".wav"), 
  #             extensible = T)
  # }
}
################################################################################
################################################################################
################################################################################


################################################################################

################################################################################

################################################################################