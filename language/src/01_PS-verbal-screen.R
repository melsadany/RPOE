################################################################################
#                     processing audio files from PS verbal screen             #
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
# groundtruth data
# library(signal);library(tuneR)
# gt <- readMP3("/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/RPOE/speech_sample/groundtruth/PSVC/230802_1241_blank.mp3")
# j.aud <- readMP3("/Dedicated/jmichaelson-wdata/jmichaelson/NDVR/RPOE/speech_sample/groundtruth/PSVC/230802_1253_jake.mp3")
# # c.j.aud <- extractWave(p.aud, xunit = "time", from = 20, to=40)
# beep_ref <- extractWave(gt,xunit = "time", from = 26.1, to = 26.5)  # exact seconds for beep sound
# beep_ref_numeric <- beep_ref@left
# # cross_correlation <- convolve(c.p.aud@left, beep_ref_numeric, type = "open")
################################################################################


################################################################################
# trial run for getting audio features for one of the participants
data.dir <- "/Dedicated/jmichaelson-sdata/MRI/RPOE"
my.data.dir <- paste0(project.dir, "/data/raw/PS_vc")
for (i in 1:nrow(p.of.int)) {
  # i = 2
  pid <- p.of.int$ID[i]
  raw.a.file <- gsub(" ", fixed = T, "\\ ", 
                     list.files(paste0(data.dir, "/", pid, 
                                       "/phenotype/PS_verbal_screen"),
                                pattern = ".mp3", full.names = T))
  a.file <- paste0(my.data.dir, "/", pid, ".mp3")
  if (!file.exists(a.file) && length(raw.a.file) == 0) {
    v.file <- gsub(" ", fixed = T, "\\ ", 
                   list.files(paste0(data.dir, "/", pid, "/phenotype/PS_verbal_screen"),
                         full.names = T, pattern = ".mp4")[1])
    system(paste0("ffmpeg -i ", v.file, 
                  " -ar 44100 -ac 2 ", 
                  my.data.dir, "/", pid, ".mp3"))
    a.file <- paste0(my.data.dir, "/", pid, ".mp3")
  }else if(!file.exists(a.file)) {
    system(str_c("cp ", raw.a.file," ", a.file))
  }
  aud <- readMP3(a.file)
  aud.cropped <- extractWave(aud, xunit = "time",
                             from = p.of.int$start[i], 
                             to = p.of.int$duration_in_sec)
  # save full task response
  system(paste0("mkdir -p ", project.dir, 
                "/data/derivatives/PS-VC_participants-response/",
                pid))
  # seewave::savewav(wave = aud.cropped, f = 44100, channel = "left",
  #                  filename = paste0(project.dir,
  #                                    "/data/derivatives/PS-VC_participants-response/",
  #                                    pid, "/", "full-PS_VC-cropped_",pid, ".mp3"))
  writeWave(Wave(left = as.numeric(aud.cropped@left), 
                 samp.rate = 44100, 
                 bit = 16, 
                 pcm=T), 
            filename = paste0(project.dir,
                              "/data/derivatives/PS-VC_participants-response/",
                              pid, "/", "full-PS_VC-cropped_",pid, ".mp3"), 
            extensible = T)
  # get responses cropped
  for (j in 1:nrow(ps.vc.metadata)) {
    # j = 1
    # o.task.start <- p.of.int$start[i]+ps.vc.metadata$start_in_sec[j]
    # o.task.end <- p.of.int$duration_in_sec[i]+ps.vc.metadata$end_in_sec[j]
    task <- paste0(pid,
                   "_task-", ps.vc.metadata$task_num[j], 
                   "_", ps.vc.metadata$word[j], "_", j)
    t.aud <- extractWave(aud.cropped, 
                         from = ps.vc.metadata$start_in_sec[j], 
                         to = ps.vc.metadata$end_in_sec[j], 
                         xunit = "time")
    plot(t.aud)
    # play(t.aud, "play")
    # saving the task output
    # seewave::savewav(wave = t.aud,  f = 44100, channel = "left",
    #                  filename = paste0(project.dir,
    #                                    "/data/derivatives/PS-VC_participants-response/",
    #                                    pid, "/", task, ".mp3"))
    writeWave(Wave(left = as.numeric(t.aud@left), 
                   samp.rate = 44100, 
                   bit = 16, 
                   pcm=T), 
              filename = paste0(project.dir,
                                "/data/derivatives/PS-VC_participants-response/",
                                pid, "/", task, ".mp3"), 
              extensible = T)
  }
}



################################################################################
################################################################################

################################################################################

################################################################################

################################################################################