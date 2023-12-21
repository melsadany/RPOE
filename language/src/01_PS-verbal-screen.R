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
participants.metadata <- readxl::read_excel("data/raw/RPOE_meta.xlsx", sheet = 1)
p.of.int <- participants.metadata %>% 
  # mutate(work_on = ifelse(work_on == "T", T, F)) %>%
  # filter(work_on == T) %>%
  mutate(work_on_2 = ifelse(work_on_2 == "T", T, F)) %>%
  filter(work_on_2 == T) %>%
  mutate(duration = format(as.POSIXct(duration), format = "%H:%M:%S"),
         first_beep = format(as.POSIXct(first_beep), format = "%H:%M:%S"),
         start = as.numeric(sub("^\\d{2}:(\\d{2}):\\d{2}$", "\\1", first_beep)),
         duration_m = as.numeric(sub("^([^:]+).*", "\\1", duration)),
         # duration_m = as.numeric(format(as.POSIXct(firsct_beep), format = "%H")), # apparently R understood minutes as hours, and seconds as minutes
         # duration_s = as.numeric(format(as.POSIXct(first_beep), format = "%M")),
         duration_s = as.numeric(sub("^\\d{2}:(\\d{2}):\\d{2}$", "\\1", duration)),
         duration_in_sec = (60*duration_m)+duration_s
         ) %>%
  select(ID, starts_with("duration"), start, audio, video, `all tasks completed`)
# edit start for 2E_040
p.of.int[which(p.of.int$ID == "2E_040"),"start"] <- 261
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
                             to = p.of.int$duration_in_sec[i])
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
                              pid, "/", "full-PS_VC-cropped_",pid, ".wav"), 
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
    # plot(t.aud)
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
                                pid, "/", task, ".wav"), 
              extensible = T)
  }
}

#######################
# work on the ones that need some cropping
# 2E_042 and 2E_053
p.of.int.2 <- participants.metadata %>% 
  mutate(work_on_2 = ifelse(work_on_2 == "M", T, F)) %>%
  filter(work_on_2 == T) %>%
  mutate(duration = format(as.POSIXct(duration), format = "%H:%M:%S"),
         # first_beep = format(as.POSIXct(first_beep), format = "%H:%M:%S"),
         start = c(90,34), # manually identified of 1:30 and 00:34
         duration_m = as.numeric(sub("^([^:]+).*", "\\1", duration)),
         duration_s = as.numeric(sub("^\\d{2}:(\\d{2}):\\d{2}$", "\\1", duration)),
         duration_in_sec = (60*duration_m)+duration_s
  ) %>%
  select(ID, starts_with("duration"), start, audio, video, `all tasks completed`)
# for 2E_042
i=1
pid <- p.of.int.2$ID[i]
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
                           from = p.of.int.2$start[i],
                           to = p.of.int.2$duration_in_sec[i])
# save full task response
system(paste0("mkdir -p ", project.dir, 
              "/data/derivatives/PS-VC_participants-response/",
              pid))
writeWave(Wave(left = as.numeric(aud.cropped@left), 
               samp.rate = 44100, 
               bit = 16, 
               pcm=T), 
          filename = paste0(project.dir,
                            "/data/derivatives/PS-VC_participants-response/",
                            pid, "/", "full-PS_VC-cropped_",pid, ".wav"), 
          extensible = T)
# get responses cropped
for (j in 1:29) {
  task <- paste0(pid,
                 "_task-", ps.vc.metadata$task_num[j], 
                 "_", ps.vc.metadata$word[j], "_", j)
  t.aud <- extractWave(aud.cropped, 
                       from = ps.vc.metadata$start_in_sec[j], 
                       to = ps.vc.metadata$end_in_sec[j], 
                       xunit = "time")
  writeWave(Wave(left = as.numeric(t.aud@left), 
                 samp.rate = 44100, 
                 bit = 16, 
                 pcm=T), 
            filename = paste0(project.dir,
                              "/data/derivatives/PS-VC_participants-response/",
                              pid, "/", task, ".wav"), 
            extensible = T)
}
# skip a minute before the faces task here
for (j in 30:nrow(ps.vc.metadata)) {
  task <- paste0(pid,
                 "_task-", ps.vc.metadata$task_num[j], 
                 "_", ps.vc.metadata$word[j], "_", j)
  t.aud <- extractWave(aud.cropped, 
                       from = ps.vc.metadata$start_in_sec[j]+63, 
                       to = ps.vc.metadata$end_in_sec[j]+63, 
                       xunit = "time")
  writeWave(Wave(left = as.numeric(t.aud@left), 
                 samp.rate = 44100, 
                 bit = 16, 
                 pcm=T), 
            filename = paste0(project.dir,
                              "/data/derivatives/PS-VC_participants-response/",
                              pid, "/", task, ".wav"), 
            extensible = T)
}
####
# for 2E_052
i=2
pid <- p.of.int.2$ID[i]
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
                           from = p.of.int.2$start[i],
                           to = p.of.int.2$duration_in_sec[i])
# save full task response
system(paste0("mkdir -p ", project.dir, 
              "/data/derivatives/PS-VC_participants-response/",
              pid))
writeWave(Wave(left = as.numeric(aud.cropped@left), 
               samp.rate = 44100, 
               bit = 16, 
               pcm=T), 
          filename = paste0(project.dir,
                            "/data/derivatives/PS-VC_participants-response/",
                            pid, "/", "full-PS_VC-cropped_",pid, ".wav"), 
          extensible = T)
# get responses cropped
for (j in 1:7) {
  task <- paste0(pid,
                 "_task-", ps.vc.metadata$task_num[j], 
                 "_", ps.vc.metadata$word[j], "_", j)
  t.aud <- extractWave(aud.cropped, 
                       from = ps.vc.metadata$start_in_sec[j], 
                       to = ps.vc.metadata$end_in_sec[j], 
                       xunit = "time")
  writeWave(Wave(left = as.numeric(t.aud@left), 
                 samp.rate = 44100, 
                 bit = 16, 
                 pcm=T), 
            filename = paste0(project.dir,
                              "/data/derivatives/PS-VC_participants-response/",
                              pid, "/", task, ".wav"), 
            extensible = T)
}
# skip a minute before the faces task here
for (j in 9:nrow(ps.vc.metadata)) {
  task <- paste0(pid,
                 "_task-", ps.vc.metadata$task_num[j], 
                 "_", ps.vc.metadata$word[j], "_", j)
  t.aud <- extractWave(aud.cropped, 
                       from = ps.vc.metadata$start_in_sec[j]+18, 
                       to = ps.vc.metadata$end_in_sec[j]+18, 
                       xunit = "time")
  writeWave(Wave(left = as.numeric(t.aud@left), 
                 samp.rate = 44100, 
                 bit = 16, 
                 pcm=T), 
            filename = paste0(project.dir,
                              "/data/derivatives/PS-VC_participants-response/",
                              pid, "/", task, ".wav"), 
            extensible = T)
}
################################################################################
################################################################################
######################## PS-VC version 2 participants ##########################
################################################################################
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
ps.vc.metadata.r <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 2) %>%
  filter(task_v==2)
ps.vc.metadata <- ps.vc.metadata.r %>%
  mutate(start_in_sec = start_in_sec - ps.vc.metadata.r$start_in_sec[1],
         end_in_sec = end_in_sec - ps.vc.metadata.r$start_in_sec[1]) %>%
  select(task_num, word, start_in_sec, end_in_sec)
# keep participants of interest
participants.metadata <- readxl::read_excel("data/raw/RPOE_meta.xlsx", sheet = 1)
# change this to every new participant
pid <- "5335"
p.of.int <- participants.metadata %>% 
  filter(ID == pid) %>%
  mutate(duration = format(as.POSIXct(duration), format = "%H:%M:%S"),
         first_beep = format(as.POSIXct(first_beep), format = "%H:%M:%S"),
         start = as.numeric(sub("^\\d{2}:(\\d{2}):\\d{2}$", "\\1", first_beep)),
         duration_m = as.numeric(sub("^([^:]+).*", "\\1", duration)),
         duration_s = as.numeric(sub("^\\d{2}:(\\d{2}):\\d{2}$", "\\1", duration)),
         duration_in_sec = (60*duration_m)+duration_s
  ) %>%
  select(ID, starts_with("duration"), start, audio, video, `all tasks completed`)
################################################################################
################################################################################
# copy ps-vc file to my dir, make the mp3 file, and crop per task
data.dir <- "/Dedicated/jmichaelson-sdata/MRI/RPOE"
my.data.dir <- paste0(project.dir, "/data/raw/PS_vc")
for (i in 1:nrow(p.of.int)) {
  # i = 1
  pid <- p.of.int$ID[i]
  raw.a.file <- gsub(" ", fixed = T, "\\ ", 
                     list.files(paste0(data.dir, "/", pid, 
                                       "/phenotype/PS_verbal_screen"),
                                pattern = ".mp3", full.names = T))
  a.file <- paste0(my.data.dir, "/", pid, ".mp3")
  if (!file.exists(a.file) && length(raw.a.file) == 0) { # make mp3 file if the raw is only video
    v.file <- gsub(" ", fixed = T, "\\ ", 
                   list.files(paste0(data.dir, "/", pid, "/phenotype/PS_verbal_screen"),
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
                             from = p.of.int$start[i], 
                             to = p.of.int$duration_in_sec[i])
  # save full task response
  system(paste0("mkdir -p ", project.dir, 
                "/data/derivatives/PS-VC_participants-response/",
                pid))
  writeWave(Wave(left = as.numeric(aud.cropped@left), 
                 samp.rate = 44100, 
                 bit = 16, 
                 pcm=T), 
            filename = paste0(project.dir,
                              "/data/derivatives/PS-VC_participants-response/",
                              pid, "/", "full-PS_VC-cropped_",pid, ".wav"), 
            extensible = T)
  # get responses cropped
  for (j in 1:nrow(ps.vc.metadata)) {
    # j = 1
    task <- paste0(pid,
                   "_task-", ps.vc.metadata$task_num[j], 
                   "_", ps.vc.metadata$word[j], "_", j)
    t.aud <- extractWave(aud.cropped, 
                         from = ps.vc.metadata$start_in_sec[j], 
                         to = ps.vc.metadata$end_in_sec[j], 
                         xunit = "time")
    # plot(t.aud)
    # play(t.aud, "play")
    # saving the task output
    writeWave(Wave(left = as.numeric(t.aud@left), 
                   samp.rate = 44100, 
                   bit = 16, 
                   pcm=T), 
              filename = paste0(project.dir,
                                "/data/derivatives/PS-VC_participants-response/",
                                pid, "/", task, ".wav"), 
              extensible = T)
  }
}
################################################################################
################################################################################
################################################################################


################################################################################

################################################################################

################################################################################