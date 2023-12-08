################################################################################
#                   checking correlation between PS_VC and other tests         #
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
# read tests data
nih.tb <- read_csv("data/derivatives/nih-tb_clean_120623.csv")
iq <- read_csv("data/derivatives/wisc-and-wais_clean_120623.csv")
################################################################################
# check correlation between NIH-TB and IQ
m1 <- nih.tb %>%
  select(dev_id, 
         ends_with("age_corrected_standard_score")) %>%
  drop_na()
m2 <- iq %>%
  select(dev_id, te_id,
         paste0(c("PSI", "WM", "VCI"), "_composite_score"),
         # ends_with("composite_score"),
         FSIQ, 
         SI, VC, BD, VP, MR, 
         #FW, PS, IN, AR, 
         DS, CD, SS)
m1.m2 <- inner_join(m1, m2) %>%
  mutate(te_id=ifelse(is.na(te_id), dev_id, te_id)) %>%
  rename(te_id = te_id)
corr.table(m1.m2 %>% select(any_of(colnames(m1)), - ends_with("_id")),
           m1.m2 %>% select(any_of(colnames(m2)), - ends_with("_id"))) %>%
  filter(V1 %in% colnames(m1),
         V2 %in% colnames(m2)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "", V1),
         V2 = sub("_composite_score", "", V2)) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(pval<0.05, "*","")))+
  geom_tile()+
  geom_text(size = 3)+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(y = "IQ", x = "NIH_TB") +
  my.guides
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
# dump
####
# read response sound to task-4 faces1 of all participants 
registerDoMC(cores = 1)
all.ps.vc <- foreach(i = 1:nrow(p.of.int), .combine = rbind) %dopar% {
# for(i in 1:nrow(p.of.int)) {
  pid <- p.of.int$ID[i]
  task <- readWave(paste0(project.dir, "/data/derivatives/PS-VC_participants-response/", 
                          pid, "/", pid, "_task-1_alone_9.mp3"))
                          # pid, "/", pid, "_task-4_faces1_30.mp3"))
  # print(task)
  # print(paste0("participant: ", pid, " has a record of: ", length(task@left)/task@samp.rate))
  t <- data.frame(te_id = pid, value = as.numeric(task@left), sample = c(1:length(as.numeric(task@left))))
  return(t)
}
all.ps.vc <- all.ps.vc %>%
  pivot_wider(names_from = "sample", values_from = "value", id_cols = te_id)
# check correlation between response and iq/nih 
m123 <- inner_join(m1.m2, all.ps.vc) %>%
  filter(dev_id != "4247_2") %>%
  select(-c(AR, IN, PS, FW, starts_with("PRI_"), starts_with("FR_"), starts_with("VSI_")))
corr.func(m123 %>% select(any_of(colnames(m1.m2)), -ends_with("id")),
           m123 %>% select(any_of(colnames(all.ps.vc)), -te_id))
################################################################################
################################################################################
################################################################################
# correlation between PSVC acoustic features and iq/nih
surfboard <- read_csv("data/derivatives/surfboard_audio_features_ALL.csv") %>% 
  rename(te_id=ID)
m123 <- inner_join(m1.m2,surfboard)

# FULL audio
corr.table(m123 %>% filter(word=="FULL") %>% select(any_of(c(colnames(m1), colnames(m2))),
                                                    -ends_with("id")),
           m123 %>% filter(word=="FULL") %>% select(any_of(colnames(surfboard)[-c(1:5)]) & 
                                                    (contains("mean") | 
                                                       contains("Jitter") | 
                                                       contains("Shimmer") | 
                                                       contains("hnr") | 
                                                       contains("dfa")),
                                                    -contains("derivative"))) %>%
  filter(V1 %in% c(colnames(m1), colnames(m2)), V2 %in% colnames(surfboard)[-c(1:5)]) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(pval<0.05, "*","")))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "IQ & NIH-TB", y = "surfboard") +
  my.guides
ggsave(filename = paste0("figs/iq-nih-ps-vc-acoustics/corr_iq-nih-acoustic-features_FULL.png"),
       width = 6, height = 6, units = "in", dpi = 320, bg = "white")

foreach(i = 1:nrow(ps.vc.metadata)) %dopar% {
  t <- ps.vc.metadata$task_num[i]
  w <- ps.vc.metadata$word[i]
  corr.table(m123 %>% filter(word==w) %>% select(any_of(c(colnames(m1), colnames(m2))),
                                                 -ends_with("id")),
             m123 %>% filter(word==w) %>% select(any_of(colnames(surfboard)[-c(1:5)]) & 
                                                   (contains("mean") | 
                                                      contains("Jitter") | 
                                                      contains("Shimmer") | 
                                                      contains("hnr") | 
                                                      contains("dfa")),
                                                 -contains("derivative"))) %>%
    filter(V1 %in% c(colnames(m1), colnames(m2)), V2 %in% colnames(surfboard)[-c(1:5)]) %>%
    mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
    mutate(V1 = factor(V1, levels = unique(V1)),
           V2 = factor(V2, levels = unique(V2))) %>%
    ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(pval<0.05, "*","")))+
    geom_tile()+
    geom_text(size = 3, color = "white")+
    scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
    labs(x = "IQ & NIH-TB", y = "surfboard") +
    my.guides
  ggsave(filename = paste0("figs/iq-nih-ps-vc-acoustics/corr_iq-nih-acoustic-features_task-",t,"_", w,".png"),
         width = 6, height = 6, units = "in", dpi = 320, bg = "white")
}
################################################################################
################################################################################
################################################################################
# correlation between PS-VC word responses and iq-nih
vc.transcription <- read_rds("data/derivatives/PS-VC_transcription/task-1-3-all-together.rds") %>%
  filter(!((grepl("um", text, ignore.case = T)&nchar(text)==2)|(grepl("uh", text, ignore.case = T)&nchar(text)==2))) %>%
  rename(te_id=ID)
ums <- read_rds("data/derivatives/PS-VC_transcription/task-1-3-all-together.rds") %>%
  filter((grepl("um", text, ignore.case = T)&nchar(text)==2)|(grepl("uh", text, ignore.case = T)&nchar(text)==2)) %>%
  group_by(ID) %>%
  dplyr::summarise(ums_count = n()) %>%
  rename(te_id=ID) %>%
  full_join(vc.transcription %>% distinct(te_id)) %>%
  mutate(ums_count = ifelse(is.na(ums_count), 0, ums_count))

word.count <- vc.transcription %>%
  group_by(te_id) %>%
  dplyr::summarise(word_count = n())
chr.wise <- vc.transcription %>%
  pivot_longer(cols = c(lingmatch.characters, lingmatch.syllables, lingmatch.reading_grade), 
               names_to = "cat1") %>%
  group_by(te_id, cat1) %>%
  dplyr::summarise(avg = mean(value, na.omit = T)) %>%
  pivot_wider(names_from = "cat1", values_from = "avg", id_cols = "te_id")

word.wise <- vc.transcription %>%
  pivot_longer(cols = c(starts_with("nrc"), profanity_count, 
                        lingmatch.sixltr, lingmatch.ppron, lingmatch.ipron, lingmatch.adverb, 
                        lingmatch.conj, lingmatch.auxverb, lingmatch.prep, lingmatch.negate, lingmatch.quant), 
               names_to = "cat2") %>%
  group_by(te_id, cat2) %>%
  dplyr::summarise(count = sum(value)) %>%
  left_join(word.count) %>%
  mutate(cat_ratio = count / word_count) %>%
  pivot_wider(names_from = "cat2", values_from = "cat_ratio", id_cols = "te_id")


m124 <- inner_join(m1.m2, 
                   inner_join(inner_join(chr.wise, word.wise),
                              inner_join(word.count, ums)))
corr.table(m124 %>% select(any_of(c(colnames(m1), colnames(m2))),
                           -ends_with("id")),
           m124 %>% select(colnames(chr.wise), 
                           colnames(word.wise), 
                           ums_count, word_count,
                           -te_id),
           method = "spearman") %>%
  filter(V1 %in% c(colnames(m1), colnames(m2)), 
         V2 %in% c("ums_count", "word_count",colnames(chr.wise), colnames(word.wise))) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1)) %>%
  mutate(V1 = factor(V1, levels = unique(V1)),
         V2 = factor(V2, levels = unique(V2))) %>%
  mutate(cat1 = ifelse(grepl(paste(c("characters", 
                                    "reading_grade",
                                    "syllables"), collapse = "|"), V2), "average", 
                      ifelse(V2 %in% c("ums_count", "word_count"), "total", "ratio"))) %>%
  mutate(cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ")) %>%
  mutate(V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1)),
         V2 = sub("lingmatch\\.", "", V2),
         V2 = sub("nrc\\.", "", V2),
         V2 = factor(V2, levels = unique(V2))) %>%
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(pval<0.05, "*","")))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(rows = vars(cat1), 
                     cols = vars(cat2),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "language metrics from recorded PS-VC",
       caption = paste0("n(samples): ", nrow(m124))) +
  my.guides
ggsave(filename = paste0("figs/corr_iq-nih-PS-VC-language-features.png"),
       width = 6, height = 6, units = "in", dpi = 320, bg = "white")

################################################################################

################################################################################


