################################################################################
#                         extract facial landmarks and features                #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/photographs/"
setwd(project.dir)
################################################################################
# keep participants of interest
participants.metadata <- readxl::read_xlsx("../language/data/raw/RPOE_participants_metadata.xlsx", sheet = 1) %>%
  drop_na(sex, te_id)
# check if the participants have face photographs taken
registerDoMC(cores = 3)
files.meta <- foreach(i=1:nrow(participants.metadata), .combine = rbind) %dopar% {
  p <- participants.metadata$te_id[i]
  df <- data.frame(file = list.files(paste0("/Dedicated/jmichaelson-sdata/MRI/RPOE/", p, "/phenotype/photographs"),
                                     pattern = "face", full.names = T)) %>%
    mutate(te_id = p,
           new_path = paste0(project.dir,
                             "/data/raw/face_", te_id, ".jpg"))
  if (nrow(df)>0) {
    cmd <- paste0("cp ", df$file[1], " ", df$new_path)
    system(cmd)
  }
  return(df)
}
################################################################################
# run the python script to process these files
py.sc <- "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/customized-functions/facial_coords.py"
cmd <- paste("python3", py.sc,
             "--folder_path", "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/photographs/data/raw",
             sep = " ")
system(cmd)

# read the landmarks csv, and save them with matched IDs
system(paste0("mv landmarks.csv ", "data/derivatives/"))
landmarks <- cbind(te_id = list.files("data/raw/", pattern = "face"),
                   read_csv("data/derivatives/landmarks.csv"))
################################################################################
# print the landmarks for one of the faces, to identify the selected 12 coordinates
tmp <- landmarks[2,] %>%
  pivot_longer(cols = c(contains("x"), contains("y")), names_to = "coord") %>%
  mutate(coord2 = ifelse(grepl("x", coord), "x", "y"),
         coord3 = sub("x", "", coord),
         coord3 = sub("y", "", coord3)) %>%
  pivot_wider(names_from = "coord2", values_from = "value", id_cols = c("te_id", "coord3")) %>%
  mutate(coord3 = sub("rel__", "P_", coord3))

tmp %>%
  filter(!grepl("nose", coord3)) %>%
  ggplot(aes(x=x, y=y, label = coord3)) +
  geom_point(color = "red") +
  geom_text(size =3)
################################################################################
# list of points of interest
# 17,21,22,26,36,39,27,42,45,30,48,54
# 
keep <- c(17,21,22,26,36,39,27,42,45,30,48,54)
main.distances <- data.frame(from = c(17, 22, 36, 42, 48, 27),
                             to = c(21, 26, 39, 45, 54, 30),
                             label = c("EB_L", "EB_R", "E_L", "E_R", "M", "N"))
all.distances <- data.frame(t(combn(keep, 2))) %>%
  rownames_to_column("pair") 
registerDoMC(cores = 2)
distances <- foreach(j = 1:nrow(landmarks), .combine = rbind) %dopar% {
  p.landmarks <- landmarks[j,] %>%
    pivot_longer(cols = c(contains("x"), contains("y")), names_to = "coord") %>%
    filter(!grepl("nose", coord)) %>%
    mutate(coord2 = ifelse(grepl("x", coord), "x", "y"),
           coord3 = sub("x", "", coord),
           coord3 = sub("y", "", coord3)) %>%
    pivot_wider(names_from = "coord2", values_from = "value", id_cols = c("te_id", "coord3")) %>%
    mutate(coord3 = sub("rel__", "P_", coord3)) %>%
    mutate(point = parse_number(coord3))
  
  d_m <- foreach(i = 1:nrow(all.distances), .combine = rbind) %dopar% {
    p1 <- all.distances$X1[i]
    p2 <- all.distances$X2[i]
    p1x <- p.landmarks %>% filter(point == p1) %>% select(x) %>% as.numeric()
    p1y <- p.landmarks %>% filter(point == p1) %>% select(y) %>% as.numeric()
    p2x <- p.landmarks %>% filter(point == p2) %>% select(x) %>% as.numeric()
    p2y <- p.landmarks %>% filter(point == p2) %>% select(y) %>% as.numeric()
    d <- sqrt(((p2x-p1x)^2) + ((p2y-p1y)^2))
    data.frame(pair = all.distances$pair[i],
               X1 = p1,
               X2 = p2,
               distance = d)
  }
  d_m %>%
    mutate(te_id = sub("face_", "", landmarks$te_id[j]),
           te_id = sub("\\.jpg", "", te_id))
}
# plot distribution of distances for each pair
p <- distances %>%
  left_join(main.distances %>%
              rename(X1 = "from", X2 = "to")) %>%
  mutate(pair = ifelse(is.na(label), pair, label)) %>%
  ggplot(aes(x=distance)) +
  geom_histogram()+
  facet_wrap("pair", scales = "free")
ggsave(p, filename = "figs/distribution-of-landmarks-pairs-distances.png", bg = "white",
       height = 8, width = 9, units = "in", dpi = 360)
#
################################################################################
wider.distances <- distances %>%
  left_join(main.distances %>%
              rename(X1 = "from", X2 = "to")) %>%
  mutate(pair = ifelse(is.na(label), pair, label),
         pair = paste0("P_", pair)) %>%
  pivot_wider(names_from = "pair", values_from = "distance", id_cols = "te_id")
write_csv(wider.distances, "data/derivatives/pairs-distances.csv")

################################################################################


################################################################################


################################################################################


################################################################################