################################################################################
#                         extract facial landmarks and features                #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/photographs"
setwd(project.dir)
################################################################################
# keep participants of interest
participants.metadata <- readxl::read_xlsx("../language/data/raw/RPOE_participants_metadata.xlsx", sheet = 1) %>%
  drop_na(sex, te_id)
# # check if the participants have face photographs taken
# registerDoMC(cores = 3)
# files.meta <- foreach(i=1:nrow(participants.metadata), .combine = rbind) %dopar% {
#   p <- participants.metadata$te_id[i]
#   df <- data.frame(file = list.files(paste0("/Dedicated/jmichaelson-sdata/MRI/RPOE/", p, "/phenotype/photographs"),
#                                      pattern = "face", full.names = T)) %>%
#     mutate(te_id = p,
#            new_path = paste0(project.dir,
#                              "/data/raw/face_", te_id, ".jpg"))
#   if (nrow(df)>0) {
#     cmd <- paste0("cp ", df$file[1], " ", df$new_path)
#     system(cmd)
#   }
#   return(df)
# }
################################################################################
# run the python script to process these files
py.sc <- "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/customized-functions/facial_coords.py"
cmd <- paste("python3", py.sc,
             "--dir_path", "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/photographs/data/raw",
             sep = " ")
system(cmd)

# read the landmarks csv, and save them with matched IDs
system(paste0("mv raw_coords.csv ", "data/derivatives/landmarks.csv"))

######
# if doing it for only one participant:
py.sc <- "/Dedicated/jmichaelson-wdata/msmuhammad/workbench/customized-functions/facial_coords.py"
iid <- "2E_094"
cmd <- paste("python3", py.sc,
             "--image_path", paste0("/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/photographs/data/raw/face_",
                                    iid,".jpg"),
             sep = " ")
system(cmd)
o.landmarks <- read_csv("data/derivatives/landmarks.csv")
n.landmarks <- read_csv(paste0("face_", iid, "_coords.csv"))
a.landmarks <- rbind(o.landmarks %>%
                       filter(!(filename %in% n.landmarks$filename)), 
                     n.landmarks) %>%
  arrange(filename)
write_csv(a.landmarks, "data/derivatives/landmarks.csv")
system(paste0("rm ", "face_", iid, "_coords.csv"))
######
######

landmarks <- cbind(read_csv("data/derivatives/landmarks.csv")) %>%
  mutate(te_id = sub("face_", "", filename),
         te_id = sub("\\.jpg", "", te_id)) %>%
  select(-QCPASS)
colnames(landmarks)[c(2,3)] <- c("NT_x", "NT_y")
################################################################################
# print the landmarks for one of the faces, to identify the selected 12 coordinates
tmp <- landmarks[9,] %>%
  pivot_longer(cols = c(contains("x"), contains("y")), names_to = "coord") %>%
  mutate(coord2 = ifelse(grepl("x", coord), "x", "y"),
         coord3 = sub("x", "", coord),
         coord3 = sub("y", "", coord3)) %>%
  pivot_wider(names_from = "coord2", values_from = "value", id_cols = c("te_id", "coord3")) %>%
  mutate(coord3 = sub("rel__", "P_", coord3))

tmp %>%
  ggplot(aes(x=x, y=y, label = coord3)) +
  geom_point(color = "red") +
  geom_text(size =3)
################################################################################
# list of points of interest
# 17,21,22,26,36,39,27,42,45,"NT",48,54
# 
keep <- c(17,21,22,26,36,39,27,42,45,"NT",48,54,31,35,51,57,0,16)
main.distances <- data.frame(from = c(17, 22, 36, 42, 48, 27, 31,51,21,21,22,17,26,39,42,17,26,36,45,0),
                             to = c(21, 26, 39, 45, 54, "NT", 35,57,22,27,27,36,45,"NT","NT",48,54,48,54,16),
                             label = c("EB_R", "EB_L", "E_R", "E_L", "M_H", "N_V", "N_H_B", "M_V", "EB_C","EB_N_R", "EB_N_L", "EB_E_R", "EB_E_L","NT_E_R", "NT_E_L","EB_M_R", "EB_M_L","E_M_R", "E_M_L", "N_H_A"))
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
    mutate(point = parse_number(coord3),
           point = ifelse(grepl("NT", coord3), "NT", point))
  p.nt.x <- p.landmarks %>% filter(coord3 == "NT_") %>% select(x) %>% as.numeric()
  p.nt.y <- p.landmarks %>% filter(coord3 == "NT_") %>% select(y) %>% as.numeric()
  p.landmarks <- p.landmarks %>% 
    mutate(x=x+p.nt.x, y=p.nt.y+y)%>%
    mutate(x = ifelse(coord3 == "NT_", p.nt.x, x),
           y = ifelse(coord3 == "NT_", p.nt.y, y)) %>%
    mutate(x = scales::rescale(x),
           y = scales::rescale(y))
  
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
              rename(X1 = "from", X2 = "to") %>%
              mutate(X1 = as.character(X1))) %>%
  mutate(pair = ifelse(is.na(label), pair, label)) %>%
  filter(!is.na(label)) %>%
  ggplot(aes(x=distance)) +
  geom_histogram()+
  facet_wrap("pair", scales = "free")
ggsave(p, filename = "figs/distribution-of-landmarks-pairs-distances.png", bg = "white",
       height = 8, width = 9, units = "in", dpi = 360)
#
################################################################################
wider.distances <- distances %>%
  left_join(main.distances %>%
              rename(X1 = "from", X2 = "to") %>%
              mutate(X1 = as.character(X1))) %>%
  mutate(pair = ifelse(is.na(label), pair, label),
         pair = paste0("P_", pair)) %>%
  pivot_wider(names_from = "pair", values_from = "distance", id_cols = "te_id")
write_csv(wider.distances, "data/derivatives/pairs-distances.csv")

################################################################################
# try to plot coords on top of image
# 
# 
# 
library(magick)
pdf(file = "figs/layered-landmarks.pdf")
for (j in 1:nrow(landmarks)) {
  tmp2 <- landmarks[j,] %>%
    pivot_longer(cols = c(contains("x"), contains("y")), names_to = "coord") %>%
    mutate(coord2 = ifelse(grepl("x", coord), "x", "y"),
           coord3 = sub("x", "", coord),
           coord3 = sub("y", "", coord3)) %>%
    pivot_wider(names_from = "coord2", values_from = "value", id_cols = c("te_id", "coord3")) %>%
    mutate(coord3 = sub("rel__", "P_", coord3)) 
  tmp.nt.x <- tmp2 %>% filter(coord3 == "NT_") %>% select(x) %>% as.numeric()
  tmp.nt.y <- tmp2 %>% filter(coord3 == "NT_") %>% select(y) %>% as.numeric()
  tmp2 <- tmp2 %>% mutate(x=x+tmp.nt.x, y=ifelse(y>0,tmp.nt.y+y,tmp.nt.y+y))%>%
    mutate(x = ifelse(coord3 == "NT_", tmp.nt.x, x),
           y = ifelse(coord3 == "NT_", tmp.nt.y, y)) 
  img2 <- imager::load.image(paste0("data/raw/",landmarks$filename[j]))
  plot(img2) + points(x = tmp2$x, y = tmp2$y) + title(main = landmarks$te_id[j])
}
dev.off()




################################################################################
################################################################################
################################################################################
# get the areas in the face
a1 <- c(27,22,23,24,25,26,45,44,43,42,27) # ES_R
a2 <- c(42,43,44,45,46,47,42) # E_R
a3 <- c(27,16,45,46,47,42,27) # CHK_I_R
a4 <- c(27,28,29,30,33,34,35,16,27) # N_R
a5 <- c(26,45,16,35,34,33,51,52,53,54,26) # CHK_O_R
a6 <- c(62,63,64,65,66,57,56,55,54,53,52,51,62) # M_R
a7 <- c(27,21,20,19,18,17,36,37,38,39,27) # ES_L
a8 <- c(36,37,38,39,40,41,36) # E_L
a9 <- c(27,0,36,41,40,39,27) # CHK_I_L
a10 <- c(27,28,29,30,33,32,31,0,27) # N_L
a11 <- c(17,36,0,31,32,33,51,50,49,48,17) # CHK_O_L
a12 <- c(62,61,60,67,66,57,58,59,48,49,50,51,62) # M_L


facial.areas <- foreach(j = 1:nrow(landmarks), .combine = rbind) %dopar% {
  #
  p.landmarks <- landmarks[j,] %>%
    pivot_longer(cols = c(contains("x"), contains("y")), names_to = "coord") %>%
    filter(!grepl("nose", coord)) %>%
    mutate(coord2 = ifelse(grepl("x", coord), "x", "y"),
           coord3 = sub("x", "", coord),
           coord3 = sub("y", "", coord3)) %>%
    pivot_wider(names_from = "coord2", values_from = "value", id_cols = c("te_id", "coord3")) %>%
    mutate(coord3 = sub("rel__", "P_", coord3)) %>%
    mutate(point = parse_number(coord3),
           point = ifelse(grepl("NT", coord3), "NT", point))
  p.nt.x <- p.landmarks %>% filter(coord3 == "NT_") %>% select(x) %>% as.numeric()
  p.nt.y <- p.landmarks %>% filter(coord3 == "NT_") %>% select(y) %>% as.numeric()
  p.landmarks <- p.landmarks %>% 
    mutate(x=x+p.nt.x, y=p.nt.y+y)%>%
    mutate(x = ifelse(coord3 == "NT_", p.nt.x, x),
           y = ifelse(coord3 == "NT_", p.nt.y, y)) %>%
    mutate(x = scales::rescale(x),
           y = scales::rescale(y))
  #
  coords.all <- p.landmarks %>%
    filter(point != "NT") %>%
    mutate(point = as.numeric(point)) %>%
    column_to_rownames("point")
  library(geometry)
  a.a1 <- polyarea(x = coords.all$x[a1+1], y = coords.all$y[a1+1])
  a.a2 <- polyarea(x = coords.all$x[a2+1], y = coords.all$y[a2+1])
  a.a3 <- polyarea(x = coords.all$x[a3+1], y = coords.all$y[a3+1])
  a.a4 <- polyarea(x = coords.all$x[a4+1], y = coords.all$y[a4+1])
  a.a5 <- polyarea(x = coords.all$x[a5+1], y = coords.all$y[a5+1])
  a.a6 <- polyarea(x = coords.all$x[a6+1], y = coords.all$y[a6+1])
  a.a7 <- polyarea(x = coords.all$x[a7+1], y = coords.all$y[a7+1])
  a.a8 <- polyarea(x = coords.all$x[a8+1], y = coords.all$y[a8+1])
  a.a9 <- polyarea(x = coords.all$x[a9+1], y = coords.all$y[a9+1])
  a.a10 <- polyarea(x = coords.all$x[a10+1], y = coords.all$y[a10+1])
  a.a11 <- polyarea(x = coords.all$x[a11+1], y = coords.all$y[a11+1])
  a.a12 <- polyarea(x = coords.all$x[a12+1], y = coords.all$y[a12+1])
  data.frame(area_label = c("ES_R", "E_R", "CHK_I_R", "N_R", "CHK_O_R", "M_R",
                            "ES_L", "E_L", "CHK_I_L", "N_L", "CHK_O_L", "M_L"),
             area = c(a.a1, a.a2, a.a3, a.a4, a.a5, a.a6, a.a7, a.a8, a.a9, a.a10, a.a11, a.a12)) %>%
    mutate(te_id = sub("face_", "", landmarks$te_id[j]),
           te_id = sub("\\.jpg", "", te_id))
}
# plot distribution of areas
p <- facial.areas %>%
  ggplot(aes(x=area)) +
  geom_histogram()+
  facet_wrap("area_label", scales = "free")
ggsave(p, filename = "figs/distribution-of-landmarks-areas.png", bg = "white",
       height = 8, width = 9, units = "in", dpi = 360)
#
################################################################################
wider.facial.areas <- facial.areas %>%
  mutate(area_label = paste0("A_", area_label)) %>%
  pivot_wider(names_from = "area_label", values_from = "area", id_cols = "te_id")
write_csv(wider.facial.areas, "data/derivatives/facial.areas.csv")
################################################################################


################################################################################
