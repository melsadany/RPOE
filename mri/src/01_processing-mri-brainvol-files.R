################################################################################
#                    extract brainvol stats from freesurfer output             #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/mri"
setwd(project.dir)
################################################################################
################################################################################
# list participants
mri.meta <- data.frame(folder = list.files("/Dedicated/jmichaelson-sdata/MRI/RPOE/RPOE_MR/derivatives",
                                           recursive = F, full.names = T, pattern = "sub-")) %>%
  mutate(te_id = sub("sub-", "", basename(folder))) %>%
  filter(te_id != "2E_031_230908bak", # drop that bak folder
         te_id %in% paste0("2E_0", c(31, 34:100))) # drop older participants
dirs <- list.dirs(mri.meta$folder, full.names = F, recursive = F)
mri.meta <- mri.meta %>%
  mutate(dir2 = dirs[grepl("sub-", dirs)],
         file = paste0(folder,"/", dir2, "/stats/brainvol.stats"),
         processed = ifelse(file.exists(file), T, F)) %>%
  filter(processed == T)
################################################################################
# read files and extract data
registerDoMC(cores = 3)
stats.c <- foreach(i = 1:nrow(mri.meta), .combine = rbind) %dopar% {
  id <- mri.meta$te_id[i]
  t <- data.frame(lines = read_lines(mri.meta$file[i])) %>%
    mutate(measure = sub(",.*", "", lines),
           measure = sub("# Measure ", "", measure),
           value = sub("mm^3", "", lines),
           value = parse_number(value),
           te_id = id) %>%
    pivot_wider(names_from = measure, values_from = value, id_cols = te_id)
  return(t)
}
write_csv(stats.c, "data/derivatives/brainvol-stats.csv")
################################################################################

################################################################################

################################################################################

################################################################################

################################################################################

################################################################################

################################################################################

################################################################################


