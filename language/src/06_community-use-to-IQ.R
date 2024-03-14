################################################################################
#            checking most&first used comm predict IQ and NIH-TB scores        #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/language"
setwd(project.dir)
################################################################################
################################################################################
# load files here
m1.m2 <- read_rds("data/derivatives/m1m2.rds") 
demo <- read_rds("data/raw/demo.rds")
ps.vc <- read_rds("data/derivatives/ps-vc-text-clean.rds")
#####
comm.meta <- read_rds("data/derivatives/community-data.rds") %>% as.data.frame()
ps.vc <- read_rds("data/derivatives/ps-vc-text-analyzed.rds")
#############
################################################################################
################################################################################
comm.meta %>%
  left_join(m1.m2) %>%
    ggplot(aes(x=archetype, y = VCI_composite_score, fill = archetype)) +
  geom_violin()+
  geom_boxplot(width=0.2, fill="white")+
  ggpubr::stat_compare_means()+
  scale_fill_manual(values = ten.colors)
m123 <- left_join(comm.meta, m1.m2)
library(ggstatsplot)
ggbetweenstats(data = comm.meta %>%
                 left_join(m1.m2), 
               x = archetype,
               y = VCI_composite_score)

lapply(m123 %>% select(colnames(m1.m2), -ends_with("_id")), function(x) {
  ggbetweenstats(data = m123 %>%
                   mutate(y=as.numeric(x))%>%
                   group_by(te_id, word) %>%
                   slice_min(order_by = visit_start_time) %>%
                   ungroup(), 
                 x = archetype,
                 y = y, 
                 title = "archetype of first word used in response to prompt")
})
comm.meta %>%
  left_join(m1.m2) %>%
  group_by(te_id, word) %>%
  slice_min(order_by = visit_start_time) %>%
  ungroup() %>%
  pivot_longer(cols = c(colnames(m1.m2), -ends_with("_id")), values_to = "val", names_to = "measure") %>%
  ggplot(aes(x=archetype, y = val))+
  geom_point()
################################################################################
colnames(ps.vc)
first.r <- ps.vc %>%
  group_by(te_id, word) %>%
  slice_min(order_by = start) %>%
  ungroup()
################################################################################



################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
