################################################################################
#                   checking correlation between PS_VC and other tests         #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = lubridate::interval(dob, age.day) / lubridate::duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
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
  select(task_num, word, start_in_sec, end_in_sec) %>%
  rownames_to_column("task_order") %>% mutate(task_order = as.numeric(task_order))
ids <- readxl::read_xlsx("data/raw/RPOE_participants_metadata.xlsx", sheet = 1) %>%
  drop_na(devGenes_id) %>%
  mutate(age = age(dob = DOB))
# keep participants of interest
participants.metadata <- readxl::read_xlsx("data/raw/RPOE_meta.xlsx", sheet = 1)
p.of.int <- participants.metadata %>% 
  filter(work_on_2 != "F")
################################################################################
# read tests data
nih.tb <- read_csv("data/derivatives/nih-tb_clean.csv")
iq <- read_csv("data/derivatives/wisc-and-wais_clean.csv")
################################################################################
# check correlation between NIH-TB and IQ
m1 <- nih.tb %>%
  select(te_id, dev_id, 
         ends_with("age_corrected_standard_score")) %>%
  drop_na()
m2 <- iq %>%
  select(dev_id, te_id,
         paste0(c("PSI", "WM", "VCI"), "_composite_score"),
         # ends_with("composite_score"),
         FSIQ, 
         SI, VC, BD, VP, MR, 
         #FW, PS, IN, AR, 
         DS, CD, SS) %>%
  mutate(VCI_PSI = VCI_composite_score - PSI_composite_score,
         abs_VCI_PSI = abs(VCI_composite_score - PSI_composite_score))
m1.m2 <- inner_join(m1, m2) %>%
  mutate(te_id=ifelse(is.na(te_id), dev_id, te_id)) %>%
  rename(te_id = te_id)
rm(iq);rm(nih.tb);gc();rm(m1);rm(m2)
################################################################################
# correlation between PSVC acoustic features and iq/nih
files.meta <- data.frame(file = list.files("data/derivatives/surfboard_out/surfboard_in/")) %>%
  mutate(id = sub("_task.*", "", file)) %>%
  mutate(task_num1 = sub(".*task-2_", "", sub(".wav", "", file))) %>%
  mutate(task_num2 = sub(".*_", "", task_num1),
         word = sub("_.*", "", task_num1)) %>%
  mutate(devGenes_id = ifelse(grepl("2E_", id), NA,id)) %>%
  left_join(ids[-1,] %>% select(devGenes_id, te_id)) %>%
  mutate(te_id = ifelse(grepl("2E_", id), id,te_id)) %>%
  select(-devGenes_id) %>%
  left_join(ids)

surfboard <- cbind(files.meta,
                   read_csv("data/derivatives/surfboard_out/ALL_task-2.csv"))
m123 <- right_join(m1.m2,surfboard)

library(lme4)
registerDoMC(cor3)
m.summ <- foreach(i=3:ncol(m1.m2), .combine = rbind) %dopar% {
  iq.var <- colnames(m1.m2)[i]
  sb <- surfboard %>% 
    select(any_of(colnames(surfboard)[-c(1:5)]) & 
             (contains("mean") | contains("Jitter") | 
                contains("Shimmer") | contains("hnr") | 
                contains("dfa")),-contains("derivative"))
  foreach (j = 1:ncol(sb), .combine = rbind) %dopar% {
    # j=1
    sb.var <- colnames(sb)[j]
    y = m123[,which(colnames(m123)==sb.var)]%>%rename(y=1)
    x = m123[,which(colnames(m123)==iq.var)]%>%rename(x=1)
    t <- summary(lmer(y ~ x + age + sex + age*sex + (1|task_num1) + (1|te_id),
                      data = cbind(m123,x,y)))
    # t$coefficients
    data.frame(y = sb.var,
               x = iq.var,
               Estimate = t$coefficients["x","Estimate"],
               se = t$coefficients["x","Std. Error"],
               confint.min = t$coefficients["x","Estimate"]-t$coefficients["x","Std. Error"],
               confint.max = t$coefficients["x","Estimate"]+t$coefficients["x","Std. Error"])
  }
}
# plot the models summary
m.summ %>%
  mutate(x = sub("_age_corrected_standard_score", "_NIH", x)) %>%
  mutate(cat2 = ifelse(grepl("NIH", x), "NIH-TB", "IQ")) %>%
  mutate(x = sub("_NIH", "", x)) %>%
  ggplot(aes(y=y, x =Estimate)) +
  geom_point(position = position_dodge(width = 0.6), size =1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.2, color = "black") +
  # scale_alpha_manual(values = c(1, 0.5)) +
  # scale_shape_manual(values = c(1, 2)) + 
  geom_errorbarh(aes(xmin = confint.min, xmax = confint.max), 
                 size = 0.8, height = 0, show.legend = F, 
                 position = position_dodge(width = 0.6)) +
  scale_color_manual(values = six.colors[1:4])  +
  ggh4x::facet_grid2(cols = vars(reorder(x, desc(cat2)))) +
  labs(y="",
       caption = paste0("the estimates are derived from this lm formula:", "\n",
                        "   lmer(y ~ x + age + sex + age*sex + (1|task_num) + (1|te_id))", "\n",
                        "   where y is a variable from surfboard output, and x is one of the IQ/NIH-TB measures"))
ggsave(filename = "figs/corr_iq-nih-PS-VC-acoustics-lmer.png", bg = "white",
       width = 23, height = 9, units = "in", dpi = 360)
###


# aa <- list()
# for (i in c(1:4)) {
#   task <- unique(surfboard$task_num2)[i]
#   task_2 <- paste0(m123 %>% filter(task_num2==task) %>% distinct(word), "_", task)
#   mm <- m123 %>%
#     filter(task_num2==task)
#   p <- corr.table(mm %>%
#                     select(any_of(c(colnames(m1), colnames(m2))),
#                            -ends_with("id")),
#                   mm %>% 
#                     select(any_of(colnames(surfboard)[-c(1:5)]) & 
#                              (contains("mean") | contains("Jitter") | 
#                                 contains("Shimmer") | contains("hnr") | 
#                                 contains("dfa")),-contains("derivative"))) %>%
#     mutate(FDR = p.adjust(pval, method = "fdr")) %>%
#     filter(V1 %in% c(colnames(m1), colnames(m2)), V2 %in% colnames(surfboard)[-c(1:5)]) %>%
#     mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
#            cat1 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
#            V1 = factor(V1, levels = unique(V1)),
#            V2 = factor(V2, levels = unique(V2)),
#            V1 = sub("_NIH", "", V1),
#            V1 = factor(V1, levels = unique(V1)),
#            V2 = factor(V2, levels = unique(V2))) %>%
#     ggplot(aes(x=V1, y=V2, fill = r, label =ifelse(FDR<0.05, "**", ifelse(pval<0.05, "*",""))))+
#     geom_tile()+
#     geom_text(size = 3, color = "white")+
#     scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
#     ggh4x::facet_grid2(cols = vars(cat1), scales = "free", space = "free") +
#     labs(x = "IQ & NIH-TB", y = "surfboard",
#          title = task_2,
#          caption = paste0("n(samples): ", nrow(mm))) +
#     my.guides
#   if (i!=1) {
#     p <- p+ theme(axis.text.y.left = element_blank())+labs(y="", caption = "")
#   }
#   aa[[i]] <- p
# }
# p.a <- patchwork::wrap_plots(aa, ncol = 4)
# ggsave(p.a, filename = "figs/corr_iq-nih-PS-VC-acoustics.png",
#        width = 14, height = 6, units = "in", dpi = 320, bg = "white")
################################################################################