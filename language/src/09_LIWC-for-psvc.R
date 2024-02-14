################################################################################
#                   checking correlation between PS_VC and other tests         #
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
################################################################################
################################################################################
# read LIWC results
liwc <- read_csv("data/derivatives/LIWC/psvc-transcriptions-LIWC.csv")
wc <- liwc %>%
  select(te_id, WC) %>%
  group_by(te_id) %>%
  dplyr::summarise(WC=sum(WC))
liwc.2 <- liwc %>%
  select(te_id, 13:109) %>%
  group_by(te_id) %>%
  dplyr::summarise_at(.vars = colnames(liwc)[13:109], .funs = function(x) mean(x, na.rm=T)) %>%
  left_join(wc)
liwc.meta <- read_csv("/wdata/msmuhammad/data/LIWC/liwc-categories.csv")
######
# extract highest and lowest values per measure
######
ref <- data.frame(measure = colnames(liwc.2)[-c(1, 99)],
                  colname = c(13:109),
                  highest = NA,lowest = NA)
for (i in 1:nrow(ref)) {
  f <- liwc[,c("text", ref$measure[i])]
  ff <- arrange(f, desc(f[,2]))
  ref$highest[i] <- as.character((ff %>% filter_at(.vars = 2, function(x) x==max(ff[,2])) %>% select(1) %>% unnest())[1:min(5, sum(ff[,2]==max(ff[,2]))),1])
  ref$lowest[i] <- as.character((ff %>% filter_at(.vars = 2, function(x) x==min(ff[,2])) %>% select(1) %>% unnest())[1:min(5, sum(ff[,2]==min(ff[,2]))),1])
}
ref <- ref %>% 
  left_join(liwc.meta %>% rename(measure=feature)) %>%
  mutate(category=ifelse(is.na(category), "summary", category)) %>%
  arrange(category, desc(measure))
table <- kableExtra::kable(ref, format="html") %>%
  kableExtra::kable_styling(full_width = T, protect_latex = T)
table
################################################################################
m123 <- inner_join(m1.m2, liwc.2)
corr.table(m123 %>% select(colnames(m1.m2), -ends_with("id")),
           m123 %>% select(any_of(colnames(liwc.2)), -te_id),
           method = "spearman") %>%
  mutate(FDR = p.adjust(pval, method = "fdr")) %>%
  filter(V1 %in% c(colnames(m1.m2)),
         V2 %in% colnames(liwc)) %>%
  mutate(V1 = sub("_age_corrected_standard_score", "_NIH", V1),
         V2 = factor(V2, levels = unique(V2)),
         cat2 = ifelse(grepl("NIH", V1), "NIH-TB", "IQ"),
         V1 = sub("_NIH", "", V1),
         V1 = factor(V1, levels = unique(V1))) %>%
  left_join(liwc.meta %>% rename(V2 = feature)) %>%
  mutate(category = ifelse(is.na(category), "summary", category)) %>%
  filter(!grepl("conversation", category), 
         !grepl("conversation", V2, ignore.case = T),
         !grepl("Dic", V2)) %>% # drop meaningless measures
  ggplot(aes(x=V1, y=V2, fill = r, label = ifelse(FDR < 0.05, "**", ifelse(pval<0.05, "*",""))))+
  geom_tile()+
  geom_text(size = 3, color = "white")+
  ggh4x::facet_grid2(cols = vars(cat2),
                     rows = vars(category),
                     scales = "free", space = "free") +
  scale_fill_gradient2(low = redblack.col[2], high = redblack.col[1]) +
  labs(x = "", y = "",
       caption = paste0("n(samples): ", nrow(m123), "\n",
                        "**   FDR<0.05", "\n",
                        "*    pval<0.05")) +
  my.guides +
  theme(strip.text.y.right = element_text(angle = 0))
ggsave(filename = "figs/corr_iq-nih-PS-VC-LIWC.png", bg = "white",
       width = 8, height = 16, units = "in", dpi = 360)
################################################################################
################################################################################
################################################################################
################################################################################