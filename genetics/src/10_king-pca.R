################################################################################
#                         do PCA for your samples                              #
################################################################################
rm(list = ls()); gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
library(bigsnpr)
options(bigstatsr.check.parallel.blas = FALSE);options(default.nproc.blas = NULL)
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/genetics"
setwd(project.dir)
################################################################################
################################################################################
##################### Script from Lucas with minor changes #####################
################################################################################
################################################################################
#### file paths ####
plink2 <- download_plink2()
## path to merged samples + 1000 Genomes PLINK fileset
bedfile <- "data/derivatives/merged-glimpse/all-samples/in-hg19/final-merged-w-1KG_hapmap.bed"
#### get unrelated individuals ####
## KING is weird, all relatedness scores are cut in half. So, a monozygotic twin or a duplicate sample has a KING relatedness score of 0.5
## This command uses the default relatedness cutoff of 2nd degree relations
rel <- snp_plinkKINGQC(
  plink2.path = plink2,
  bedfile.in = bedfile,
  thr.king = 2^-3.5,
  make.bed = FALSE,
  ncores = 6
)
#### run PCA (it's actually SVD) ####
## get genotype object
(obj.bed <- bed(bedfile))
## related samples
rel %>%
  filter(KINSHIP > 2^-3.5) %>%
  as_tibble() 

ind.rel <- match(c(rel$IID1, rel$IID2), obj.bed$fam$sample.ID)  # /!\ use $ID1 instead with old PLINK
## unrelated samples
ind.norel <- rows_along(obj.bed)[-ind.rel]
obj.bed$fam$sample.ID[ind.norel]
## SVD, get first 20 dimensions
obj.svd <- bed_autoSVD(obj.bed, ind.row = ind.norel, k = 20,
                       ncores = 6, verbose = T)
#### identify outliers to remove from PC calculations ####
prob <- bigutilsr::prob_dist(obj.svd$u, ncores = 1)
S <- prob$dist.self / sqrt(prob$dist.nn)

ggplot() +
  geom_histogram(aes(S), color = "#000000", fill = "#000000", alpha = 0.5) +
  scale_x_continuous(breaks = 0:5 / 5, limits = c(0, NA)) +
  scale_y_sqrt(breaks = c(10, 100, 500)) +
  theme_bigstatsr() +
  labs(x = "Statistic of outlierness", y = "Frequency (sqrt-scale)")

#### use an outlier score of 0.8 as cutoff ####
## outlier IDs
obj.bed$fam$sample.ID[ind.norel[S > 0.8]]
length(ind.norel[S > 0.8])
# drop my cohort here and keep 1KG
to_drop <- c(ind.norel[S > 0.8], which(obj.bed$fam$sample.ID %in% paste0("S", 1:96, "_recal")))
ind.row <- ind.norel[-to_drop]
ind.col <- attr(obj.svd, "subset")

good_id <- data.frame(sample.ID = obj.bed$fam$sample.ID[ind.row]) %>% 
  as_tibble()

obj.bed$fam  %>%
  as_tibble() %>%
  select(family.ID, sample.ID) %>%
  filter(sample.ID %in% good_id$sample.ID) %>%
  write_tsv('data/derivatives/unrelated_qc_samples_for_pca.tsv')

#### redo PCA with high quality sample set ####
obj.svd2 <- bed_autoSVD(obj.bed, ind.row = ind.row,
                        ind.col = ind.col, thr.r2 = NA,
                        k = 20, ncores = 6)
####
plot(obj.svd2)
#### compute each individuals PC scores (the "bed_projectSelfPCA" takes a few minutes) ####
PCs <- matrix(NA, nrow(obj.bed), ncol(obj.svd2$u))
PCs[ind.row, ] <- predict(obj.svd2)
proj <- bed_projectSelfPCA(obj.svd2, obj.bed,
                           ind.row = rows_along(obj.bed)[-ind.row],
                           ncores = 6) # useless -> too few individuals
PCs[-ind.row, ] <- proj$OADP_proj
#### visualize, overlay the dropped samples on top of the rest to make sure they fit in this PCA correctly ####
plot(PCs[ind.row, 1:2], pch = 20, xlab = "PC1", ylab = "PC2")
points(PCs[-ind.row, 1:2], pch = 20, col = "blue")
#### sanity check ####
pc_dat <- PCs %>% 
  as_tibble() %>%
  mutate(sample.ID = obj.bed$fam$sample.ID) %>%
  relocate(sample.ID)
names(pc_dat) <- c('sample.ID', str_c('pc', 1:20))
## read in 1000 Genomes population labels
dm <- read_tsv('/Dedicated/jmichaelson-sdata/1KG/phase3/sample_info.txt') %>%
  rename(sample.ID = Sample) %>%
  select(-`Family ID`) %>%
  janitor::clean_names() %>%
  rename(sample.ID = sample_id)
##
tmp <- pc_dat %>%
  #  left_join(bind_rows(pop_list)) %>%
  left_join(dm) %>%
  # filter(!sample.ID %in% to_drop$sample.ID) %>%
  mutate(population = case_when(is.na(population) == T ~ 'NDVR',
                                TRUE ~ population),
         population_description = case_when(is.na(population_description) == T ~ 'NDVR',
                                            TRUE ~ population_description))
## find NDVR outliers to drop 
to_drop <- tmp %>%
  filter(population == 'NDVR') %>%
  filter(pc1 < 0 & pc2 > 0) %>%
  select(1)
## plot 
source('/Dedicated/jmichaelson-wdata/lcasten/functions/simple_ggplot_theme.R')
p <- tmp %>%
  filter(population_description != 'NDVR') %>%
  ggplot(aes(x = pc1, y = pc2, color = population_description)) +
  labs(color = NULL) +
  geom_point(size = 1) +
  geom_point(data = filter(tmp, population_description == 'NDVR'), aes(color = 'NDVR'), size = 2.5, shape = 'square') +
  guides(color=guide_legend(nrow=2,byrow=TRUE)) 
p
# dir.create('/home/lcasten/LSS/jmichaelson-wdata/lcasten/NDVR/genomics/figures')
p %>%
  ggsave(filename = 'figs/pca_plot.png', 
         device = 'png', units = 'in', width = 11, height = 10, dpi = 360)
## looks good - we get good population separation
#### write out PC data
tmp %>%
  # filter(! sample.ID %in% to_drop$sample.ID) %>%
  relocate(sample.ID, population, population_description, gender) %>%
  write_csv('data/derivatives/KING_pca_results.csv')
##