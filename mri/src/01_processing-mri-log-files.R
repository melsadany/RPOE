################################################################################
#                               processing fMRI log data                       #
################################################################################
rm(list = ls())
gc()
source("/Dedicated/jmichaelson-wdata/msmuhammad/msmuhammad-source.R")
################################################################################
################################################################################
project.dir <- "/Dedicated/jmichaelson-wdata/msmuhammad/projects/RPOE/mri"
setwd(project.dir)
################################################################################
# read fMRI tabulated data? 
# using Jake's function to process the file here
format_2e_fmri_metadata = function(fn,nframes=1000){ 
  dat = read.table(fn,sep="\t",header=F,stringsAsFactors=F) 
  d1 = dat[grepl("EXP",dat[,2]),] 
  d1 = d1[grepl("New trial",d1[,3]),] 
  mdata = t(sapply(strsplit(d1[[3]],split="\\, |\\: "),'[',c(4,6,8,10,12,14))) 
  mdata = data.frame(abs_time=d1[[1]], 
                     rel_time=d1[[1]]-d1[[1]][1], 
                     end_time=d1[[1]]-d1[[1]][1]+as.numeric(mdata[,6]), 
                     task=mdata[,2], 
                     exp_condition=mdata[,3], 
                     category=mdata[,4], 
                     specific=mdata[,5], 
                     duration=as.numeric(mdata[,6]), 
                     file=mdata[,1],stringsAsFactors=F) 
  ipt = dat[grepl("DATA",dat[,2]),] 
  ipt = ipt[!grepl("Keypress: 5",ipt[,3],fixed=T),] #what's keypress 5?
  o = outer(ipt[,1],mdata$abs_time,'-') 
  o[o<0] = Inf  
  kp = apply(o,2,which.min) 
  mdata$keypress = ipt[kp,3] 
  mdata$keypress_rel_time = ipt[kp,1] - mdata$abs_time[1] 
  mdata$keypress[mdata$task%in%c("neutral","instructions") | mdata$keypress_rel_time>mdata$end_time] = NA 
  mdata$keypress_rel_time[mdata$task%in%c("neutral","instructions") | mdata$keypress_rel_time>mdata$end_time] = NA 
  rt = mdata$keypress_rel_time - mdata$rel_time # is that how long it takes to press per task?
  # boxplot(tapply(rt,mdata$category,function(x) x),las=2) 
  # table(mdata$category) 
  #nframes = dim(m)[4] #I assumed it's dim[4] of fMRI
  tr = 1.0 
  tt = seq(0,tr*nframes,length.out=nframes) 
  ii = sapply(tt,function(x) rev(which(x-mdata$rel_time>=0))[1]) 
  fdat = data.frame(time=tt,mdata[ii,],stringsAsFactors=F) 
  expr = rep("instructions",nrow(fdat)) 
  expr[fdat$prompt%in%"baseline"] = "baseline" 
  expr[fdat$prompt%in%c("coherent","incoherent")] = "verbal" 
  expr[fdat$prompt%in%c("same","diff")] = "PS" 
  fdat$experiment=as.factor(expr) 
  prompt = sapply(unique(fdat$category),function(x) fdat$category%in%x) 
  
  # Muhammad's section
  # tried to extract how many presses were done per word in word association task
  first <- mdata[171:209,]
  first <- first[order(first$abs_time), ]
  # Function to find the max abs_time from first dataframe before the given abs_time in second dataframe
  find_max_time <- function(abs_time) {
    max_time <- max(first$abs_time[first$abs_time < abs_time])
    ifelse(is.na(max_time), NA, max_time)
  }
  counts <- ipt %>%
    rename(abs_time = V1, keypress=V3) %>%
    filter(abs_time > min(first$abs_time))
  # Add task_b4_start column to the counts dataframe using sapply/mapply
  counts$task_b4_start <- sapply(counts$abs_time, find_max_time)
  tmp <- inner_join(counts %>% select(abs_time=task_b4_start), mdata %>% select(-keypress))  %>%
    group_by(task,exp_condition, category, specific,abs_time) %>%
    dplyr::summarise(count=n())
  # View(left_join(mdata,tmp))
  mdata <- left_join(mdata,tmp)
  return(list(mdata = mdata, fdat = fdat, prompt = prompt)) 
} 
# # EXAMPLE USE  
# fn = "/sdata/MRI/RPOE/2E_045/scan/metadata/2E_045_semanticMap_2023-07-26_11h05.52.863.log" 
# l = format_2e_fmri_metadata(fn, nframes = 1000) 
# questions for Jake
# what's keypress 7,8,4, and 5
# where's the multiple keypresses for the word association task
################################################################################
################################################################################
################################################################################
# apply the function on users that have done the fMRI-PSVC
system(paste0("mkdir -p ", project.dir, "/data/derivatives/MRI-log/mdata"))
p.list <- list.dirs("/Dedicated/jmichaelson-sdata/MRI/RPOE", recursive = F)
registerDoMC(cores = 4)
foreach (i = 1:(length(p.list)-1)) %dopar% {
  f.path <- p.list[i]
  pid <- sub("/Dedicated/jmichaelson-sdata/MRI/RPOE/", "", f.path)
  file <- list.files(paste0(f.path, "/scan/metadata"), 
                     pattern = "log", full.names = T)
  if (length(file)!=0) {
    out <- format_2e_fmri_metadata(fn = file)
    write_rds(out, paste0(project.dir, "/data/derivatives/MRI-log/", pid, "_fmri-all.rds"))
    write_tsv(out[["mdata"]], paste0(project.dir, "/data/derivatives/MRI-log/mdata/", pid, "_mdata.tsv"))
  }
}
################################################################################

################################################################################


################################################################################




