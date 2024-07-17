# Script to calculate all ttest results
#source("setup/help_functions.R")

# general ----
## packages
require(tidyverse)

## functions
source("~/Nextcloud/repository/humet/app/dev/src/processing_functions.R")
statistic_ttest<-function(data,id, baseline, tp){
  base_data = data %>% select(subject, timepoint, all_of(id)) %>% 
    filter(timepoint==baseline) %>% 
    select(-timepoint) %>% 
    setNames(c("subject","base_tp"))
  
  tp_data = data %>% select(subject, timepoint, all_of(id)) %>% 
    filter(timepoint==tp) %>% 
    select(-timepoint) %>% 
    setNames(c("subject","test_tp"))
  
  test_data=merge(base_data,tp_data, by="subject", all=T) %>% na.omit() %>% select(-subject)
  if(nrow(test_data)<=2){
    out=data.frame(pValue=NA,
                   foldchange=NA,
                   timepoint=tp,
                   baseline=baseline,
                   id=id)
  }else{
    model=try(t.test(test_data[,"test_tp"], test_data[,"base_tp"], paired = TRUE),silent = TRUE)           # do paired t.test
    if(inherits(model, "try-error")) {
      out=data.frame(pValue=NA,
                     foldchange=NA,
                     conf_int_low=NA,
                     conf_int_high=NA,
                     t=NA,
                     df = NA,
                     tp_measured=F,
                     timepoint=tp,
                     baseline=baseline,
                     id=id)
    }else{
      out=data.frame(pValue=model$p.value,
                     foldchange=mean(test_data[,"test_tp"]-test_data[,"base_tp"],na.rm = T),
                     conf_int_low = model$conf.int[1],
                     conf_int_high = model$conf.int[2],
                     t=model$statistic[1],
                     df = model$parameter,
                     tp_measured=T,
                     timepoint=tp,
                     baseline=baseline,
                     id=id)
    }
    
  }
  return(out)
}
## set and find working directory
wd = rstudioapi::getSourceEditorContext()$path %>% dirname()%>% dirname()
setwd(wd)

# load information needed to process
info_sample <- readRDS("~/Nextcloud/repository/humet/app/data/info/sample.rds")
info_met <- readRDS("~/Nextcloud/repository/humet/app/data/info/met.rds")

mapping <- data.frame(file = list.files("data/",recursive = T) %>% grep(pattern="df_",value=T)%>% grep(pattern="raw",value=T)) %>% 
  tidyr::separate(col="file",into=c("type","method","trans"),sep="_",remove=F) %>% 
  dplyr::mutate(trans = gsub(trans, pattern=".rds",replacement=""))

for(xx in 1:nrow(mapping)){
  my_data <- readRDS(file=paste0("data/",mapping$file[xx]))[1:840,]
  my_data$timepoint = my_data$timepoint %>% as.numeric()
  my_data$subject = my_data$subject %>% as.numeric()
  met_id=intersect(names(my_data), info_met$ID)
  
  # save combinations
  my_timepoints <- intersect(info_sample$timepoint, as.numeric(my_data$timepoint))
  tp_combinations=lapply(my_timepoints,function(yy)
    my_timepoints[which(my_timepoints>yy)]
    ) %>% setNames(my_timepoints)
  
  for(yy in seq_along(tp_combinations)){
    file_name <- paste0(mapping$method[xx],"_",ifelse(nchar(yy)==1,paste0("0",yy),yy),".rds")
    cat(file_name, "; ")
    if(file_name %in% list.files(paste0("~/Nextcloud/repository/humet/app/data/results_ttest/",mapping$method[xx],"/"))) next()
    out <- lapply(tp_combinations[[yy]],function(zz){
      lapply(met_id, function(ww){
        statistic_ttest(data=my_data, id=ww, baseline=names(tp_combinations)[yy], tp=zz)
      }) %>% 
        plyr::rbind.fill() %>% 
        dplyr::mutate(qvalue = pValue %>% p.adjust(method="BH"))
    }) %>% plyr::rbind.fill()
    saveRDS(out,
            file = paste0("~/Nextcloud/repository/humet/app/data/results_ttest/",mapping$method[xx],"/",file_name))
  }
}