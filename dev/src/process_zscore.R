#calculate zscore

wd = rstudioapi::getSourceEditorContext()$path %>% dirname()%>% dirname()
files_list <- list.files(paste0(wd,"/data/")) %>% grep(pattern="list_",value=T)%>% grep(pattern="fcblock|zscore|fcchal",value=T,invert=T)
source("~/Nextcloud/repository/humet/app/dev/src/processing_functions.R")

zscore_data <- function(data, name){
  id_tp = "timepoint"
  id_code ="code"
  id_subject="subject"
  #get the data
  this_data <- data %>% 
    as.data.frame() %>% 
    dplyr::mutate(timepoint = substr(rownames(.[]),start=3, stop=4),
                  code = rownames(.[])) %>% 
    reshape2::melt(c("timepoint","code"),value.name = name) %>% 
    dplyr::rename("subject"="variable") %>%
    dplyr::mutate(subject = as.character(subject))
  this_data[,name] <-  scale(this_data[,name], center = TRUE, scale = TRUE) %>% as.numeric()
  
  mapping <- this_data[,c(id_tp, id_code)] %>% 
    dplyr::distinct()
  
  out <- this_data %>% 
    dplyr::select(-any_of(id_code)) %>% 
    reshape(timevar=id_subject, idvar = id_tp, direction = "wide") %>% 
    setNames(names(.[]) %>% gsub(pattern=paste0(name,"."), replacement="",fixed = T)) %>% 
    dplyr::select(-timepoint)
  rownames(out) <- rownames(data)
  return(out)
}

for(ii in files_list){
  cat(ii)
  dataset <- readRDS(paste0(wd,"/data/",ii))

  out <- lapply(names(dataset), function(yy){
   dataset[[yy]] %>% 
      zscore_data(name=yy)
  }) %>% 
    setNames(names(dataset)) 
  
  saveRDS(out, file = paste0(wd,"/data/",gsub(pattern="_raw",replacement="_zscore",x=ii)))
}
