# calculate foldchange

# setup ------------------------------------------------------------------------

require(tidyverse)

wd = rstudioapi::getSourceEditorContext()$path %>% dirname()%>% dirname()

# load data --------------------------------------------------------------------
# imp_df <- readRDS(file=paste0(wd,"/data_processed/data_imp_df.rds"))
# imp_list <- readRDS(file=paste0(wd,"/data_processed/data_imp_list.rds"))
# 
# 
# raw_df <- readRDS(file=paste0(wd,"/data_processed/data_raw_df.rds"))
# raw_list <- readRDS(file=paste0(wd,"/data_processed/data_raw_list.rds"))

info_sample <- readRDS("~/Nextcloud/repository/humet/app/data/info/sample.rds")

# add functions --------------------------------------------------------------------
covert_code <- function(x.code, to=c("timepoint","subject")){
  if(!to %in% c("timepoint","subject") & length(to)!=1) stop("covert_code() could not match")
  out = x.code %>% gsub(pattern="s_|s",replacement = "") %>% gsub(pattern="t_|t",replacement = "") %>% stringr::str_split(pattern="_")
  out = do.call(rbind.data.frame, out)
  names(out)=c("subject","timepoint")
  return(out[,to])
}

foldchange_df <-function(x.data){
  my_data = x.data %>% 
    tibble::column_to_rownames(var="code") %>% 
    dplyr::select(all_of(setdiff(names(.[]),c("timepoint","time","subject","code")))) %>% 
    log2() %>% 
    dplyr::mutate(timepoint=covert_code(rownames(.[]), to="timepoint") %>% as.numeric(),
                  subject=covert_code(rownames(.[]), to="subject")%>% as.numeric()) %>% 
    dplyr::left_join(info_sample %>% dplyr::select(timepoint, plot_timepoint), by="timepoint")%>% 
  dplyr::arrange(timepoint, subject)
  
  my_index =  my_data  %>% dplyr::select(timepoint, subject, plot_timepoint)
  my_data = my_data  %>% dplyr::select(-timepoint, -subject)
  my_ids=names(x.data)[which(!names(x.data)%in% c("timepoint","code","subject"))]
  out_data=my_data %>% as.matrix()

  use_cols = which(names(my_data) %in% my_ids)
  
  for(i in 1:nrow(info_sample)){
    if(info_sample$plot_timepoint[i] %in% my_index$plot_timepoint){
      use_rows = which(my_index$plot_timepoint == info_sample$plot_timepoint[i])
      baseline_rows = which(my_index$plot_timepoint == info_sample$challengeTPbeginn[i])
      
      baseline=my_data[baseline_rows,use_cols] %>% as.matrix()
      testtp=my_data[use_rows,use_cols]%>% as.matrix()
      out_data[use_rows,use_cols]=testtp-baseline 
    }
  }
  out_data= cbind(my_index,out_data)
  return(out_data)
}

foldchange_list <- function(x.list){
  out=sapply(names(x.list), function(x){
    my_data=x.list[[x]]
    my_tp=rownames(my_data) %>% base::substring(3,4) %>% as.numeric()
    my_code=info_sample %>% dplyr::filter(timepoint %in% my_tp)
    this_out=data.frame()
    for(i in 1:nrow(my_code)){
        this_tp = my_data[which(my_tp == my_code$timepoint[i]),] %>% log2()
        this_base = my_data[which(my_tp ==info_sample$timepoint[which(info_sample$plot_timepoint==my_code$challengeTPbeginn[i])]),] %>% log2()
        this_out= rbind(this_out,this_tp-this_base)
      }
    this_out%>% 
      mutate_all(~ifelse(is.nan(.), NA, .))
  }, USE.NAMES = TRUE)
 return(out)
}

files_list <- list.files(paste0(wd,"/data/")) %>% grep(pattern="list_",value=T)%>% grep(pattern="raw",value=T,invert=F)
for(ii in files_list){
  cat(ii)
  out <- readRDS(paste0(wd,"/data/",ii)) %>% foldchange_list()
  saveRDS(out, file = paste0(wd,"/data/",gsub(pattern="_raw",replacement="_fcchal",x=ii)))
}
