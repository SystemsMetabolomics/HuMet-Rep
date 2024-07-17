# Linear imputation of missing values
# Author: Patrick Weinisch
# Last change: 02.02.2024

# Info: missing values are only imputed if:
## - the value of TP + 1 and TP - 1 are available
## - TP is not the first and last of the block


# general -----

## load all needed packages ----
library(parallel)
library(tidyverse)

## load all functions ----

# prepare data ----

## get data ----
source("~/Nextcloud/repository/humet/app/dev/src/processing_functions.R")
file_path <- "~/Nextcloud/repository/humet/app/data/measurements/list_none_raw.rds"
my_data <- readRDS(file_path)


info_met<-base::readRDS("~/Nextcloud/repository/humet/app/data/info/met.rds") %>% 
  dplyr::mutate(platform_code = ifelse(platform_code=="[P, lipidizer]", "[P, Lipidyzer]",platform_code))

info_sample<-base::readRDS("~/Nextcloud/repository/humet/app/data/info/sample.rds")


final_dataframes <-list() 
final_list <- list()


impute_linear <- function(data, 
                          cutoff=0.3,
                          id_tp="timepoint", 
                          id_subject = "subject",
                          id_code="code",
                          block1 = info_sample$timepoint[which(info_sample$block==1)], 
                          block2 = info_sample$timepoint[which(info_sample$block==2)]){
  
  # save index of data
  index <- data %>% 
    dplyr::select(all_of(c(id_tp, id_subject, id_code)))
  
  #reformat data to long format
  # add a block_code which specifies subject and block
  
  long_data <- data %>% 
    dplyr::mutate(tp_coded = as.numeric(timepoint)) %>% 
    dplyr::mutate(blocks = ifelse(tp_coded %in% block1, "block1", 
                                    ifelse(tp_coded %in% block2, "block2", NA))) %>% 
    dplyr::select(-tp_coded) %>% 
    dplyr::mutate(block_code =  paste0(subject,blocks)) %>% 
    reshape2::melt(c("code","subject","timepoint","block_code")) %>% 
    dplyr::mutate(variable=as.character(variable),value=as.numeric(value)) %>% 
    dplyr::rename("metabolite" = "variable")
  
  # split data into blocks and 
  chunk_index <- long_data %>% 
    dplyr::select(metabolite, block_code) %>% 
    dplyr::distinct()
  
  chunk_data <- lapply(1:nrow(chunk_index),function(yy){
    tmp_chunk_data <- long_data %>%
      dplyr::filter(block_code == chunk_index$block_code[yy],metabolite==chunk_index$metabolite[yy])
    
    values_imputed <- lapply(which(is.na(tmp_chunk_data$value)),function(zz){
      if(zz==1 | zz == nrow(tmp_chunk_data)){
        out <- NA
      }
      else if(!is.na(tmp_chunk_data$value[zz-1]) & !is.na(tmp_chunk_data$value[zz+1])){
        out <- approx(y=tmp_chunk_data$value[c(zz-1,zz+1)], x=as.numeric(tmp_chunk_data$timepoint[c(zz-1,zz+1)]), xout=as.numeric(tmp_chunk_data$timepoint[[zz]]))$y %>% unlist()
      }else{
        out<-NA
      }
      out
    }) %>% 
      setNames(which(is.na(tmp_chunk_data$value))) %>% 
      unlist()
    
    tmp_chunk_data$value[as.numeric(names(values_imputed))] <- values_imputed
    tmp_chunk_data
    }) %>% 
    plyr::rbind.fill()
  
  out <- lapply(unique(chunk_data$metabolite),function(yy){
    chunk_data %>% 
      dplyr::filter(metabolite==yy) %>% 
      dplyr::select(code, subject,timepoint,value) %>% 
      setNames(c("code","subject","timepoint",yy))
  }) %>% 
    plyr::join_all(type="full", by=c("code","subject","timepoint"))
  return(out)
}


# run imputation per platform -----
for(xx in unique(info_met$platform_code)){
  ## filter metabolites in question
  this_mets <- info_met$ID[which(info_met$platform_code==xx)]
  
  ## get dataset 
  data_all <- get_data(my_data[this_mets])
  
  ## split dataset into files with and without 30pct missingness
  cutoff = 0.3
  met_to_impute <- names(data_all)[which((colSums(is.na(data_all)) < nrow(data_all)*cutoff) & (colSums(is.na(data_all))>1))] %>% 
    setdiff(c("timepoint","code","subject"))
  
  data_to_impute <- data_all %>% 
    dplyr::select(timepoint, code, subject, all_of(met_to_impute))
  
  data_non_impute <- data_all%>% 
    dplyr::select(-all_of(met_to_impute))
  
  # impute data
  if(ncol(data_to_impute)>3){
  #if(xx %in% c("[P, Lipidyzer]","[P, nt-ms]","[U, nt-ms]","[P, t-ms]")){
    data_imputed <- impute_linear(data = data_to_impute,
                                  id_tp="timepoint", 
                                  id_subject = "subject",
                                  id_code="code",
                                  block1 = info_sample$timepoint[which(info_sample$block==1)], 
                                  block2 = info_sample$timepoint[which(info_sample$block==2)])
  }
  else{
    data_imputed <- data_to_impute
  }
  
  # merge data 
  full_data_all <- full_join(data_non_impute, data_imputed, by=c("timepoint","subject","code")) %>% 
    dplyr::select(all_of(names(data_all)))
  
  # converte back to list objects
  imputed_list <- seperate_data(data=full_data_all,
                                id_tp="timepoint", id_subject = "subject",id_code="code")
  
  final_dataframes[[xx]] <-full_data_all %>% select(-code)
  final_list <- append(final_list,
                       imputed_list[1:length(imputed_list)])
}


final_df <- plyr::join_all(final_dataframes, by=c("timepoint","subject"),type="full") %>% 
  dplyr::mutate(code = paste0("s", subject, "_t_",timepoint)) %>% 
  dplyr::select(code, all_of(setdiff(names(.[]),c("code","timepoint","subject"))))

saveRDS(final_df, file=paste0("~/Nextcloud/repository/humet/app/dev/data/","df_","linear","_raw.rds"))
saveRDS(final_list, file=paste0("~/Nextcloud/repository/humet/app/dev/data/","list_","linear","_raw.rds"))
