setwd("~/Nextcloud/repository/humet/app/dev")
library(parallel)
library(SummarizedExperiment)
library(maplet)
library(tidyverse)


# get data
source("~/Nextcloud/repository/humet/app/dev/src/processing_functions.R")
file_path <- "~/Nextcloud/repository/humet/app/data/measurements/list_none_raw.rds"
my_data <- readRDS(file_path)

# functions -----

# imputation

info_met<-base::readRDS("~/Nextcloud/repository/humet/app/data/info/met.rds") %>% 
  dplyr::mutate(platform_code = ifelse(platform_code=="[P, lipidizer]", "[P, Lipidyzer]",platform_code))

final_dataframes <-list() 
final_list <- list()

# run imputation per platform -----
for(xx in unique(info_met$platform_code)){
  
  ## remove tmp folder 
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
    se_index <- data_to_impute %>% 
      dplyr::select(timepoint,code,subject) 
    rownames(se_index) = paste0("t_",1:nrow(se_index))
    se_data <- data_to_impute %>% 
      dplyr::select(-timepoint,-code,-subject)
    rownames(se_data) = paste0("t_",1:nrow(se_data))
    se_imp <- SummarizedExperiment::SummarizedExperiment(assay= se_data) %>% 
      mt_pre_impute_knn(n_cores=detectCores(all.tests = FALSE, logical = TRUE)-1,use_multicore=T)
    
    data_imputed=se_index %>% dplyr::mutate(row_code = rownames(.[])) %>% 
      dplyr::left_join(se_imp@assays@data@listData[[1]] %>% as.data.frame() %>% dplyr::mutate(row_code = rownames(.[]),by=row_code)) %>%
      dplyr::select(-row_code)
    unlink(x=paste0(getwd(),"/tmp"))
  }
  else{
    data_imputed <- data_to_impute
  }
  # 
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

saveRDS(final_df, file=paste0("~/Nextcloud/repository/humet/app/dev/data/","df_","knn","_raw.rds"))
saveRDS(final_list, file=paste0("~/Nextcloud/repository/humet/app/dev/data/","list_","knn","_raw.rds"))
