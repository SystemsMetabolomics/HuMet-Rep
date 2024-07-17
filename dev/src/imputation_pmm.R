
library(parallel)
library(mice)
library(tidyverse)

imputation = "pmm"

# get data
source("~/Nextcloud/repository/humet/app/dev/src/processing_functions.R")
file_path <- "~/Nextcloud/repository/humet/app/data/measurements/list_none_raw.rds"
my_data <- readRDS(file_path)

# functions -----
impute_mice <- function(data, method="2lonly.pmm",id_tp="timepoint", id_subject = "subject",id_code="code"){
  this_data <- data %>% 
    dplyr::mutate(merge_code=paste0(subject,"_",timepoint)) %>% 
    tibble::column_to_rownames("merge_code") %>%
    dplyr::arrange(timepoint,subject) %>% 
    dplyr::select(all_of(setdiff(names(.[]),c(id_tp, id_subject,id_code))))
  col_ids <- data.frame(id = names(this_data),
                        dummy = paste0("met",1:ncol(this_data)))
  
  names(this_data) <- col_ids$dummy
  mets_for_imputation <- names(this_data)[which(colSums(is.na(this_data))<nrow(this_data)*cutoff)]
  
  pred_matrix <- mice::quickpred(this_data[,mets_for_imputation],mincor = 0.6)
  mice_model <- mice::futuremice(this_data[,mets_for_imputation],
                                 n.core=parallel::detectCores()-1,
                                 method = method,
                                 predictorMatrix = pred_matrix, 
                                 print=F)
  
  imputed_data <- data %>% 
    dplyr::mutate(merge_code=paste0(subject,"_",timepoint)) %>%
    dplyr::select(merge_code,all_of(c(id_tp,id_subject,id_code))) %>% 
    dplyr::left_join(mice::complete(mice_model) %>% 
                       dplyr::mutate(merge_code = rownames(.[])),
                     by="merge_code") %>% 
    dplyr::select(-merge_code)
  
  non_imputed_data <- cbind(
    data[,c(id_tp,id_subject,id_code)],
    this_data[,setdiff(names(this_data),mets_for_imputation)]
  )
  
  non_imputed_data <- data %>% 
    dplyr::mutate(merge_code=paste0(subject,"_",timepoint)) %>%
    dplyr::select(merge_code,all_of(c(id_tp,id_subject,id_code))) %>% 
    dplyr::left_join(this_data[,setdiff(names(this_data),mets_for_imputation)] %>% dplyr::mutate(merge_code=rownames(.[])),
                     by="merge_code") %>% 
    dplyr::select(-merge_code)
  
  out <- plyr::join_all(dfs=list(imputed_data, non_imputed_data), by=c(id_tp,id_subject,id_code))
  for(i in 1:nrow(col_ids)){
    names(out)[which(names(out)==col_ids$dummy[i])] <- col_ids$id[i]
  }
  return(out)
}

# imputation

info_met<-base::readRDS("~/Nextcloud/repository/humet/app/data/info/met.rds") %>% 
  dplyr::mutate(platform_code = ifelse(platform_code=="[P, lipidizer]", "[P, Lipidyzer]",platform_code))

final_dataframes <-list() 
final_list <- list()

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
    data_imputed <- impute_mice(data = data_to_impute,
                                  id_tp="timepoint", 
                                  id_subject = "subject",
                                  id_code="code")
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

saveRDS(final_df, file=paste0("~/Nextcloud/repository/humet/app/dev/data/","df_","pmm","_raw.rds"))
saveRDS(final_list, file=paste0("~/Nextcloud/repository/humet/app/dev/data/","list_","pmm","_raw.rds"))
