#imputation


get_data <- function(data,verbose=F,join_by = c("subject","timepoint","code")){
  
  out <- lapply(names(data), function(yy){
    cat(which(names(data)==yy),"; ")
    this_data <- data[[yy]] %>% 
      as.data.frame() %>% 
      dplyr::mutate(timepoint = substr(rownames(.[]),start=3, stop=4),
                    code = rownames(.[])) %>% 
      reshape2::melt(c("timepoint","code"),value.name = yy) %>% 
      dplyr::rename("subject"="variable") %>%
      dplyr::mutate(subject = as.character(subject))
  }) %>% 
    plyr::join_all(type="full",by=join_by) 
  return(out)
}

seperate_data <- function(data, id_tp="timepoint", id_subject = "subject",id_code="code"){
  mets <- setdiff(names(data), c(id_tp, id_subject,id_code))
  mapping <- data[,c(id_tp, id_code)] %>% 
    dplyr::distinct()
  out <- lapply(mets, function(yy)
    data[,c(id_tp, id_subject, yy)] %>%
      reshape(timevar=id_subject, idvar = id_tp, direction = "wide") %>% 
      dplyr::left_join(mapping, by=id_tp) %>% 
      tibble::column_to_rownames(var=id_code) %>% 
      dplyr::select(-any_of(c(id_tp, id_subject, id_code))) %>% 
      setNames(names(.[]) %>% gsub(pattern=paste0(yy, "."), replacement="")) 
  ) %>% 
    setNames(mets)
  return(out)
}