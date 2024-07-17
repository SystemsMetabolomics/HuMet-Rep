# HuMet functions: selection
# Author: Patrick Weinisch
# Last updated: 22.03.2024


#### SelectTable ####
huBrowser_selectionTable<-function(matchingTable){
  matchingCols=c("labels","Metabolite","SUPER.PATHWAY","SUB.PATHWAY","Fluid","Platform","ID","COMP.ID","PLATFORM","CHEMICAL.ID","RI","MASS","CAS","PUBCHEM","KEGG","HMDB")
  output_table<-matchingTable[,matchingCols]
  colnames(output_table)=c("labels","Metabolite","Super-pathway","Sub-pathway","Fluid","Platform","ID","COMP.ID","PLATFORM","CHEMICAL.ID","RI","MASS","CAS","PUBCHEM","KEGG","HMDB")
  output_table$Fluid<-ifelse(output_table$Fluid=="P","Plasma",
                             ifelse(output_table$Fluid=="U","Urine",
                                    ifelse(output_table$Fluid=="BA", "Breath air",
                                           ifelse(output_table$Fluid=="BC", "Breath condensate", NA))))
  
  output_table$graph<-paste('<img src="', "img/thumbnails/", output_table$ID, ".png",'" height="30" height="180"></img>', sep="")
  #output_table$'RefMet*'<-paste0('<input type="radio" name="refmet" value="',output_table$labels, 'class="metV_tc" "/>', sep="")
  
  for(i in c("Fluid","PUBCHEM","KEGG","HMDB","Metabolite","Super-pathway","Sub-pathway", "PLATFORM","Platform")){
    output_table[[i]]=as.factor(output_table[[i]])
  }
  output_table=output_table[,c("graph","Metabolite","Super-pathway","Sub-pathway","Fluid","Platform","KEGG","HMDB", "ID","labels")]
  output_table$Rank="choose ranking option"
  output_table=output_table[order(output_table[["Super-pathway"]],output_table[["labels"]]),] # order metabolites
  rownames(output_table) = 1:nrow(output_table)
  return(output_table)
}


selection_table_basis <-function(matchingTable){
  columns_to_mutate = c("Fluid","KEGG","HMDB","Metabolite","SUPER.PATHWAY","SUB.PATHWAY","PLATFORM","platform_name","IUPAC","InchiKey")
  out_table <- matchingTable %>% 
    dplyr::mutate(fluid_code=Fluid, 
                   Fluid = case_when(
                     Fluid == "P" ~ "Plasma",
                     Fluid == "U" ~ "Urine",
                     Fluid == "BA" ~ "Breath air",
                     Fluid == "BC" ~ "Breath condensate",
                     TRUE ~ "no information"
                   ), 
                   graph = paste('<img src="', "img/thumbnails/", ID, ".png",'" height="30" height="180"></img>', sep=""),
                   Rank = "choose ranking option") %>% 
    mutate(across(any_of(columns_to_mutate), ~as.factor(.))) %>% 
    dplyr::mutate(Platform=platform_name) %>% 
    dplyr::mutate(chebi = rep_link_chebi(chebi),
                  pubchem = rep_link_pubchem(pubchem)) %>%
    dplyr::rename("Annotated super-pathway"="SUPER.PATHWAY","Annotated sub-pathway"="SUB.PATHWAY","ChEBI"="chebi", "PubChem"="pubchem",
                  "Swisslipids synonyms"="swisslipids_synonyms","Lipidmaps synonyms"="lipidmaps_synonyms") %>% 
    dplyr::arrange("Super-pathway",labels) %>% 
    as.data.frame()
    
  rownames(out_table) = 1:nrow(out_table)
  return(out_table)
}
browser_distance_mean<-function(table, distance_data, reference, method="euclidean"){
  
  my_data<-distance_data[,which(!colnames(distance_data)%in% c("code","subject"))]
  my_data<-aggregate.data.frame(my_data, by=list(my_data$timepoint), FUN = mean,na.rm=T )
  my_data<-t(my_data[,which(!colnames(my_data) %in% c("Group.1","timepoint"))])
  
  ## calculate distance
  dist = my_data %>% 
    dist(method=method) %>% 
    as.matrix()
  ## generate output
  out_table= data.frame(ID=rownames(dist),
                        distance=dist[,reference], 
                        stringsAsFactors = F)
  return(out_table)
}


browser_distance_single<-function(table, distance_data, min_overlap=4,reference, method="euclidean"){
  # get unique subjects for calculation
  my_subject <- unique(distance_data$subject)
  
  # calculate dist per subject
  out_table <- lapply(my_subject, function(x){
    # Filter down dataset to subject and all values existant for reference
    this_data <- distance_data %>% 
      dplyr::filter(subject == x) %>% 
      dplyr::select(-any_of(c("code","subject","timepoint"))) %>% 
      dplyr::filter(!is.na(get(reference)))
    
    # only add metabolites that have >= overlap with min_overlap to reference metabolite
    met_check <- this_data %>%  is.na() %>% colSums()
    # overlap is calculated by maximum number of timepoints for overlap - min_overlap
    met_use <- which(met_check<(nrow(this_data)-min_overlap)) %>% names()
    
    this_data %>% 
      t() %>% 
      dist(method=method) %>% 
      as.matrix() %>% 
      as.data.frame() %>% 
      dplyr::select(all_of(reference)) %>% 
      setNames(c("dist")) %>% 
      dplyr::mutate(ID = rownames(.[])) %>% 
      dplyr::mutate(dist = ifelse(ID %in% c(reference, met_use),dist, NA)) # remove all metabolites that are lower than min_overlap
  }) %>% 
    plyr::rbind.fill() %>% 
    dplyr::group_by(ID) %>% 
    dplyr::summarise(distance=mean(dist, na.rm=T)) %>% 
    dplyr::mutate(distance = ifelse(is.nan(distance),NA,distance))

  return(out_table)
}

browser_cor_single<-function(table, distance_data, min_overlap=4,reference){
  ## format data
  my_subject <- unique(distance_data$subject)
  
  # calculate dist per subject
  out_table <- lapply(my_subject, function(x){
    # get dataset of interest
    this_data <- distance_data %>% 
      dplyr::filter(subject == x) %>% 
      dplyr::select(-any_of(c("code","subject","timepoint"))) %>% 
      dplyr::filter(!is.na(get(reference)))
    # only add metabolites that have >= overlap with min_overlap to reference metabolite
    met_check <- this_data %>%  is.na() %>% colSums()
    met_use <- which(met_check<(nrow(this_data)-min_overlap)) %>% names()
    
    if(nrow(this_data)!=0){
      this_data %>% 
        cor(method="pearson", use="pairwise.complete.obs") %>% 
        as.data.frame() %>% 
        dplyr::select(all_of(reference)) %>% 
        setNames(c("dist")) %>% 
        dplyr::mutate(ID = rownames(.[])) %>% 
        dplyr::mutate(dist = ifelse(ID %in% c(reference, met_use),dist, NA)) # remove all metabolites that are lower than min_overlap
    }else{
      #in case a metabolite is not measured for a subject
      data.frame(ID = names(this_data),dist=NA,stringsAsFactors = F)
    }
  }) %>% 
    plyr::rbind.fill() %>% 
    dplyr::group_by(ID) %>% 
    dplyr::summarise(distance=mean(dist, na.rm=T)) %>% 
    dplyr::mutate(distance = ifelse(is.nan(distance),NA,distance))
  
  return(out_table)
}

browser_format_dist_table <- function(table, reference, cols){
  if("dist" %in% names(table)){
    out <- table %>% 
      dplyr::mutate(dist = round(dist, 4)) %>% 
      dplyr::arrange(dist)
  }else if("distance" %in% names(table)){
    out <- table %>% 
      dplyr::mutate(distance = round(distance, 4)) %>% 
      dplyr::arrange(distance)
  }else{
    out <- table
  }
  out <- out %>% 
    dplyr::select(all_of(cols))
  
  return(out)
}

statistic_calc_dtw = function(ref_id, data){
  mets=data[,-which(colnames(data) %in% c("code","subject","timepoint"))]
  results=data.frame(metabolites=names(mets),stringsAsFactors = F)
  
  for(i in unique(data$subject)){
    temp_mets=mets[which(data$subject==i),]
    out<-sapply(names(temp_mets), function(x){
      if(sum(is.na(temp_mets[[x]]))==length(temp_mets[[x]])){
        NA
      }else{
        dtw(na.omit(temp_mets[[ref_id]]),na.omit(temp_mets[[x]]),keep=TRUE)$normalizedDistance
      }
    })
    results[paste(i)]=out
  }
  
  results1=data.frame(id=results$metabolites,
                      dist=rowSums(results[,-1]/15),
                      stringsAsFactors = F)
  return(results1)
}


statistic_calc_frechet<-function(data, time, id){
  if(!id%in%names(data)) stop("id does not match to df")
  if(!nrow(data)==length(time)) stop("n time points do not match data")
  out=sapply(names(data), function(yy){
    if(all(is.na(data[[yy]]))){
      NA
    }else{
      # longitudinalData::distFrechet(Px=time,Py=data[[id]],
      #                        Qx=time, Qy=data[[yy]],
      #                        timeScale=0.1, FrechetSumOrMax = "max")
      kmlShape::distFrechet(Px=time,Py=data[[id]],
                            Qx=time, Qy=data[[yy]],
                            timeScale=0.1, FrechetSumOrMax = "max")
    }
  })
  outDF=data.frame(ID=as.character(names(out)),
                   dist=as.numeric(out),
                   stringsAsFactors = F)
  return(outDF)
}

statistic_calc_pearson<-function(ref_id, data){
  if(!id%in%names(data)) stop("id does not match to df")
  if(!nrow(data)==length(time)) stop("n time points do not match data")
  
  out=sapply(names(data), function(x){
    if(all(is.na(data[[x]]))){
      NA
    }else{
      longitudinalData::distFrechet(Px=time,Py=data[[id]],
                            Qx=time, Qy=data[[x]],
                            timeScale=0.1, FrechetSumOrMax = "max")
    }
  })
  outDF=data.frame(ID=as.character(names(out)),
                   dist=as.numeric(out),
                   stringsAsFactors = F)
  return(outDF)
}

statistic_calc_pearson_single<-function(x.data, ref_id){
  if(!ref_id%in%names(x.data)) stop("statistic_calc_pearson(): ref_id does not match to df")
  subject=x.data$subject
  timepoint=x.data$timepoint
  x.data=x.data[,-which(names(x.data) %in% c("subject","timepoint"))]
  # by subject
  by_person_data<-lapply(unique(subject), function(x.sub){
    my_data=x.data[which(subject==x.sub),]
    my_results=sapply(names(my_data), function(x.met){
      if(all(is.na(my_data[[x.met]]))){
        # if test metabolite misses values
        return_cor=NA
      }
      else{
        model=try(cor.test(x=my_data[,ref_id],y=my_data[,x.met],method="pearson")) 
        if(inherits(model, "try-error")) {
          return_cor=NA
        }
        else{
          return_cor=model$estimate %>% as.numeric()
        }
      }
    })
    
    by_person_results=data.frame(
      ref_id=ref_id,
      id=as.character(names(x.data)),
      dist=as.numeric(my_results),
      stringsAsFactors = F)
  })
  
  result_dist=data.frame(ref_id=by_person_data[[1]]$ref_id,
                         id=by_person_data[[1]]$id,
                         "01"=by_person_data[[1]]$dist,
                         stringsAsFactors = F)
  
  for(i in 2:length(unique(subject))){
    result_dist[paste0(unique(subject)[i])]=by_person_data[[i]]$dist
  }
  output_dist=rowSums(result_dist[,-c(1,2)], na.rm=T)/rowSums(!is.na(result_dist[,-c(1,2)]))
  
  output_df=cbind(result_dist[,c(1,2)], data.frame(dist=output_dist, stringsAsFactors = F))
  return(output_df)
}

