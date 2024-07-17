#### download functions ####

help_convert_csv=function(data, sd=T, minmax=T){
  output<-timecode[,c("plot_timepoint","timepoint","block","day","day_time","challengeName")]
  if("mean" %in% colnames(data[[1]])){
    #add averaged
    use_column=c("mean")
    if(sd==T){use_column=append(use_column,"sd")}
    if(minmax==T){use_column=append(use_column,"min");use_column=append(use_column,"max")}
    add_average=NULL
    for(metID in names(data)){
      metName=matchingTable$labels[which(matchingTable$ID==metID)]
      data[[metID]]$metabolite=metName
      add_average=rbind(add_average,data[[metID]][,c("timepoints","metabolite",use_column)])
    }
    output<-merge(output, add_average, by.x="plot_timepoint",by.y="timepoints",all.y=T)
    output<-output[which(rowSums(is.na(output))==0),c("metabolite","timepoint","block","day","day_time","challengeName",use_column)]
    output<-output[order(output$metabolite, output$timepoint),]
    rownames(output)<-1:nrow(output)
  }else{
    ##add individuals
    nsubjects=colnames(data[[1]])[-1]
    output_single=NULL
    for(i in nsubjects){
      output$subject=i
      output_single=rbind(output, output_single)
    }
    output=output_single
    output$code=paste(output$plot_timepoint, output$subject)
    
    #add individuals
    add_single=NULL
    for(metID in names(data)){
      metName=matchingTable$labels[which(matchingTable$ID==metID)]
      metData=NULL
      for(subj in colnames(data[[metID]][-1])){
        metData_subj=data[[metID]][,c("timepoints",subj)]
        metData_subj$subject=subj
        colnames(metData_subj)[2]=metName
        metData=rbind(metData,metData_subj)
      }
      metData$code=paste(metData$timepoints, metData$subject)
      output=merge(output,metData[,c(metName,"code")], by="code",all.x = T)
      
    }      
    metNames=metName=matchingTable$labels[which(matchingTable$ID%in%names(data))]
    output=output[which(rowSums(is.na(output))==0),c("subject","timepoint","block","day","day_time","challengeName",metNames)]
    output$subject=as.numeric(output$subject)
    output<-output[order(output$subject, output$timepoint),]
    rownames(output)=1:nrow(output)
  }
  
  return(output)
}
download_metabdata=function(data=reacHigh$allData,average=F,metID,subjects,sd=T,minmax=T){
  if(average==T){
    data=highchart_process_data(data=data[metID],subjects=subjects)
  }else{
    data=highchart_single_process(data=data[metID],subjects=subjects)
  }
  convert_csv(data=data, sd=sd, minmax==minmax)
}
#data=highchart_process_data(data=datacache$normalized_interpolated[1300:1319],subjects=1:15)
#data=highchart_single_process(data=humetDB$normalized_interpolated["01090804"],subjects=1:15)
#test=download_metabdata(humetDB$normalized_interpolated[1300:1319],subjects=1:15)


download_all<-function(code, run_day, outlier, imputation, transformation){
  if(!exists("info_met")) warning("info_met(): ui_elements could not be found")
  if(!exists("ui_elements")) warning("download_all(): ui_elements could not be found")
  
  #get code
  if(!any(code %in% ui_elements[["study_sum_platforms"]]$code)){
    warning("download_all() could not match data")
    id_mets=NA
  }else{
    id_mets=NULL
    for(i in 1:length(code)){
      id_platform=ui_elements[["study_sum_platforms"]]$Platform_code[which(ui_elements[["study_sum_platforms"]]$code==code[i])]
      id_fluid=huBasic_matchFluid(fluidLong=ui_elements[["study_sum_platforms"]]$Medium[which(ui_elements[["study_sum_platforms"]]$code==code[i])])
      id_mets<-append(id_mets, info_met$ID[which(info_met$Fluid==id_fluid & info_met$Platform==id_platform)])
    }
  }
  # get data
  if(imputation=="none"){
    data=readRDS("app/data/conc/hudata_dist_imp.rds")
  }
  if(imputation=="missforest"){
    data=readRDS("app/data/conc/hudata_dist_norm_imp.rds")
  }
  
  #get transformation
  if(transformation=="zscore"){
    data[,-c(1:3)]<-scale(data[,-c(1:3)])
  }
  if(transformation=="log2"){
    data[,-c(1:3)]<-log2(data[,-c(1:3)])
  }
  
  ##filter data
  data=data[,c(2:3,which(colnames(data) %in% id_mets))]
  return(data)
}

# network downloads
download_network_edges=function(edges){
  edges$superpw_from=NA
  edges$subpw_from=NA
  
  edges$superpw_to=NA
  edges$subpw_to=NA
  
  colnames(edges)[which(colnames(edges)=="from")]<-"metabolite_from"
  colnames(edges)[which(colnames(edges)=="to")]<-"metabolite_to"
  if("value" %in% names(edges)){
    names(edges)[which(names(edges)=="value")]<-"dyn_pcor"
  }
  for(i in 1:nrow(edges)){
    if(!edges$metabolite_from[i] %in% info_met$ID){
      edges$superpw_from[i]<- "-"
      edges$subpw_from[i]<- "-"
    }else{
      edges$superpw_from[i]<- info_met$SUPER.PATHWAY[which(info_met$ID == edges$metabolite_from[i])]
      edges$subpw_from[i]<- info_met$SUB.PATHWAY[which(info_met$ID == edges$metabolite_from[i])]
      edges$metabolite_from[i]<- info_met$labels[which(info_met$ID == edges$metabolite_from[i])] 
    }
    if(!edges$metabolite_to[i] %in% info_met$ID){
      edges$superpw_to[i]<- "-"
      edges$subpw_to[i]<- "-"
    }else{
      edges$superpw_to[i]<- info_met$SUPER.PATHWAY[which(info_met$ID == edges$metabolite_to[i])]
      edges$subpw_to[i]<- info_met$SUB.PATHWAY[which(info_met$ID == edges$metabolite_to[i])]
      edges$metabolite_to[i]<- info_met$labels[which(info_met$ID == edges$metabolite_to[i])]
    }
  }
  colnames(edges)[which(colnames(edges)=="metabolite_from")]="metabolite_1"
  colnames(edges)[which(colnames(edges)=="metabolite_to")]="metabolite_2"
  colnames(edges)[which(colnames(edges)=="superpw_from")]="superpw_1"
  colnames(edges)[which(colnames(edges)=="superpw_to")]="superpw_2"
  colnames(edges)[which(colnames(edges)=="subpw_from")]="subpw_1"
  colnames(edges)[which(colnames(edges)=="subpw_to")]="subpw_2"
  use_cols=which(names(edges) %in% c("metabolite_1","metabolite_2","dyn_pcor","superpw_1","subpw_1","superpw_2","subpw_2"))
  edges=edges[,use_cols]
  rownames(edges)=1:nrow(edges)
  return(edges)
}



#### download dataset ####
download_data<-function(platform=unique(info_met$platform_code), correction=c("bcorrection"), outlier=c("manual"), imputation=c("none","missforest"), transformation=c("log2","zscore")){
  #create template
  new_data=data.frame(subject=NA,info_sample[,c("timepoint", "day","block","day_time","challengeName","challengeTPbeginn","challengeTPend")])
  colnames(new_data)=c("subject","timepoint","day","block","day_time","challenge_name","challenge_tp_beginn","challenge_tp_end")
  my_data=NULL
  for(i in 1:15){
    new_data$subject=i
    my_data=rbind(my_data,new_data)
  }
  my_data$subject=my_data$subject%>%as.character()
  my_data$subject=ifelse(nchar(my_data$subject)==1,paste0(0,my_data$subject),my_data$subject)
  
  my_data$timepoint=my_data$timepoint%>%as.character()
  my_data$timepoint=ifelse(nchar(my_data$timepoint)==1,paste0(0,my_data$timepoint),my_data$timepoint)
  my_data$code=paste0("S",my_data$subject,"T",my_data$timepoint)
  #get data
  if(imputation=="none" & transformation %in% c("none", "zscore")){
    my_dataset=readRDS("app/data/data_raw_df.rds")
    names(my_dataset)=c( "[P, nt-ms]","[U, nt-ms]","[U, NMR]" ,"[P, t-ms]", "[P, chem.]","[P, lipidizer]","[P, NMR]" ,"[BA, PTRMS]" ,"[BC, ICR]")
  }
  else if(imputation=="missforest" & transformation %in% c("none", "zscore")){
    my_dataset=readRDS("app/data/data_imp_df.rds") 
    names(my_dataset)=c( "[P, nt-ms]","[U, nt-ms]","[U, NMR]" ,"[P, t-ms]", "[P, chem.]","[P, lipidizer]","[P, NMR]" ,"[BA, PTRMS]" ,"[BC, ICR]")
  }
  else{
    my_dataset=NULL
  }
  
  # merge dataset and template
  
  if(!any(platform %in% unique(info_met$platform_code))){
    warning("download_data() could not find platform")
  }
  else if(is.null(my_dataset)){
    warning("download_data() could find choices")
  }
  else{
    for(i in platform){
      this_data=my_dataset[[i]]
      this_data$subject=this_data$subject%>%as.character()
      this_data$subject=ifelse(nchar(this_data$subject)==1,paste0(0,this_data$subject),this_data$subject)
      
      this_data$timepoint=this_data$timepoint%>%as.character()
      this_data$timepoint=ifelse(nchar(this_data$timepoint)==1,paste0(0,this_data$timepoint),this_data$timepoint)
      this_data$code=paste0("S",this_data$subject,"T",this_data$timepoint)
      
      this_data$timepoint=NULL
      this_data$subject=NULL
      
      my_data=merge(my_data, this_data, by="code",all.x=T)
    }
    
    my_data$code=NULL
    my_info=my_data[,c("subject" , "timepoint","day","block","day_time"  , "challenge_name", "challenge_tp_beginn" ,"challenge_tp_end")]
    my_metabolite=my_data[,c(which(names(my_data) %in% info_met$ID))]
    
    new_metabolite_names=info_met$labels[match(info_met$ID,colnames(my_data)[-c(1:8)])]
    
    colnames(my_metabolite)=info_met$labels[match(info_met$ID,colnames(my_metabolite))]%>%na.omit()
    my_data=cbind(my_info,my_metabolite)
  }
  
  if(transformation=="zscore"){
    my_data[,-c(1:8)]=scale(my_data[,-c(1:8)])
  }
  
  return(my_data)
}

