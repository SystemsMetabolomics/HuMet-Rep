# HuMet function: helpers #
# Author: Patrick Weinisch
# Last updated: 01.02.2021

## filters -------------------------------------------------------------------------
help_filter_data=function(x.data, time=c(1:56), subject=c(1:15), platform=options$general$platform$platform_code){
  if(!exists("info_met")) stop("help_filter_data() could not find info_met")
  x.data$subject=x.data$subject %>% as.numeric()
  x.data$timepoint=x.data$subject %>% as.numeric()
  x.met=info_met$ID[which(info_met$platform_code %in% platform)]
  
  if(length(x.met)==0){
    warning("no metabolite could be matched")
    out=x.data[,c("subject","timepoint",x.met)]
  }else{
    out=x.data[which(x.data$subject %in% subject & x.data$timepoint %in% time),c("subject","timepoint",x.met)]
  }
  return(out)
}

## data transformation -------------------------------------------------------------
help_zscore_data<-function(x.data){
  if(any(c("subject", "timepoint") %in% names(x.data))){
    x.data[,-which(names(x.data) %in% c("subject","timepoint"))]=x.data[,-which(names(x.data) %in% c("subject","timepoint"))] %>% scale()
  }else{
    x.data=x.data%>% scale()
  }
  return(x.data)
}
help_log2_data<-function(x.data){
  if(any(c("subject", "timepoint") %in% names(x.data))){
    x.data[,-which(names(x.data) %in% c("subject","timepoint"))]=x.data[,-which(names(x.data) %in% c("subject","timepoint"))] %>% log2()
  }else{
    x.data=x.data%>% log2()
  }
  return(x.data)
}
## converters ----------------------------------------------------------------------

help_read_code <- function(x.code, to="timepoint", type="numeric"){
  out=x.code%>%
    str_split(pattern="s_", n=2) %>% sapply("[[", 2) %>%
    str_split(pattern="_t_", n=2)
  if(to=="timepoint"){
    out=out %>% sapply("[[", 2)
  }
  else if(to=="subject"){
    out=out %>% sapply("[[", 1)
  }
  else{
    out=NA
  }
  if(type=="numeric") out= out %>% as.numeric()
  if(type=="character") out= out %>% as.character()
  return(out)
}

help_convert_additional_tp<- function(x.timepoint){
  if(70 %in% x.timepoint) x.timepoint[which(x.timepoint==70)] = 10.5
  if(71 %in% x.timepoint) x.timepoint[which(x.timepoint==71)] = 11.5
  if(72 %in% x.timepoint) x.timepoint[which(x.timepoint==72)] = 27.5
  return(x.timepoint)
}



help_convert<-function(x, to=NULL){
  if(!exists("info_met")) stop("help_convert() could not find info_met")
  if(!to %in% names(info_met)) stop("help_convert() can only convert id or labels")
  

  if(any(x %in% info_met$ID) & !is.null(to)){
    ## if x is an id
    #if code is incomplete add a 0
      x=ifelse(nchar(x)==7, paste0("0",x, sep=""),ifelse(nchar(x)==6, paste0("00",x, sep=""), x))
      if (to %in% names(info_met)) out.x= merge(data.frame(ID=x),info_met[,c("ID",to)], by="ID", all.x=T)[,to] %>% as.character()
      else out.x=x
  }
  else if(any(x %in% info_met$labels) & !is.null(to)){
    if (to %in% names(info_met)) out.x= merge(data.frame(labels=x),info_met[,c("labels",to)], by="labels", all.x=T)[,to] %>% as.character()
    else out.x=x
  }
  else if (is.null(x)){
    out.x = NULL
  }
  else if (x==""){
    out.x = NULL
  }
  else{
    warning("help_convert() can only convert proper ids or labels")
    out.x=NULL
  }
  return(out.x)
}

help_convert_studyslider<-function(labels){
  if(!exists("info_sample")) stop()
  from=info_sample$timepoint[which(info_sample$challengeTime==labels[1])]
  to=info_sample$timepoint[which(info_sample$challengeTime==labels[length(labels)])]
  timepoints=as.numeric(c(from:to))
  return(timepoints)
}


## colors ---------------------------------------------------------------------------
help_platform_colors<-function(x){
  if(!exists("info_met")) stop("help_convert() could not find info_met")
  if(!to %in% names(info_met)) stop("help_convert() can only convert id or labels")
  
  
}


# Humet helper functions

## converters ----------------------------------------------------------------------

## converte ids and labels
hu_labels_id<-function(id=NULL, labels=NULL){
  ## convertes id or labels 
  out=NULL
  if(!exists("info_met")) stop("info_met not found!")
  else if(!is.null(id) & !is.null(labels)) stop("hu_labels_id() can only convert one variable")
  else{
    if(!is.null(id)) out=info_met$labels[which(info_met$ID %in% id)]
    if(!is.null(labels)) out=info_met$ID[which(info_met$labels %in% labels)]
  }
  return(out)
}
## convert code to tp
hu_code_to_tp<-function(code){
  ## convertes sample code to tp
  tp=NULL
  tp=code%>%
    substr(3, 4) %>%
    as.numeric()
  return(tp)
}
## converte fluid names
hu_fluid_convert<-function(fluid){
  # convert any fluid abbreviation to long version or vice versa
  if(any(fluid %in% c("P","U","BC","BA"))){
    fluid_out= data.frame(fluid=fluid, stringsAsFactors = F) %>% 
      dplyr::left_join(data.frame(fluid=c("P","U","BC","BA"),
                           fluid_new=c("Plasma","Urine","Breath condensate","Breath air"),
                           stringsAsFactors = F),
                by="fluid") %>% 
      dplyr::pull(var="fluid_new") %>% 
      as.character()
  }
  else if(any(fluid %in% c("Plasma","Urine","Breath condensate","Breath air"))){
    fluid_out= data.frame(fluid=fluid, stringsAsFactors = F) %>% 
      dplyr::left_join(data.frame(fluid_new=c("P","U","BC","BA"),
                                  fluid=c("Plasma","Urine","Breath condensate","Breath air"),
                                  stringsAsFactors = F),
                       by="fluid") %>% 
      dplyr::pull(var="fluid_new") %>% 
      as.character()
  }
  else{
    fluid_out=fluid
  }
}


## match Short fluid name to long or in reverse
huBasic_matchFluid<-function(fluidShort=NULL, fluidLong=NULL){
  match<-data.frame(fluidShort=c("P","U","BC","BA"),
                    fluidLong=c("Plasma","Urine","Breath condensate","Breath air"),
                    stringsAsFactors = F)
  out=NULL
  if(!is.null(fluidShort)){
    out<-match$fluidLong[which(match$fluidShort %in% fluidShort)]
  }
  if(!is.null(fluidLong)){
    out<-match$fluidShort[which(match$fluidLong %in% fluidLong)]
  }
  if(is.null(out) | length(out)==0){
    warning("fluid could not be matched")
  }else{
    return(out)
  }
}


## html Elements ----------------------------------------------------------------------      


## browser table ----------------------------------------------------------------------
## plot data to table
hu_browser_table<-function(plot_data){
  if(!exists("info_met")) stop("info_met not found!")
  if(!exists("info_sample")) stop("info_sample not found!")
  out_df<-data.frame(timepoints=unique(info_sample$timepoint))
  for(i in names(plot_data)){
    out_df<-merge(out_df, plot_data[[i]][,c("timepoints","mean")], by="timepoints",all.x=T)
    colnames(out_df)[ncol(out_df)]<-i
  }
  out_df<-out_df[which(rowSums(is.na(out_df))< ncol(out_df)-1),]
  colnames(out_df)[-1]<-info_met$labels[which(info_met$ID%in%names(plot_data))]
  return(out_df)
}


## coloring ---------------------------------------------------------------------------
hu_selectize_exlcude<-function(available_met, colorExclude="grey", text="line-trough"){
  if(!exists("info_met")) stop("huUI_selectizeColor(): could not find info_met")
  tagslist<-NULL
  to_disable=info_met$labels[-which(info_met$ID %in% available_met)]
  tagslist=paste0('.selectize-input [data-value=\"',to_disable,'\"] { color: ',colorExclude,' !important;text-decoration:line-through; }', collapse = "")
  return(tagslist)
}


## platform naming
#table=allTable[,c("Fluid","Platform")]
hu_switch_platform<-function(table){
  table$Fluid= table$Fluid %>% as.character()
  table$Platform= table$Platform%>% as.character()
  match_table=data.frame(
    Fluid_l=c("Plasma","Urine","Plasma","Plasma", "Breath air","Breath condensate", "Plasma","Urine","Plasma"),
    Fluid_s=c("P","U","P","P","BA","BC","P","U","P"),
    Platform_new=c("Metabolon HD4 [nt-ms]","Metabolon HD4 [nt-ms]","Biocrates p150 [t-ms]", "Lipidyzer [Lipidyzer]", "In-house PTR-MS [PTRMS]","In-house FTICR-MS [ICR]", "numares (Lipofit) [NMR]", "Chenomx [NMR]","In-house biochemistry [chem.]"),
    Platform=c("non-targeted LCMS","non-targeted LCMS","MS Biocrates p150","Lipidyzer","PTRMS", "ICR-FT-MS","NMR","NMR","Biochemistry"),
    stringsAsFactors = F
  )
  out=NA
  if(any(c("P","U")%in%table$Fluid)){
    table$Platform_new=NA
    table$code=paste0(table$Platform, table$Fluid)
    match_table$code=paste0(match_table$Platform, match_table$Fluid_s)
    for(i in 1:nrow(table)){
      table$Platform_new[i]=match_table$Platform_new[which(match_table$code==table$code[i])]
    }
    out=table$Platform_new
  }
  
  if(any(c("Plasma","Urine")%in%table$Fluid)){
    table$Platform_new=NA
    table$code=paste0(table$Platform, table$Fluid)
    match_table$code=paste0(match_table$Platform, match_table$Fluid_l)
    for(i in 1:nrow(table)){
      table$Platform_new[i]=match_table$Platform_new[which(match_table$code==table$code[i])]
    }
    out=table$Platform_new
  }
  return(out)
}

# add subpathway labeler
hu_subpathway_labels=function(sub_names){
  table=info_met[which(info_met$SUB.PATHWAY %in% sub_names), c("platform_name", "SUB.PATHWAY")]
  table=table[!duplicated(table$SUB.PATHWAY),]
  table=table[order(table$SUB.PATHWAY),]
  label_list=as.list(setNames(table$SUB.PATHWAY, paste0("(",table$platform_name, ") ",table$SUB.PATHWAY)))
  return(label_list)
}

hu_switch_fluid=function(fluid){
  fluid_out=ifelse(fluid=="Plasma", "Plasma [P]",
                   ifelse(fluid=="P", "Plasma [P]",
                          ifelse(fluid=="Urine", "Urine [U]",
                                 ifelse(fluid=="U", "Urine [U]",
                                        ifelse(fluid=="Breath air", "Breath air [BA]",
                                               ifelse(fluid=="BA", "Breath air [BA]",
                                                      ifelse(fluid=="Breath condensate", "Breath condensate [BC]",
                                                             ifelse(fluid=="BC", "Breath condensate [BC]",fluid))))))))
  return(fluid_out)
}
