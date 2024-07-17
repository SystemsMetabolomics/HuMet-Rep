## functions for the statistics module ###


#### PCA functions ####
## Calculate data for PCA
huStatistics_calcPCAdata<-function(data, platform, fluid, subject,timepoint){
  # filter for errors
  if(intersect(fluid,c("U","BA","BC","P"))>0){
    # metabolites used 
    metabolites_used=info_met$ID[which(info_met$platform_name %in%platform & info_met$Fluid%in%fluid)]
    
    #generate outputList
    outputList<-list()
    
    if(length(metabolites_used)>0){
      #get data
      my_data=data[,c("timepoint","subject",metabolites_used)] 
      
      #reduce data by removed time points and subjects
      my_data=my_data[which(my_data$subject %in% subject & my_data$timepoint%in% timepoint),] # filter for subjects
      
      #reduce data when timepoints weren't measured on platforms
      if(length(intersect(c("U","BA","BC") , fluid))>0){
        tp_remove=unique(my_data$timepoint[which(rowSums(is.na(my_data))==length(metabolites_used))])
        my_data=my_data[-which(my_data$timepoint%in%tp_remove),]
      }else{
        tp_remove=NULL
      }
      
      #reduce data when columns(metabolites) have missing values
      metabolites_missing=colnames(my_data)[(which(colSums(is.na(my_data))>0))]
      if(length(metabolites_missing)>0){
        my_data=my_data[,-which(colnames(my_data)%in% metabolites_missing)]
      }
      sampleInfo=my_data
      # merge data
      personColors=data.frame(subject=1:15,
                              subject_color=c("#000000", "#696969", "#d3d3d3","#ffff00", "#8b4513", "#d2b48c", 
                                              "#007000", "#00ff00", "#40e0d0","#ff00ff", "#87ceeb", "#8a2be2", 
                                              "#ff0000", "#0000cd", "#ffa500"),
                              subject_name=c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15"),
                              stringsAsFactors = F
      )
      
      challengeColors=data.frame(timepoint=1:56,
                                 timepoint_gradient=rainbow(56),
                                 challenge_color=c(rep("#FF0000",9),
                                                   rep("#008080",2),
                                                   rep("#000080", 9),
                                                   rep("#808080", 8),
                                                   rep("#0000FF", 4),
                                                   rep("#800080", 7),
                                                   rep("#FF6403",10),
                                                   rep("#008000", 7)),
                                 challenge_gradient=NA, # add gradient
                                 challenge_name=c(rep("Fasting",9), 
                                                  rep("Fasting recovery (SLD)",2), 
                                                  rep("Standard liquid diet (SLD)", 9),
                                                  rep("Oral glucose tolerance (OGTT)", 8),
                                                  rep("Lunch (SLD)", 4),
                                                  rep("Physical activity (PAT)", 7),
                                                  rep("Oral lipid tolerance (OLTT)",10),
                                                  rep("Cold stress test", 7)),
                                 challenge_nameTime=c("Fasting 12h","Fasting 14h","Fasting 16h","Fasting 18h","Fasting 20h","Fasting 22h", "Fasting 24h","Fasting 26h","Fasting 28h","Fasting 36h/Fasting recovery 0h","Fasting recovery 2h","Standard liquid diet 0h","Standard liquid diet 15min","Standard liquid diet 30min","Standard liquid diet 45min","Standard liquid diet 60min","Standard liquid diet 90min","Standard liquid diet 120min","Standard liquid diet 3h","Standard liquid diet 4h","Oral glucose tolerance 0h","Oral glucose tolerance 15min","Oral glucose tolerance 30min","Oral glucose tolerance 45min","Oral glucose tolerance 60min", "Oral glucose tolerance 90min", "Oral glucose tolerance 120min", "Oral glucose tolerance 3h", "Oral glucose tolerance 4h/Lunch SLD 0h", "Lunch SLD 1h", "Lunch SLD 2h", "Lunch SLD 3h", "Lunch SLD 4h/Physical activity 0h","Physical activity 15min","Physical activity 30min","Physical activity 45min","Physical activity 60min","Physical activity 90min","Physical activity 120min","Oral lipid tolerance 0h","Oral lipid tolerance 30min","Oral lipid tolerance 60min","Oral lipid tolerance 90min","Oral lipid tolerance 120min","Oral lipid tolerance 3h","Oral lipid tolerance 4h","Oral lipid tolerance 5h","Oral lipid tolerance 6h","Oral lipid tolerance 7h","Oral lipid tolerance 8h/Cold stress test 0h","Cold stress test 15min","Cold stress test 30min","Cold stress test 45min","Cold stress test 60min","Cold stress test 90min","Cold stress test 120min"),
                                 stringsAsFactors = F)
      
      pca.pooled=summary(prcomp((sampleInfo[,which(colnames(sampleInfo)%in%metabolites_used)]), scale=T))
      sampleInfo=merge(sampleInfo, personColors, by="subject",all.x=T)
      sampleInfo=merge(sampleInfo, challengeColors, by="timepoint",all.x=T)
      
      #order data
      sampleInfo=sampleInfo[order(sampleInfo$timepoint, sampleInfo$subject),] #order data
      rownames(sampleInfo)=1:nrow(sampleInfo)
      
      outputList[["data"]]=pca.pooled
      outputList[["sampleInfo"]]=sampleInfo[,unique(c("subject",colnames(challengeColors), colnames(personColors)))]
      outputList[["metaboliteInfo"]]=info_met[which(info_met$ID %in% colnames(my_data)),c("ID","labels","BIOCHEMICAL","Fluid","Platform","SUPER.PATHWAY","SUB.PATHWAY","KEGG","HMDB","PUBCHEM")]

      superColor=options[["general"]]$super_pathway
      superColor=superColor[,c("super_pathway","super_color")]
      colnames(superColor)=c("SUPER.PATHWAY","super.color")
      
      #platformColor1=options[["general"]]$platform
      platformColor=options[["network"]][["info_platform"]]
      colnames(platformColor)=c("Platform","platform_id","platform.color","platform.color.background")
      
      #superColor=data.frame(SUPER.PATHWAY=c("Amino Acid","Amino Acids, Peptides, and Analogues", "Xenobiotics","Nucleotide","Lipid","Lipids","Energy","Peptide","Carbohydrate","Carbohydrates and Carbohydrate Conjugates","Cofactors and Vitamins",NA,"Not available", "Lipoprotein","Aliphatic Acyclic Compounds","Organic compounds"),
      #                      super_color=c("#E41A1C","#E41A1C","#377EB8","#4DAF4A","orange","orange","#A65628","#F781BF","#999999","#999999","#984EA3","#bfbfbf","grey", "blueviolet","grey","grey"),
      #                      stringsAsFactors = F)
      
      #platformColor=data.frame(Platform=c("non-targeted LCMS", "MS Biocrates p150","Biochemistry","NMR","Lipidizer","PTRMS","ICR-FT-MS",NA),
      #                         platform_color=c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#bfbfbf"),
      #                         stringsAsFactors = F)
      
      #superColor=data.frame(SUPER.PATHWAY=c("Amino Acid","Amino Acids, Peptides, and Analogues", "Xenobiotics","Nucleotide","Lipid","Lipids","Energy","Peptide","Carbohydrate","Carbohydrates and Carbohydrate Conjugates","Cofactors and Vitamins",NA,"Not available", "Lipoprotein","Aliphatic Acyclic Compounds","Organic compounds"),
      #                      super.color=c("#E41A1C","#E41A1C","#377EB8","#4DAF4A","orange","orange","#A65628","#F781BF","#999999","#999999","#984EA3","#bfbfbf","grey", "blueviolet","grey","grey"),
      #                      stringsAsFactors = F)
      
      #platformColor=data.frame(Platform=c("non-targeted LCMS", "MS Biocrates p150","Biochemistry","NMR","Lipidizer","PTRMS","ICR-FT-MS",NA),
      #                         platform.color=c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#bfbfbf"),
      #                         stringsAsFactors = F)
      
      outputList[["metaboliteInfo"]]=merge(outputList[["metaboliteInfo"]],superColor, by="SUPER.PATHWAY",all.x=T)
      outputList[["metaboliteInfo"]]=merge(outputList[["metaboliteInfo"]],platformColor, by="Platform",all.x=T)
      outputList[["type"]]="bySample"
      outputList[["otherInfo"]]=list(removedTP=tp_remove,
                                     usedTP=timepoint[-which(timepoint %in% tp_remove)],
                                     metabolites_missing=metabolites_missing)
      outputList[["removedTP"]]=tp_remove
      outputList[["usedTP"]]=tp_remove
      scree=as.data.frame(pca.pooled$importance)
      #scree=data$data$importance
      results=data.frame(dimensions=character(), exp.var=numeric())
      for(i in 1:ncol(scree)){
        results=rbind(results, data.frame(dimensions=paste0(i),
                                          exp.var=scree[[i]][1]))
      }
      results$exp.var=round(results$exp.var,2)
      outputList[["importance"]]=results
      return(outputList)
    }else{
      warning("check function huStatistics_calcPCAdata")
    }
  }else{
    #add warnings
  }
  
  ####
}

## Calculate variance explained per PCA
huStatistics_varianceExplained<-function(data, n=10){
  plot_cords=as.data.frame(data$data$importance, stringsAsFactors = F)
  plot_cords=as.numeric(plot_cords[1,])
  color_pal <- setNames(c(1:10),  paste("PC",c(1:n)))
  p <- plot_ly(
    x =c(1:10),
    y = plot_cords[1:n],
    marker = list(color = "black"),
    text = paste(round(plot_cords[1:n],2), "%"),
    textposition = 'auto',
    type = "bar"
  ) %>%
    layout(#title = "Scree plot",
      xaxis = list(title = "Dimensions"),
      yaxis = list(title = "Percentage of explained variances")) %>%
    htmlwidgets::onRender("function(el, x) {Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')}") %>%
    config(displaylogo = FALSE, modeBarButtonsToRemove = c("sendDataToCloud", "zoomOut2d","zoomIn2d","toggleSpikelines","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian","select2d","lasso2d"))
  
  return(p)
}

## Plot PCA loadings
huStatistics_plotLoadings<-function(data, type="2D",color_by=c("super", "default","platform"),marker_size=5,filter=T, use_PC=c(1:3)){
  
  if(data[["type"]]=="bySample"){
    plot_cords=cbind(as.data.frame(data[["data"]][["rotation"]][,paste0("PC",use_PC)], stringsAsFactors = F),
                     data[["metaboliteInfo"]])
    ## color options
    if(color_by=="default"){
      plot_cords$color="grey"
      color_pal="grey"
    }
    if(color_by=="super"){
      plot_cords$color=plot_cords$SUPER.PATHWAY
      color_pal <- setNames(plot_cords$super.color, plot_cords$SUPER.PATHWAY)
    }
    if(color_by=="platform"){
      plot_cords$color=plot_cords$Platform
      color_pal <- setNames(unique(plot_cords$platform.color), unique(plot_cords$Platform))
    }
    
    if(length(unique(plot_cords$SUPER.PATHWAY))>=1){
      plot_cords$text<-paste('</br> Metabolite: ', plot_cords$BIOCHEMICAL,
                             '</br> Super-pathway: ', plot_cords$SUPER.PATHWAY,
                             '</br> Sub-pathway: ', plot_cords$SUB.PATHWAY,
                             '</br> Platform: ', plot_cords$Platform)
    }else{
      plot_cords$text<-paste('</br> Metabolite: ', plot_cords$BIOCHEMICAL,
                             '</br> Fluid: ', huBasic_matchFluid(fluidShort=plot_cords$Fluid),
                             '</br> Platform: ', plot_cords$Platform)
    }
    if(type=="3D"){
      pca_p= plot_ly(source="loadingsPlot",
                     x=plot_cords[,paste0("PC",use_PC[1])], 
                     y=plot_cords[,paste0("PC",use_PC[2])], 
                     z=plot_cords[,paste0("PC",use_PC[3])], 
                     type="scatter3d",
                     mode="markers",
                     marker=list(size=marker_size),
                     color=plot_cords$color,
                     key = plot_cords$labels,
                     colors=color_pal,
                     hoverinfo = 'text',
                     text=plot_cords$text)%>%
        layout(
          scene=list(
            xaxis = list(title=paste0("PC",use_PC[1], " (",data$importance$exp.var[use_PC[1]], "%)")), 
            yaxis = list(title=paste0("PC",use_PC[2], " (",data$importance$exp.var[use_PC[2]], "%)")), 
            zaxis= list(title=paste0("PC",use_PC[3], " (",data$importance$exp.var[use_PC[3]], "%)"))
          ),
          legend = list(orientation = 'h')
        )%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("orbitRotation","tableRotation","resetCameraDefault3d","resetCameraLastSave3d","hoverClosest3d"))%>%
        htmlwidgets::onRender("function(el, x) {Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')}")
    }
    if(type=="2D"){
      pca_p= plot_ly(source="loadingsPlot",
                     x=plot_cords[,paste0("PC",use_PC[1])], 
                     y=plot_cords[,paste0("PC",use_PC[2])], 
                     type="scatter",
                     mode="markers",
                     marker=list(size=marker_size),
                     color=plot_cords$color,
                     key = plot_cords$labels,
                     colors=color_pal,
                     hoverinfo = 'text',
                     text=plot_cords$text)%>%
        layout(
          xaxis = list(title=paste0("PC",use_PC[1], " (",data$importance$exp.var[use_PC[1]], "%)")), 
          yaxis = list(title=paste0("PC",use_PC[2], " (",data$importance$exp.var[use_PC[2]], "%)")), 
          legend = list(orientation = 'h')
        )%>%htmlwidgets::onRender("
function(el, x) {
                                  Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')
    }
                                  ")%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("sendDataToCloud", "zoomOut2d","zoomIn2d","toggleSpikelines","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian","select2d","lasso2d"))
  }
  }
  return(pca_p)
  }

## Plot PCA scores

huStatistics_plotScores=function(data, type="3D",color_by=c("subject","timepoint","challenge", "default"),marker_size=5,use_PC=c(1:3)){
  if(is.null(data)) stop("huStatistics_plotScores() cannot match data")
  if(!type%in%c("2D","3D")) stop("huStatistics_plotScores() cannot match type")
  
  if(data[["type"]]=="bySample"){
    plot_cords=cbind(as.data.frame(data[["data"]]$x, stringsAsFactors = F), data[["sampleInfo"]])
    if(color_by=="subject"){
      plot_cords$color=as.character(plot_cords$subject)
      plot_cords$color=ifelse(nchar(plot_cords$color)==1, paste0("0",plot_cords$color),plot_cords$color)
      color_pal <- setNames(unique(plot_cords$subject_color), unique(plot_cords$color))
    }
    if(color_by=="timepoint"){
      plot_cords$color=plot_cords$timepoint
      color_pal <- setNames(unique(plot_cords$timepoint_gradient), unique(plot_cords$timepoint))
    }
    if(color_by=="challenge"){
      plot_cords$color=plot_cords$challenge_name
      color_pal <- setNames(unique(plot_cords$challenge_color), unique(plot_cords$challenge_name))
    }
    
    if(color_by=="default"){
      plot_cords$color="grey"
      color_pal <- "grey"
    }
    plot_cords$text=paste('</br> Subject: ', plot_cords$subject,
                          '</br> Time point: ', plot_cords$timepoint,
                          '</br> Challenge: ', plot_cords$challenge_name)
    
    if(type=="3D"){
      pca_p= plot_ly(x=plot_cords[[paste0("PC",use_PC[1])]], 
                     y=plot_cords[[paste0("PC",use_PC[3])]], 
                     z=plot_cords[[paste0("PC",use_PC[2])]], 
                     type="scatter3d",
                     mode="markers",
                     marker=list(size=marker_size),
                     color=plot_cords$color,
                     colors=color_pal,
                     hoverinfo = 'text',
                     text=plot_cords$text)%>%
        layout(
          scene=list(
            xaxis = list(title=paste("PC 1 (",round(data$importance$exp.var[use_PC[1]],2),"%)",sep="")), 
            yaxis = list(title=paste("PC 3 (",round(data$importance$exp.var[use_PC[3]],2),"%)",sep="")),
            zaxis = list (title=paste("PC 2 (",round(data$importance$exp.var[use_PC[2]],2),"%)",sep=""))
          ),
          legend = list(orientation = 'h')
        )%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("orbitRotation","tableRotation","resetCameraDefault3d","resetCameraLastSave3d","hoverClosest3d"))%>%
        htmlwidgets::onRender("function(el, x) {Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')}")
    }
    if(type=="2D"){
      pca_p= plot_ly(x=plot_cords[[paste0("PC",use_PC[1])]], 
                     y=plot_cords[[paste0("PC",use_PC[2])]], 
                     type="scatter",
                     mode="markers",
                     marker=list(size=marker_size),
                     color=plot_cords$color,
                     colors=color_pal,
                     hoverinfo = 'text',
                     text=plot_cords$text)%>%
        layout(
          scene=list(
            xaxis = list(title=paste("PC 1 (",round(data$importance$exp.var[use_PC[1]],2),"%)",sep="")), 
            yaxis = list (title=paste("PC 2 (",round(data$importance$exp.var[use_PC[2]],2),"%)",sep=""))
          ),
          legend = list(orientation = 'h')
        )%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("sendDataToCloud", "zoomOut2d","zoomIn2d","toggleSpikelines","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian","select2d","lasso2d"))%>%
        htmlwidgets::onRender("function(el, x) {Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')}")
    }
    
  } # end by sample
  return(pca_p)
}

## Plot PCA traces
huStatistics_plotTraces= function(.plot, data, trace=F, use_PC=c(1:3),tp_start=NULL, tp_end=NULL){
  p=.plot
  if(trace==T & !is.null(tp_start) & !is.null(tp_end)){
    if(length(use_PC)>2) warning("hu_statistics_addTrace(): can only handle a max of 2 PCs")
    dataDF=cbind(as.data.frame(data[["data"]]$x[,use_PC], stringsAsFactors = F), data[["sampleInfo"]])
    
    dataDF$arrow.x.end<-NA # tp_end of PC1
    dataDF$arrow.y.end<-NA # tp_end of PC2
    dataDF$arrow.x.start<-NA #tp_start of PC1
    dataDF$arrow.y.start<-NA #tp_start pf PC2
    for(i in 1:nrow(dataDF)){
      if(dataDF$timepoint[i]%in% c(tp_start,tp_end)){
        dataDF$arrow.x.end[i]<-dataDF[[1]][which(dataDF[["timepoint"]]==tp_end & dataDF[["subject"]]==dataDF[["subject"]][i])]
        dataDF$arrow.y.end[i]<-dataDF[[2]][which(dataDF[["timepoint"]]==tp_end & dataDF[["subject"]]==dataDF[["subject"]][i])]
        dataDF$arrow.x.start[i]<-dataDF[[1]][which(dataDF[["timepoint"]]==tp_start & dataDF[["subject"]]==dataDF[["subject"]][i])]
        dataDF$arrow.y.start[i]<-dataDF[[2]][which(dataDF[["timepoint"]]==tp_start & dataDF[["subject"]]==dataDF[["subject"]][i])]
      }
    }
    
    p=p%>%add_annotations( x = dataDF$arrow.x.end,
                           y = dataDF$arrow.y.end,
                           xref = "x", yref = "y",
                           axref = "x", ayref = "y",
                           text = "",
                           showarrow = T,
                           ax = dataDF$arrow.x.start,
                           ay = dataDF$arrow.y.start)
  }
  return(p)
}

#### T test functions ####

## Calculate t tests single metabolites old
# statistic_ttest_filter_single<-function(data, cutoff_type=NULL, add_timepoints=NULL, met_id="all"){
# 
# 
#   super_color=options[["general"]]$super_pathway
#   super_color=super_color[,c("super_pathway","super_color")]
#   colnames(super_color)=c("SUPER.PATHWAY","super.color")
#   
#   #platform_color=options[["network"]][["info_platform"]]
#   #colnames(platform_color)=c("Platform","platform_id","platform.color","platform.color.background")
# 
#   platform_color=options[["general"]]$platform
#   platform_color=platform_color[,c("platform_id","platform_color")]
#   colnames(platform_color)=c("Platform","platform.color")
#   
#   if(is.null(data) | is.null(cutoff_type) | is.null(add_timepoints)) stop("statistic_ttest_filter() evaluate input")
#   if(!cutoff_type %in% c("bonferroni","fdr","none")) stop("statistic_ttest_filter() could not find cutoff_type")
# 
#   
#   if(any(met_id %in% info_met$ID)){
#     my_data = data %>%
#       filter(TP %in% add_timepoints, ID %in% met_id)
#     my_data$qValue = p.adjust(my_data$pValue, method="fdr") # adjust fdr
#     bonferroni_threshold=0.05/nrow(my_data)
#     
#     # filter with metabolite id
#     if(cutoff_type=="bonferroni" & length(met_id)>=1){
#       my_data = my_data %>%
#         filter(pValue <= bonferroni_threshold)
#     }
#     else if (cutoff_type=="fdr" & length(met_id)>=1){
#       if(any(my_data$qValue)<=0.05) my_data = my_data %>%filter(qValue <= 0.05)
#       else{my_data=my_data[0,]}
#     }
#     else if (cutoff_type=="none" & length(met_id)>=1){
#       my_data = my_data
#     }
#     else{
#       my_data=my_data[0,]
#     }
#   }
# 
#   
#   my_data<-merge(my_data, info_met[,c("ID","SUPER.PATHWAY","SUB.PATHWAY")], by="ID", all.x=T)
#     if(length(is.na(my_data$SUB.PATHWAY))>0){
#       my_data$SUB.PATHWAY[which(is.na(my_data$SUB.PATHWAY))]<-"Not available"
#     }
#     if(length(is.na(my_data$SUPER.PATHWAY))>0){
#       my_data$SUPER.PATHWAY[which(is.na(my_data$SUPER.PATHWAY))]<-"Not available"
#     }
#   if(nrow(my_data)>0){
#     my_data$text=paste("Metabolite: ", my_data$Metabolite, 
#                        '<br>Super-pathway:', my_data$SUPER.PATHWAY,
#                        '<br>Sub-pathway:', my_data$SUB.PATHWAY,
#                        '<br>p Value:', signif(my_data$pValue, digits = 3),
#                        '<br>log2 fc:', round(my_data$FoldchangeLog2,2))
#   }
#   else{
#     my_data$text=character()
#   }
#     my_data<-merge(my_data, super_color, by="SUPER.PATHWAY", all.x = T)
#     my_data<-merge(my_data, platform_color, by="Platform", all.x = T)
#   
#     
#     output=list(data=my_data, 
#                 info=list(bonferroni=bonferroni_threshold, 
#                           timepoints=add_timepoints, 
#                           cut_at=cutoff_type)
#     )
#     return(output)
# }

## Calculate t tests all metabolites old 
# 
# statistic_ttest_filter_all<-function(data, cutoff_type=NULL){
#   
#   if(is.null(data) | !cutoff_type%in%c("fdr","bonferroni","none")) stop("statistic_ttest_filter() evaluate input")
#   
#   my_data=data
#   bonferroni_threshold=0.05/nrow(my_data) # calculate bonferroni threshold
#   my_data$qValue = p.adjust(my_data$pValue, method="fdr") # adjust fdr
#   
#   if(cutoff_type=="bonferroni"){
#     my_data = my_data %>% filter(pValue <= bonferroni_threshold)
#   }
#   else if(cutoff_type=="fdr"){
#     my_data = my_data %>% filter(qValue <= 0.05)
#   }
# 
#   
#   super_color=options[["general"]]$super_pathway
#   super_color=super_color[,c("super_pathway","super_color")]
#   colnames(super_color)=c("SUPER.PATHWAY","super.color")
#   
#   
#   #platform_color=options[["network"]][["info_platform"]]
#   #colnames(platform_color)=c("Platform","platform_id","platform.color","platform.color.background")
#   
#   platform_color=options[["general"]]$platform
#   platform_color=platform_color[,c("platform_id","platform_color")]
#   colnames(platform_color)=c("Platform","platform.color")
#   
#   my_data<-merge(my_data, info_met[,c("ID","SUPER.PATHWAY","SUB.PATHWAY")], by="ID", all.x=T)
#   
#   if(length(is.na(my_data$SUB.PATHWAY))>0){
#     my_data$SUB.PATHWAY[which(is.na(my_data$SUB.PATHWAY))]<-"Not available"
#   }
#   if(length(is.na(my_data$SUPER.PATHWAY))>0){
#     my_data$SUPER.PATHWAY[which(is.na(my_data$SUPER.PATHWAY))]<-"Not available"
#   }
#   my_data$text=paste("Metabolite: ", my_data$Metabolite, 
#                      '<br>Super-pathway:', my_data$SUPER.PATHWAY,
#                      '<br>Sub-pathway:', my_data$SUB.PATHWAY,
#                      '<br>p Value:', signif(my_data$pValue, digits = 3),
#                      '<br>log2 fc:', round(my_data$FoldchangeLog2,2))
#   
#   my_data<-merge(my_data, super_color, by="SUPER.PATHWAY", all.x = T)
#   my_data<-merge(my_data, platform_color, by="Platform", all.x = T)
#   
#   output=list(data=my_data, 
#               info=list(bonferroni=bonferroni_threshold, 
#                         cut_at=cutoff_type)
#   )
#   return(output)
# }

## Filter data if all metabolites are used
# statistic_ttest_filter_data<-function(x.data, cutoff_type=c("bonferroni","fdr","none"), timepoint=NULL, met_id=NULL){
#   if(!cutoff_type %in% c("bonferroni","fdr","none") | length(cutoff_type)!=1) stop("statistic_ttest_filter_data() could not match cutoff_type")
#   if(is.null(x.data) | is.null(cutoff_type) | is.null(timepoint)) stop("statistic_ttest_filter_data() evaluate input")
#   if(!any(timepoint %in% x.data$TP)) stop("statistic_ttest_filter_data() error in time point matching")
#   
#   super_color=options[["general"]][["super_pathway"]] %>% select(super_pathway, super_color) %>%  rename(SUPER.PATHWAY=super_pathway, super.color= super_color)
#   platform_color=options[["general"]][["platform"]] %>% select(platform_code, platform_color) %>% rename(platform.color= platform_color)
#   
#   if(is.null(met_id)){ 
#     my_data = x.data %>% filter(TP %in% timepoint)
#     my_data$qValue = p.adjust(my_data$pValue, method="fdr") # adjust fdr
#     bonferroni_threshold=0.05/nrow(my_data)
#     if(cutoff_type=="bonferroni") my_data = my_data %>% filter(pValue <= bonferroni_threshold)
#     else if (cutoff_type=="fdr") my_data = my_data %>%filter(qValue <= 0.05)
#     else if (cutoff_type=="none") my_data = my_data
#     else my_data=my_data[0,]
#   }
#   else if(any(met_id %in% info_met$ID)){
#     my_data = x.data %>% filter(TP %in% timepoint, ID %in% met_id)
#     my_data$qValue = p.adjust(my_data$pValue, method="fdr") # adjust fdr
#     bonferroni_threshold=0.05/nrow(my_data)
#     
#     # filter with metabolite id
#     if(cutoff_type=="bonferroni") my_data = my_data %>% filter(pValue <= bonferroni_threshold)
#     else if (cutoff_type=="fdr") my_data = my_data %>%filter(qValue <= 0.05)
#     else if (cutoff_type=="none") my_data = my_data
#     else my_data=my_data[0,]
#   }
#   my_data = merge(my_data, info_met[,c("ID","SUPER.PATHWAY","SUB.PATHWAY")], by="ID", all.x=T)
#   
#   if(nrow(my_data)>0) my_data$text=paste("Metabolite: ", my_data$Metabolite,'<br>Super-pathway:', my_data$SUPER.PATHWAY, '<br>Sub-pathway:', my_data$SUB.PATHWAY,'<br>p Value:', signif(my_data$pValue, digits = 3), '<br>log2 fc:', round(my_data$FoldchangeLog2,2))
#   else my_data$text=character()
#   
#   my_data<-merge(my_data, info_met[,c("ID","platform_code")], all.x = T, by="ID")
#   my_data<-merge(my_data, super_color, by="SUPER.PATHWAY", all.x = T)
#   my_data<-merge(my_data, platform_color, by="platform_code", all.x = T)
#   
#   
#   output=list(data=my_data, 
#               info=list(bonferroni=bonferroni_threshold, 
#                         timepoints=timepoint, 
#                         cut_at=cutoff_type))
#   return(output)
# }

## Calculate t tests per single metabolites
# statistic_ttest_calc<-function(data, met_id=NULL, add_timepoints=1:56){
#   
#   #process data
#   if(!is.null(data) | length(add_timepoints)>1){
#     data=data[which(data$Baseline==add_timepoints[1] & data$TP %in% add_timepoints & data$ID %in% met_id),]
#     bonferroni_threshold=0.05/nrow(data)
#     
#     #superColor=data.frame(SUPER.PATHWAY=c("Amino Acid", "Xenobiotics","Nucleotide","Lipid","Energy","Peptide","Carbohydrate","Cofactors and Vitamins",NA,"Not available", "Lipoprotein"),
#     #                      super.color=c("#E41A1C","#377EB8","#4DAF4A","orange","#A65628","#F781BF","#999999","#984EA3","#bfbfbf","grey", "blueviolet"),
#      #                     stringsAsFactors = F)
#     superColor=options[["general"]]$super_pathway
#     superColor=superColor[,c("super_pathway","super_color")]
#     colnames(superColor)=c("SUPER.PATHWAY","super.color")
#     
#     platformColor=data.frame(Platform=c("non-targeted LCMS", "MS Biocrates p150","Biochemistry","NMR","Lipidizer","PTRMS","ICR-FT-MS",NA),
#                              platform.color=c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#bfbfbf"),
#                              stringsAsFactors = F)
#     
#     data<-merge(data, info_met[,c("ID","SUPER.PATHWAY","SUB.PATHWAY")], by="ID", all.x=T)
#     if(length(is.na(data$SUB.PATHWAY))>0){
#       data$SUB.PATHWAY[which(is.na(data$SUB.PATHWAY))]<-"Not available"
#     }
#     if(length(is.na(data$SUPER.PATHWAY))>0){
#       data$SUPER.PATHWAY[which(is.na(data$SUPER.PATHWAY))]<-"Not available"
#     }
#     data$text=paste("Metabolite: ", data$Metabolite, 
#                     '<br>Super-pathway:', data$SUPER.PATHWAY,
#                     '<br>Sub-pathway:', data$SUB.PATHWAY,
#                     '<br>p Value:', signif(data$pValue, digits = 3),
#                     '<br>log2 fc:', round(data$FoldchangeLog2,2))
#     data<-merge(data, superColor, by="SUPER.PATHWAY", all.x = T)
#     data<-merge(data, platformColor, by="Platform", all.x = T)
#     output=list(data=data, 
#                 thresh=list(bonferroni=bonferroni_threshold, 
#                             timepoints=add_timepoints, 
#                             cut_at="none")
#     )
#     return(output)
#   }else{
#     warning("check function 'hu_statistic_tTest_single' ")
#   }
# }
# 
# 

statistics_relable<-function(data){
  data<-merge(data, info_met[,c("ID","platform_name")], by="ID")
  data$Platform=data$platform_name
  data$platform_name=NULL
  data$Fluid=ifelse(data$Fluid=="P","Plasma",
                    ifelse(data$Fluid=="U","Urine",
                           ifelse(data$Fluid=="BA","Breath air",
                                  ifelse(data$Fluid=="BC","Breath condensate",NA))))
    
  return(data)
}
