# network functions
## Author: Patrick Dreher
## Update: 28.05.2020


#### Network basic functions ####

# format title tooltip for visNetwork
network_title<-function(xnodes, time=F){
  if(all(c("Metabolite","SUB.PATHWAY","SUPER.PATHWAY","HMDB","PUBCHEM","KEGG","Platform") %in% colnames(xnodes))){
    if(time){
      xnodes$log2fc_corrected=xnodes$log2fc %>% signif(2)
      xnodes$pvalue_corrected=xnodes$pvalue %>% signif(2)
      xnodes$pvalue_corrected=ifelse(xnodes$pvalue_corrected<=0.001, paste0(xnodes$pvalue_corrected, " ***"),
                           ifelse(xnodes$pvalue_corrected<=0.01, paste0(xnodes$pvalue_corrected," *"),
                                  ifelse(xnodes$pvalue_corrected<=0.05, paste0(xnodes$pvalue_corrected," *"),xnodes$pvalue_corrected)))
    }
    
    for(i in 1:nrow(xnodes)){
      if(xnodes$label[i] %in% xnodes$labels & !time){
        xnodes$title[i] = paste(
          '<table>',
          '<tr>','<td>',"Metabolite:",'</td>','<td>',xnodes$Metabolite[i],'</td>','</tr>',
          '<tr>','<td>',"Super-pathway:",'</td>','<td>',xnodes$SUPER.PATHWAY[i],'</td>','</tr>',
          '<tr>','<td>',"Sub-pathway:",'</td>','<td>',xnodes$SUB.PATHWAY[i],'</td>','</tr>',
          #'<tr>','<td>',"HMDB:",'</td>','<td>', ifelse(is.na(xnodes$HMDB[i])," ",paste('<a target="_blank" href="http://www.hmdb.ca/metabolites/',xnodes$HMDB[i],'/">',xnodes$HMDB[i],'</a>', sep="")),'</td>','</tr>',
          #'<tr>','<td>',"PubChem:",'</td>','<td>', ifelse(is.na(xnodes$PUBCHEM[i])," ",paste('<a target="_blank" href="https://pubchem.ncbi.nlm.nih.gov/compound/',xnodes$PUBCHEM[i],'/">',xnodes$PUBCHEM[i],'</a>', sep="")),'</td>','</tr>',
          #'<tr>','<td>',"KEGG:",'</td>','<td>',ifelse(is.na(xnodes$KEGG[i])," ",paste('<a target="_blank" href="http://www.genome.jp/dbget-bin/www_bget?cpd:',xnodes$KEGG[i],'">',xnodes$KEGG[i],'</a>', sep="")),'</td>','</tr>',
          '<tr>','<td>',"ChEBI:",'</td>','<td>',rep_link_chebi(xnodes$chebi[i]),'</td>','</tr>',
          '<tr>','<td>',"Platform:",'</td>','<td>',xnodes$platform_name[i],'</td>','</tr>',
          '</table>'
        )
      }
      if(xnodes$label[i] %in% xnodes$labels & time){
        xnodes$title[i] = paste(
          '<table>',
          '<tr>','<td>',"Metabolite:",'</td>','<td>',xnodes$Metabolite[i],'</td>','</tr>',
          '<tr>','<td>',"Super-pathway:",'</td>','<td>',xnodes$SUPER.PATHWAY[i],'</td>','</tr>',
          '<tr>','<td>',"Sub-pathway:",'</td>','<td>',xnodes$SUB.PATHWAY[i],'</td>','</tr>',
          #'<tr>','<td>',"HMDB:",'</td>','<td>', ifelse(is.na(xnodes$HMDB[i])," ",paste('<a target="_blank" href="http://www.hmdb.ca/metabolites/',xnodes$HMDB[i],'/">',xnodes$HMDB[i],'</a>', sep="")),'</td>','</tr>',
          #'<tr>','<td>',"PubChem:",'</td>','<td>', ifelse(is.na(xnodes$PUBCHEM[i])," ",paste('<a target="_blank" href="https://pubchem.ncbi.nlm.nih.gov/compound/',xnodes$PUBCHEM[i],'/">',xnodes$PUBCHEM[i],'</a>', sep="")),'</td>','</tr>',
          #'<tr>','<td>',"KEGG:",'</td>','<td>',ifelse(is.na(xnodes$KEGG[i])," ",paste('<a target="_blank" href="http://www.genome.jp/dbget-bin/www_bget?cpd:',xnodes$KEGG[i],'">',xnodes$KEGG[i],'</a>', sep="")),'</td>','</tr>',
          '<tr>','<td>',"ChEBI:",'</td>','<td>',rep_link_chebi(xnodes$chebi[i]),'</td>','</tr>',
          '<tr>','<td>',"Platform:",'</td>','<td>',xnodes$platform_name[i],'</td>','</tr>',
          '<tr>','<td>',"p-value:",'</td>','<td>',xnodes$pvalue_corrected[i],'</td>','</tr>',
          '<tr>','<td>',"log2 fc:",'</td>','<td>',xnodes$log2fc_corrected[i],'</td>','</tr>',
          '<tr>','<td>',"time point:",'</td>','<td>',xnodes$time.point[i],'</td>','</tr>',
          '</table>'
        )
      }
      if(xnodes$label[i] %in% xnodes$SUPER.PATHWAY){
        xnodes$title[i] = paste(
          '<table>',
          '<tr>','<td>',"Super-pathway:",'</td>','<td>',xnodes$SUPER.PATHWAY[i],'</td>','</tr>',
          '<tr>','<td>',"Sub-pathway:",'</td>','<td>',"all",'</td>','</tr>',
          '</table>'
        )
      }
      if(xnodes$label[i] %in% xnodes$SUB.PATHWAY){
        xnodes$title[i] = paste(
          '<table>',
          '<tr>','<td>',"Super-pathway:",'</td>','<td>',xnodes$SUPER.PATHWAY[i],'</td>','</tr>',
          '<tr>','<td>',"Sub-pathway:",'</td>','<td>',xnodes$SUB.PATHWAY[i],'</td>','</tr>',
          '</table>'
        )
      }    
      if(xnodes$label[i] %in% c("metabolon plasma","metabolon urine","biocrates plasma")){
        xnodes$title[i] = NA
      }
    }
  }else{warning("annotate_network() could not overlap all necessary parameters")}
  return(xnodes)
}

# shape settings for visNetwork
network_shape<-function(xnodes){
  if(all(c("Metabolite","SUB.PATHWAY","SUPER.PATHWAY") %in% colnames(xnodes))){
    xnodes$shape="dot"
    xnodes$shape[ grep("U",xnodes$Fluid)]<- "square"
    for(i in which(xnodes$label %in% xnodes$SUB.PATHWAY)){
      xnodes$shape[i] = "triangle"
    }
    for(i in which(xnodes$label %in% xnodes$SUPER.PATHWAY)){
      xnodes$shape[i] = "ellipse"
    }
    for(i in which(xnodes$label %in% c("metabolon plasma","metabolon urine","biocrates plasma"))){
      xnodes$shape[i] = "box"
    }
    return(xnodes)
  }else{warning("network_shape() could not overlap all necessary parameters")}
}

# font settings for visNetwork
network_font<-function(xnodes){
  if(all(c("Metabolite","SUB.PATHWAY","SUPER.PATHWAY") %in% colnames(xnodes))){
    xnodes$font.size[i] = "30"
    xnodes$font.background[i] = "transparent"
    xnodes$font.color[i] = "black"
    xnodes$font.face[i] = "arial"
    for(i in which(xnodes$label %in% xnodes$SUB.PATHWAY)){
      #xnodes$font[i] = "10px arial black"
      xnodes$font.size[i] = "10"
      xnodes$font.background[i] = "transparent"
      xnodes$font.color[i] = "black"
      xnodes$font.face[i] = "arial"
      if("font" %in% names(xnodes)){
        xnodes$font=NULL
      }
    }
    for(i in which(xnodes$label %in% xnodes$SUPER.PATHWAY)){
      #xnodes$font[i] = "20px arial black"
      xnodes$font.size[i] = "20"
      xnodes$font.background[i] = "transparent"
      xnodes$font.color[i] = "black"
      xnodes$font.face[i] = "arial"
      if("font" %in% names(xnodes)){
        xnodes$font=NULL
      }
    }
    for(i in which(xnodes$label %in% c("metabolon plasma","metabolon urine","biocrates plasma"))){
      #xnodes$font[i] = "30px arial black"
      xnodes$font.size[i] = "30"
      xnodes$font.background[i] = "transparent"
      xnodes$font.color[i] = "black"
      xnodes$font.face[i] = "arial"
      if("font" %in% names(xnodes)){
        xnodes$font=NULL
      }
    }
  }else{warning("network_font() could not overlap all necessary parameters")}
  return(xnodes)
}


# color settings for visNetwork

network_color<-function(xnodes){
  xnodes$color.background="white"
  xnodes$color.border="black"
  xnodes$color.highlight.background="lightgrey"
  xnodes$color.highlight.border="#ffd500"
  xnodes$font.color="black"
  xnodes$borderWidth=1
  return(xnodes)
}


# size settings for visNetwork
network_color_converter<-function(fc){
  if(!"options"%in%names(db_network)) stop("network_color_converter(): could not find db_network$options[['fcColor']]")
  outColor=lapply(fc,function(fc) 
    db_network$options$fcColor$color[which(abs(db_network$options$fcColor$seq-fc)==min(abs(db_network$options$fcColor$seq-fc)))])
  outColor=outColor%>%as.character()
  return(outColor)
}

network_color_proxy<-function(nodes, color=c("super","platform"),selected=NULL){
  nodes<-nodes[,c("id","label","Platform","SUPER.PATHWAY","font.size","font.background")]
  if(color=="super" & "info_super_pathway" %in% names(options$network)){
    superInfo=options$network$info_super_pathway
    nodes=merge(nodes, superInfo, by="SUPER.PATHWAY", all.x=T)
    nodes$color.background=ifelse(nodes$SUPER.PATHWAY%in% selected, nodes$color.background, "white")
    nodes$color.highlight.background=ifelse(nodes$SUPER.PATHWAY%in% selected, nodes$color.highlight.background, "gainsboro")
  }
  
  if(color=="platform"){
    platformInfo=options$network$info_platform
    nodes=merge(nodes, platformInfo, by="Platform", all.x=T)
    nodes$color.background=ifelse(nodes$platform_id %in% selected, nodes$color.background, "white") 
    nodes$color.highlight.background=ifelse(nodes$platform_id%in% selected, nodes$color.highlight.background, "grey")
  }
  return(nodes)
}
network_color_super_proxy<-function(nodes,selected=NULL){
    superInfo=options$network$info_super_pathway
    nodes$color.background=NULL
    nodes$color.highlight.background=NULL
    nodes=merge(nodes, superInfo, by="SUPER.PATHWAY", all.x=T)
    
    nodes_selected=nodes$SUPER.PATHWAY %in% selected
    nodes$color.background=ifelse(nodes_selected, nodes$color.background, "white") 
    
  return(nodes)
}

network_color_platform_proxy<-function(nodes,selected=NULL){
  nodes_out<-nodes %>% 
    dplyr::select(-any_of(c("color.background","color.highlight.background", "platform_id"))) %>%
    dplyr::left_join(options$network$info_platform, by="Platform") #%>% 
    #dplyr::mutate(color.background=ifelse(platform_id %in% selected, nodes_color$color.background,"white"))
  # 
  # platformInfo=options$network$info_platform
  # nodes_color=merge(nodes_color, platformInfo, by="Platform", all.x=T)
  # 
  # nodes_selected=nodes_color$platform_id %in% selected
  # nodes$color.background=ifelse(nodes_selected, nodes_color$color.background, "white") 
  return(nodes_out)
}
network_color_time_proxy<-function(xnodes, highlightTime,node_expansion=15, node_min_size=30){
  if(!"data" %in% names(db_network)) stop("network_color_time_proxy() could not find data")

  ## filter data to list
  metId=xnodes$id
  nodeData=lapply(metId, function(x) db_network$data[[x]][which(db_network$data[[x]]$network_timepoint==highlightTime),])
  names(nodeData)=xnodes$id
  nodeData=do.call(rbind, nodeData)
  nodesOut<-data.frame(id=rownames(nodeData),
                       color.background=network_color_converter(nodeData$log2foldchange),
                       size=ifelse(nodeData$pvalue<0.05,-log10(nodeData$pvalue)*node_expansion,0),
                       pvalue=nodeData$pvalue,
                       log2fc=nodeData$log2foldchange,
                       'time point'=paste0(nodeData$challenge_timepoint," vs. ",nodeData$baseline_timepoint),
                       stringsAsFactors = F)
  
  ## scale nodes
  if(min(nodesOut$size, na.rm = T)<20){
    nodesOut$size<-nodesOut$size+(node_min_size-min(nodesOut$size, na.rm = T))
  }
  xnodes$color.background=NULL
  xnodes$size=NULL
  xnodes$log2fc=NULL
  xnodes$pvalue=NULL
  xnodes$time.point=NULL
  xnodes=merge(xnodes, nodesOut, by="id",all.x = T)
  xnodes$color.background=ifelse(is.na(xnodes$color.background),"white",xnodes$color.background)
  return(xnodes)
}

network_subtitle_proxy<-function(timepoint){
  if(!exists("info_network")) stop("network_subtitle_proxy(): could not find info_network")
  if(!timepoint%in%info_network$timepoint_code) stop("network_subtitle_proxy(): could not match time point")
  nr<-which(info_network$timepoint_code==timepoint)
  
  subTitle<-paste0("Challenge:", info_network$challenge_time[nr],
                   "; Day:",
                   info_network$day[nr],
                   "; Time:",
                   info_network$day_time[nr],
                   "; Time Point:",info_network$timepoint[nr],";")
  return(subTitle)
}

network_size<-function(xnodes, size=30){
  xnodes$size=size
  return(xnodes)
}

# network resizing settings for visNetwork
network_resize<-function(xnodes){
  #strech network to x= -4 - 4 ; y= -3 - 3
  if(all(c("x","y") %in% colnames(xnodes))){
    rescale <- function(x) (x-min(x))/(max(x) - min(x))
    xnodes$x=(rescale(xnodes$x)*8)-4
    xnodes$y=(rescale(xnodes$y)*6)-3
  }else{warning("network_resize() could not overlap all necessary parameters")}
  return(xnodes)
}

# network highlight metabolites
network_highlight<-function(xnodes, met_id, highlight=T){
  if(highlight){
    color_mets=which(xnodes$id %in% met_id)
    xnodes$font.background="transparent"
    xnodes$font.size=30
    xnodes$font.size[color_mets]=50
    xnodes$font.background[color_mets]="#ffd500"
    xnodes$color.border[color_mets]="#ffd500"
    xnodes$borderWidth=1
    xnodes$borderWidth[color_mets]=3 
  }else{
    xnodes$font.background="white"
    xnodes$font.size=30
    xnodes$color.border="black"
    xnodes$borderWidth=1
  }
  return(xnodes)
}

network_filter_singles<-function(network){
    nodesInEdge<-unique(c(network$x$edges$from, network$x$edges$to))
    network$x$nodes<-network$x$nodes[which(network$x$nodes$id %in% nodesInEdge),]
  return(network)
}


# network titles


network_add_title=function(network, title){
  network$x$main=list(
    text=as.character(title),
    style="font-family:Arial, Times New Roman, Times, serif;font-weight:bold;font-size:24px;text-align:center;"
  )
  return(network)
  
}

network_add_subtitle=function(network, subtitle){
  network$x$submain=list(
    text=as.character(subtitle),
    style="font-family:Arial, Times New Roman, Times, serif;font-size:16px;text-align:center;"
  )
  return(network)
}
