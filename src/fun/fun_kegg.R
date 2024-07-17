## kegg network ##

#has to be updated!

network_kegg_node_info<-function(pathwayInfo, matchingTable=NULL,fluid="P", platform="non-targeted LCMS"){
  if(is.null(matchingTable)){
    warning("add matching table")
  }else{
    pathwayNodes=as.data.frame(pathwayInfo$nodes, stringsAsFactors = F)
    pathwayNodes$x=as.numeric(pathwayNodes$x)
    pathwayNodes$y=as.numeric(pathwayNodes$y)
    pathwayNodes=merge(pathwayNodes, matchingTable[which(matchingTable$Fluid==fluid&matchingTable$Platform==platform),c("Metabolite","ID", "Fluid","Platform", "SUPER.PATHWAY","SUB.PATHWAY", "KEGG", "PUBCHEM","KEGG","HMDB", "labels")],
                       by.x="id",by.y="KEGG", all.x=T)
    pathwayNodes$shapeProperties.borderDashes=ifelse(!is.na(pathwayNodes$labels), F, T)
    pathwayNodes$label=ifelse(!is.na(pathwayNodes$labels), pathwayNodes$labels, pathwayNodes$label)
    return(pathwayNodes)
  }
}


#plot kegg network
network_kegg_create_network<-function(pathwayInfo, fluid="P", platform="non-targeted LCMS", nodeSize=20){
  nodes=network_kegg_node_info(pathwayInfo,matchingTable=matchingTable)
  nodes$x=nodes$x*2
  if(T %in%duplicated(nodes$id)){
    for(i in which(duplicated(nodes$id))){
      nodes$id[i]=paste0(nodes$id[i],"_1")
    }
  }
  edges=as.data.frame(pathwayInfo$edges, stringsAsFactors = F)
  
  net<-visNetwork(nodes=nodes,edges=edges,height=600)%>%
    visNodes(shape="dot",
             physics=F,
             color=list(
               background="Gainsboro",
               border="black",
               highlight=list(
                 background="Gainsboro",
                 border="black"
               )
             ),
             borderWidth=3,
             size=nodeSize)%>%
    visPhysics(enabled=FALSE)%>%
    visEdges(arrows="to", smooth=list(enabled=F))
  return(net)
}
