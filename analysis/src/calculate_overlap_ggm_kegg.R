# Script that calculates overlap between GGM and KEGG edges
# Matched by KEGG pathways


# Load necessary library
library(igraph)
library(tidyverse)

# load necessary data

# Load metabolite annotation table
info_met<-base::readRDS("app/data/info/met.rds")


## Load KEGG edges 
kegg_db <- readRDS(file="app/data/database/kegg.rds")

kegg_edges <- lapply(seq_along(kegg_db), function(x) kegg_db[[x]]$edges %>% as.data.frame) %>% 
  plyr::rbind.fill() %>% 
  dplyr::select(from,to)


## Load GGM edges
ggm_db <- readRDS("~/humet_repository/humet/app/data/network/backbone.rds")

## plasma edges
ggm_edges_plasma <- ggm_db[["[P,nt-ms]_Partial corrrelation 0.120"]][["network"]][["x"]][["edges"]] %>% 
  # add KEGG id to from node
  dplyr::left_join(info_met[,c("ID","KEGG")],by=c("from"="ID")) %>% 
  # add KEGG id to to node
  dplyr::left_join(info_met[,c("ID","KEGG")],by=c("to"="ID"),suffix=c("_from","_to")) %>%
  # remove all rows with Nodes that can't be annotated
  #dplyr::filter(!is.na(KEGG_from) & !is.na(KEGG_to)) %>% 
  dplyr::select(KEGG_from,KEGG_to)

ggm_edges_urine <- ggm_db[["[U,nt-ms]_Partial corrrelation 0.080"]][["network"]][["x"]][["edges"]] %>% 
  # add KEGG id to from node
  dplyr::left_join(info_met[,c("ID","KEGG")],by=c("from"="ID")) %>% 
  # add KEGG id to to node
  dplyr::left_join(info_met[,c("ID","KEGG")],by=c("to"="ID"),suffix=c("_from","_to")) %>%
  # remove all rows with Nodes that can't be annotated
  #dplyr::filter(!is.na(KEGG_from) & !is.na(KEGG_to)) %>% 
  dplyr::select(KEGG_from,KEGG_to)

# calculate the overlap of ggm edges and kegg edges
calc_distance <- function(test_edges,network_edges){
  # create network as a graph
  network_graph <- graph_from_edgelist(as.matrix(network_edges), directed = FALSE)
  
  distance <- lapply(1:nrow(test_edges), function(x){
    tmp_test <- test_edges[x,] %>% as.character()
    tmp_edges <- test_edges[x,]
    
    # NA_1: if one metabolite of the edge is not mappable to KEGG
    if(length(which(is.na(tmp_test)))==1){ tmp_edges$distance = "NA_1"}
    
    # NA_2: if both metabolite of the edge were not mappable to KEGG
    else if(length(which(is.na(tmp_test)))==2){tmp_edges$distance = "NA_2"}
    
    #NA_0: if both metabolites of the edge were mapped to KEGG metabolites but at least one of them does not occur in its human pathway map
    else if(!all(as.character(tmp_edges) %in% c(network_edges$from, network_edges$to))){
      tmp_edges$distance <- "NA_0"
    }
    
    # Calculate overlap of fully mapped KEGG metabolites
    else{
      tmp_edges$distance <- igraph::distances(network_graph, v = tmp_edges$KEGG_from, to = tmp_edges$KEGG_to, mode = "all")
    }
    #rename inf to undefined: if both metabolites of the edge were mapped to KEGG metabolites and occur in its human pathway map but are not linked through any route
    tmp_edges$distance <- ifelse(is.infinite(tmp_edges$distance), "undefined",tmp_edges$distance)
    tmp_edges$distance
  }) %>% 
    unlist()
  
  out <- test_edges %>% 
    dplyr::mutate(distance=distance)
  return(out)
}

# Provide summary table of metabolon plasma
## undefined: if both metabolites of the edge were mapped to KEGG metabolites and occur in its human pathway map but are not linked through any route
## NA_0: if both metabolites of the edge were mapped to KEGG metabolites but at least one of them does not occur in its human pathway map
## NA_1: if one metabolite of the edge was not mappable to KEGG
## NA_2: if both metabolite of the edge were not mappable to KEGG
summary_distance <- calc_distance(test_edges=ggm_edges_plasma,network_edges=kegg_edges)

summary_distance %>% 
  count(distance)

# Provide summary table of metabolon urine
## undefined: if both metabolites of the edge were mapped to KEGG metabolites and occur in its human pathway map but are not linked through any route
## NA_0: if both metabolites of the edge were mapped to KEGG metabolites but at least one of them does not occur in its human pathway map
## NA_1: if one metabolite of the edge was not mappable to KEGG
## NA_2: if both metabolite of the edge were not mappable to KEGG
summary_distance <- calc_distance(test_edges=ggm_edges_urine,network_edges=kegg_edges)

summary_distance %>% 
  count(distance)


# get info on distance 0
## This edge connects 1- and 3-methylhistidine, which are wrongly represented by the same ID in KEGG
summary_distance %>% 
  dplyr::filter(distance==0) %>% 
  dplyr::left_join(info_met[,c("KEGG","Metabolite")],by=c("KEGG_from"="KEGG"))%>% 
  dplyr::left_join(info_met[,c("KEGG","Metabolite")],by=c("KEGG_to"="KEGG"),suffix=c("_from","_to")) %>% 
  distinct()

# Bootstrapping approach with 1000 networks generated with same topology but random node labelling

## Plasma network
summary_bootstrap_plasma <- lapply(1:1000, function(y){
  # get edges list with randomized nodes 
  bootstrap_edges <- ggm_edges_plasma%>% 
    dplyr::mutate(KEGG_from = sample(KEGG_from), KEGG_to=sample(KEGG_to))
  
  # calculate distance between runs
  calc_distance(test_edges=bootstrap_edges, 
                network_edges = kegg_edges) %>% 
    ## filter to distance = 1 to see direct neighbors
    dplyr::filter(distance==1) %>% 
    ## add number of runs
    dplyr::mutate(sample=y)
}) %>%
  plyr::rbind.fill() %>% 
  dplyr::filter(distance==1) %>% 
  dplyr::count(sample) %>% 
  dplyr::mutate(distance = 1)  %>% 
  dplyr::select(distance,n, sample)  %>% 
  dplyr::rename("Pathway distance"="distance", "# edges"="n")
  
## Urine network
summary_bootstrap_urine <- lapply(1:1000, function(y){
  # get edges list with randomized nodes 
  bootstrap_edges <- ggm_edges_urine%>% 
    dplyr::mutate(KEGG_from = sample(KEGG_from), KEGG_to=sample(KEGG_to))
  
  # calculate distance between runs
  calc_distance(test_edges=bootstrap_edges, 
                network_edges = kegg_edges) %>% 
    ## filter to distance = 1 to see direct neighbors
    dplyr::filter(distance==1) %>% 
    ## add number of runs
    dplyr::mutate(sample=y)
}) %>%
  plyr::rbind.fill() %>% 
  dplyr::filter(distance==1) %>% 
  dplyr::count(sample) %>% 
  dplyr::mutate(distance = 1)  %>% 
  dplyr::select(distance,n, sample)  %>% 
  dplyr::rename("Pathway distance"="distance", "# edges"="n")

