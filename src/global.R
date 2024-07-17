# Global loading script
# Author: Patrick Weinisch
# Last change: 19.01.2024

# Information: Script loads all necessary data and functions that were preformatted for the HuMet Repository app.

# libraries --------------------------------------------------------------------
## load all necessary r packages
lapply(c("shinydashboard","shinyWidgets","shiny","shinyBS","bsplus","shinyalert",   "shinydashboardPlus","shinycssloaders","shinyjs",
         "dplyr","doParallel","foreach","readxl","writexl","reshape2","plyr","openxlsx","readr",
         "visNetwork","plotly","highcharter","igraph", 
         "DT","tableHTML",
         "colorRamps", 
         "dtw", "longitudinalData","kmlShape"
         ), 
       FUN=require, character.only = TRUE)

## register cores for parallel computing
registerDoParallel(cores=detectCores())

# functions general -----
## load all functions that are necessary for general usage
source("src/fun/fun_repository.R")

# data ----

## info data ----
### load data from the MetOntology. This file is a direct export file from the MetOntology Neo4j database.
info_metontology <- readr::read_csv("data/info/met_ontology.csv",show_col_types = FALSE) %>% 
  dplyr::mutate(biochemical = biochemical %>% gsub(pattern='\"',replacement="", fixed=T),
                id = id %>% gsub(pattern="[^0-9]",replacement=""),
                metabolite = metabolite %>% gsub(pattern="^\"|\"$",replacement=""),
                fluid = fluid %>% gsub(pattern="^\"|\"$",replacement=""),
                platform_name = platform_name %>% gsub(pattern="^\"|\"$",replacement=""),
                IUPAC = IUPAC %>% gsub(pattern="\\[|\\]|", replacement="",fixed=F) %>% gsub(pattern="[\\\\\"]", replacement="",fixed=F),
                chebi_name = chebi_name %>% gsub(pattern="\\[|\\]|", replacement="",fixed=F) %>% gsub(pattern="[\\\\\"]", replacement="",fixed=F),
                chebi = chebi %>% gsub(pattern="\\[|\\]|", replacement="",fixed=F) %>% gsub(pattern="[\\\\\"]", replacement="",fixed=F),
                InchiKey = InchiKey %>% gsub(pattern="\\[|\\]|", replacement="",fixed=F) %>% gsub(pattern="[\\\\\"]", replacement="",fixed=F),
                pubchem = pubchem %>% gsub(pattern="\\[|\\]|", replacement="",fixed=F) %>% gsub(pattern="[\\\\\"]", replacement="",fixed=F),
                chebi_synonyms = chebi_synonyms %>% gsub(pattern="\\[|\\]|", replacement="",fixed=F) %>% gsub(pattern="[\\\\\"]", replacement="",fixed=F),
                swisslipids_synonyms = swisslipids_synonyms %>% gsub(pattern="\\[|\\]|", replacement="",fixed=F) %>% gsub(pattern="[\\\\\"]", replacement="",fixed=F),
                lipidmaps_synonyms = lipidmaps_synonyms  %>% gsub(pattern="\\[|\\]|", replacement="",fixed=F) %>% gsub(pattern="[\\\\\"]", replacement="",fixed=F)
                ) %>% 
  dplyr::mutate(fluid= fluid %>% gsub(pattern="breath air",replacement="BA")) %>% 
  dplyr::mutate(fluid= fluid %>% gsub(pattern="breath condensate",replacement="BC")) %>% 
  dplyr::mutate(fluid= fluid %>% gsub(pattern="plasma",replacement="P")) %>% 
  dplyr::mutate(fluid= fluid %>% gsub(pattern="urine",replacement="U")) %>% 
  dplyr::mutate(synonym = paste0(chebi_synonyms,chebi_name,",",swisslipids_synonyms,",",lipidmaps_synonyms,IUPAC)) %>% 
  dplyr::select(metabolite, platform_name,fluid,synonym, chebi,chebi_name,chebi_synonyms,swisslipids_synonyms,lipidmaps_synonyms,IUPAC,pubchem,InchiKey) %>% 
  dplyr::distinct()

### info_met includes all necesary information that is used to append tables, networks (i.e. adding the ChEBI information)
info_met<-base::readRDS("data/info/met.rds") %>% 
  dplyr::left_join(info_metontology, by=c("platform_name"="platform_name","Metabolite"="metabolite", "Fluid"="fluid"))

### info_sample includes all information on the samples (i.e. which challenge, timepoint, block ... a sample is attributed to)
info_sample<-base::readRDS("data/info/sample.rds")

## info_network: all information needed for the networks. (i.e. coloring and time points)
info_network<- base::readRDS("data/info/network.rds") # network information


## network data -----

## load network data. necessary for stable pre-computed x and y coordinates of nodes in the network
db_network=base::readRDS("data/network/backbone.rds") 

 ## append networks with info_metabolites
info_met_network<- info_met[,c("ID","SUPER.PATHWAY","SUB.PATHWAY","labels","chebi","chebi_synonyms","swisslipids_synonyms","lipidmaps_synonyms","IUPAC")] %>% 
  plyr::rbind.fill(info_met %>% dplyr::distinct(SUPER.PATHWAY) %>% dplyr::mutate(ID = SUPER.PATHWAY,labels = SUPER.PATHWAY,SUB.PATHWAY = SUPER.PATHWAY),
                   info_met %>% dplyr::distinct(SUPER.PATHWAY,SUB.PATHWAY) %>% dplyr::mutate(ID = SUB.PATHWAY,labels = SUB.PATHWAY),
                   data.frame(labels = c("Metabolon plasma","Metabolon urine","Biocrates p150"),
                              ID = c("Metabolon plasma","Metabolon urine","Biocrates p150"),
                              SUPER.PATHWAY = "Platform", SUB.PATHWAY="Platform"),
                   data.frame(labels = c("Fatty Acid Metabolism (Acyl Carnitine)","Fatty Acid Metabolism (Acyl Glycine)"),
                              ID = c("Fatty Acid Metabolism(Acyl Carnitine)","Fatty Acid Metabolism(Acyl Glycine)"),
                              SUPER.PATHWAY = "Lipids", 
                              SUB.PATHWAY = c("Fatty Acid Metabolism (Acyl Carnitine)","Fatty Acid Metabolism (Acyl Glycine)"))
                   )

db_network <- lapply(names(db_network),function(x){
  # only change for networks
  if("network" %in% names(db_network[[x]])){
    out <- db_network[[x]]
    out$network$x$nodes <- out$network$x$nodes%>% 
      dplyr::select(-any_of(c("SUPER.PATHWAY","label", "SUB.PATHWAY","labels","chebi","chebi_synonyms","swisslipids_synonyms",
                              "lipidmaps_synonyms","IUPAC"))) %>% #make sure that network does not contain any of the relevant columns
      dplyr::left_join(info_met_network,by=c("id"="ID")) %>% 
      dplyr::mutate(label=labels)  #use labels as label for each node
  }else{
    out <- db_network[[x]]
  }
  out
}) %>% 
  setNames(names(db_network))

# add annotation to networks
db_network[["selected"]]<-db_network$`[P,nt-ms]_Partial corrrelation 0.120` # standard network
db_network$data=base::readRDS("data/network/data.rds") # animation data

# load metabolomics data ---
db_data<-list()

db_data[["dist_norm_imp"]]<-readRDS("data/measurements/df_rf_raw.rds") %>% 
  dplyr::select(code, timepoint, subject, setdiff(names(.[]), c("code","timepoint","subject"))) %>% 
  dplyr::mutate(timepoint=as.numeric(timepoint), subject=as.numeric(subject))
db_data[["dist_norm_imp"]][,-c(1:3)] = db_data[["dist_norm_imp"]][,-c(1:3)] %>% scale(center = TRUE, scale = TRUE) %>% as.data.frame()

db_data[["dist_norm"]]<-readRDS("data/measurements/df_none_raw.rds") %>% 
  dplyr::select(code, timepoint, subject, setdiff(names(.[]), c("code","timepoint","subject"))) %>% 
  dplyr::mutate(timepoint=as.numeric(timepoint), subject=as.numeric(subject))
db_data[["dist_norm"]][,-c(1:3)] =db_data[["dist_norm"]][,-c(1:3)] %>% scale(center = TRUE, scale = TRUE) %>% as.data.frame()


db_data[["raw_df"]] <- readRDS("data/measurements/df_none_raw.rds")
db_data[["imp_df"]] <- readRDS("data/measurements/df_rf_raw.rds")

db_data[["raw_list"]] <- readRDS("data/measurements/list_none_raw.rds")
db_data[["imp_list"]] <- readRDS("data/measurements/list_rf_raw.rds")

# Statistics database #

source("src/showcases/showcase_washout.R")
source("src/showcases/showcase_networks.R")
source("src/showcases/showcase_platforms.R")

# Functions ----------------------------------------------------------------------

# funcitions UI
source("src/fun/fun_ui.R")


## add ui functions

source("src/repo_ui/mt_header.R")
source("src/repo_ui/mt_showcase.R")
source("src/repo_ui/mt_flow.R")
source("src/repo_ui/mt_submenu.R")


## add html elements
source("src/repo_ui/mt_btn.R")
source("src/repo_ui/mt_box.R")


## humet ------------------------
#### General helpers ####
source("src/fun/fun_help.R")


####  Selection module ####
source("src/fun/fun_selection.R")

####  Browser module ####
source("src/fun/fun_browser.R") # new, checked

####  Network module #### 
source("src/fun/fun_network.R")
#source("app/functions/functions_humetNetwork.R") # old -> integrate into new

####  Statistics module #### 
source("src/fun/fun_ttest.R")

source("src/fun/fun_statistics.R") # old -> split to modules

#### Downloads ####
source("src/fun/fun_download.R")

#### Showcase ####
source("src/fun/fun_showcase.R")

####  About ####
source("src/fun/fun_about.R")

#### Database functions ####
source("src/fun/fun_kegg.R")


# load ui elements -----

source("src/repo_ui/repo_header.R")


# Options ----

options = list()

## General --------------------------------------------
options[["general"]]=list()
options[["general"]][["platform"]] <- readxl::read_xlsx("data/options/platform.xlsx") %>% as.data.frame() # load information on platforms
options[["general"]][["platform_choices"]]=list("Metabolon HD4 [nt-ms]"="[P, nt-ms]","Metabolon HD4 [nt-ms]"="[U, nt-ms]","Lipidyzer [nt-ms]"="[P, Lipidyzer]","Biocrates p150 [t-ms]"="[P, t-ms]","numares (Lipofit) [NMR]"="[P, NMR]","In-house biochemistry [chem.]" = "[P, chem.]","Chenomx [NMR]"="[U, NMR]", "In-house PTR-MS [PTRMS]"="[BA, PTRMS]","In-house FTICR-MS [ICR]"="[BC, ICR]")
options[["general"]][["super_pathway"]]<- readxl::read_xlsx("data/options/super_pathway.xlsx") %>% as.data.frame() # load information on super pathways
options[["general"]][["subject"]]<-readxl::read_xlsx("data/options/subject.xlsx") %>% as.data.frame() # load information on subjects
options[["general"]][["subject"]]$subject_number= c("01","02","03","04","05","06","07","08","09","10","11","12","13","14","15")

options[["general"]][["colors"]] <- list(
  text_highlight="#b98474", # brown
  link_highlight="#46b5ef", # lightblue
  red_highlight="#d2310b", # bright red
  content_background="fff5ef", # ghostwhite
  text_highlight2= "#ffa471", # lightbrown
  header_background="#3d485d" # grey
)

## browser ---------------------------------------------
### Highcharts theme
options[["browser"]]=list(
  hc_theme=hc_theme_economist()
)
options[["browser"]]$hc_theme$chart$backgroundColor <- "white"
options[["browser"]]$hc_theme$chart$plotBackgroundColor <- "white"
options[["browser"]]$hc_theme$tooltip$backgroundColor <- "#FFFFFF"
options[["browser"]]$hc_theme$tooltip$borderColor <- "transparent"
options[["browser"]]$hc_theme$legend$backgroundColor <- "transparent"
options[["browser"]]$hc_theme$legend$itemStyle$fontWeight <- "500"
options[["browser"]]$hc_theme$legend$itemStyle$fontSize <- "14px"
options[["browser"]]$hc_theme$chart$style$fontFamily <- "Source Sans Pro"

## further ----------------------------------------------------

## stats ---------------------------------------------------------------------------------------------
options[["stats"]][["pca"]]<-data.frame(fluid=c("Plasma", "Urine","Plasma",     "Plasma", "Plasma","Urine","Plasma"),
                                        platform_id=c("Metabolon HD4 [nt-ms]","Metabolon HD4 [nt-ms]", "Biocrates p150 [t-ms]","In-house biochemistry [chem.]","numares (Lipofit) [NMR]","Chenomx [NMR]","Lipidyzer [nt-ms]"),
                                        platform=c("non-targeted LCMS","non-targeted LCMS","MS Biocrates p150","Biochemistry","NMR","NMR","Lipidyzer"),
                                        stringsAsFactors = F)

##network ---------------------------------------------------------------------------------------------

options[["network"]][["single_ggm"]]=NULL
for(i in grep(",",names(db_network))){
  options[["network"]][["single_ggm"]]=rbind(options[["network"]][["single_ggm"]],
                                             data.frame(
                                               stringsAsFactors = F,
                                               fluid=db_network[[i]]$fluid,
                                               platform=paste0(db_network[[i]]$platform,collapse=", "),
                                               cutoff=db_network[[i]]$thresh,
                                               code=names(db_network)[i] 
                                             ))
}
options[["network"]][["single_ggm"]]["cutoff_subtext"] = ""
options[["network"]][["single_ggm"]][["cutoff_subtext"]][c(11, 22, 25, 43, 55,66)] = "default"

options[["network"]][["multi_ggm"]]=NULL
for(i in grep("multi",names(db_network))){
  
  options[["network"]][["multi_ggm"]]=rbind(options[["network"]][["multi_ggm"]],
                                            data.frame(
                                              code=names(db_network)[i],
                                              platform_plasma = db_network[[i]]$platform_plasma , 
                                              platform_urine = db_network[[i]]$platform_urine , 
                                              cutoff_plasma = db_network[[i]]$thresh_plasma , 
                                              cutoff_urine = db_network[[i]]$thresh_urine,
                                              stringsAsFactors = F
                                            ))
}

options[["network"]][["multi_ggm"]][["cutoff_subtext"]]<-c("recommended", "", "")


options[["network"]][["databases"]]<- data.frame(fluid=c("Plasma","Urine","Plasma"),
                                                 platform=c("Metabolon HD4", "Metabolon HD4", "Biocrates p150"),
                                                 code=c("metabolon_plasma","metabolon_urine", "biocrates_plasma"),
                                                 stringsAsFactors = F)

options[["network"]][["info_super_pathway"]]=data.frame('SUPER.PATHWAY'=c("Amino Acids", "Xenobiotics","Nucleotides","Lipids","Energy","Peptides","Carbohydrates","Cofactors and Vitamins",NA, "super", "sub","Lipoproteins","Unknown"),
                                                        color.background=c("#E41A1C","#377EB8","#4DAF4A","orange","#A65628","salmon","#999999","#984EA3","#bfbfbf", "grey","grey","orange","purple"),
                                                        color.highlight.background=c("#E41A1C","#377EB8","#4DAF4A","orange","#A65628","salmon","#999999","#984EA3","#bfbfbf", "grey","grey","orange","purple"),
                                                        stringsAsFactors = F)

### information on platforms

options[["network"]][["info_platform"]]=data.frame(
  Platform=c("non-targeted LCMS", "MS Biocrates p150","Biochemistry","NMR","Lipidizer","PTRMS","ICR-FT-MS",NA),
  platform_id=c("Metabolon HD4", "Biocrates p150","Biochemistry","NMR","Lipidizer","PTRMS","ICR-FT-MS",NA),
  color.background=c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#bfbfbf"),
  color.highlight.background=c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#bfbfbf"),
  stringsAsFactors = F)


# About ----

about=list()

about[["baseline_anthropometry"]] <- read.csv2(file="data/about/baseline_anthropometry.csv")
colnames(about[["baseline_anthropometry"]]) <- c("Parameter","Mean","Standard deviation", "Coefficient of variation")


about[["imputation_table"]] <- read.csv2(file="data/about/imputation_table.csv")
colnames(about[["imputation_table"]]) <- c("Fluid","Platform","Total number of metabolites", "Metabolites >= 30% missingness")


about[["profiling_table"]] <- readxl::read_excel(path="data/about/profiling_table.xlsx") %>%
  dplyr::mutate(Reference = ifelse(Reference=="Krug et al. 2012",paste0(rep_href(href="https://pubmed.ncbi.nlm.nih.gov/22426117/",label="Krug et al., 2012.")),Reference))



