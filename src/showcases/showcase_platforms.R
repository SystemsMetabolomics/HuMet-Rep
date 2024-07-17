### Template for showcases

### create showcase list if it doesn't exist
if(!exists("showcase")){
  showcase<-list()
}


### general specifications
showcase_tag="platforms" ## create a tag to be used in the shiny app

### First row (Title + Research question) ####
    ## add text to research question
    showcase[[showcase_tag]]$question$text = '"How good is the correlation of overlapping metabolite signals measured on Metabolon HD4 vs Biocrates p150?"' ## adds title to showcase
    
    ## add img to research question
    showcase[[showcase_tag]]$question$img= "pkg/img/showcase/template/template.jpeg"
    
    ## add title to showcase
    showcase[[showcase_tag]]$title$text="Platform comparison"
    
    ## add subtext to title
    showcase[[showcase_tag]]$title$subtext1 = "(Yet et al., 2016) compiled a list comprising 43 metabolites that were measured both using the Biocrates p150 kit and using an older version of the Metabolon HD4 platform in a British cohort. Out of the 43 listed metabolite pairs, we could match 39 within our dataset. This list includes 17 amino acids, one carbohydrate, and 23 lipids.In this use case, we were interested to what extent these metabolite measures correlate between platforms."
    
    ## add additional subtext to title (numbering from 2 to ...)
    showcase[[showcase_tag]]$title$subtext2 = NULL ## if NULL or NA this section will not be integrated


### Second row (Workflow) ####
    ## add title to workflow
    showcase[[showcase_tag]]$workflow$title= "Workflow"
    
    showcase[[showcase_tag]]$workflow$title= "TBD"
    showcase[[showcase_tag]]$workflow$img1= "pkg/img/showcase/template/template.jpeg"
    showcase[[showcase_tag]]$workflow$img2= "pkg/img/showcase/template/template.jpeg"
    showcase[[showcase_tag]]$workflow$img3= "pkg/img/showcase/template/template.jpeg"
    showcase[[showcase_tag]]$workflow$img4= NULL
    
    showcase[[showcase_tag]]$workflow$subtext1= "TBD "
    showcase[[showcase_tag]]$workflow$subtext2= "TBD "
    showcase[[showcase_tag]]$workflow$subtext3= "TBD "
    showcase[[showcase_tag]]$workflow$subtext4= NULL


    ### Third row (Results) ####
    showcase[[showcase_tag]]$results$title= "Results"
    showcase[[showcase_tag]]$results$img= "pkg/img/showcase/template/template.jpeg"
    showcase[[showcase_tag]]$results$img_subtext= "Add results img subtext here "
    showcase[[showcase_tag]]$results$text2= "Add second results text here"
    showcase[[showcase_tag]]$results$text3= "Add third results text here"

### Forth row (Discussion) ####
    showcase[[showcase_tag]]$discussion$title= "Discussion"
    showcase[[showcase_tag]]$discussion$img= "pkg/img/showcase/template/template.jpeg"
    showcase[[showcase_tag]]$discussion$img_subtext= "Add discussion img subtext here "
    showcase[[showcase_tag]]$discussion$text2= "Add second discussion text here"
    showcase[[showcase_tag]]$discussion$text3= "Add third discussion text here"
    
### Fith row (Methods) ####
    showcase[[showcase_tag]]$methods$title= "Methods"
    showcase[[showcase_tag]]$methods$img= "pkg/img/showcase/template/template.jpeg"
    showcase[[showcase_tag]]$methods$img_subtext= "Add methods img subtext here "
    showcase[[showcase_tag]]$methods$text2= "Add second methods text here"
    showcase[[showcase_tag]]$methods$text3= NULL
    
rm("showcase_tag")
