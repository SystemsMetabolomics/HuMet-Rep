### Template for showcases

### create showcase list if it doesn't exist
if(!exists("showcase")){
  showcase<-list()
}


### general specifications
showcase_tag="networks" ## create a tag to be used in the shiny app

### First row (Title + Research question) ####
    ## add text to research question
    showcase[[showcase_tag]]$question$text = '"Which areas of metabolism change after extended fasting (36 h) globally in the reconstructed metabolic network?"' ## adds title to showcase
    
    ## add img to research question
    showcase[[showcase_tag]]$question$img= "pkg/img/showcase/template/template.jpeg"
    
    ## add title to showcase
    showcase[[showcase_tag]]$title$text="Showcase: Time-resolved metabolic networks"
    
    ## add subtext to title
    showcase[[showcase_tag]]$title$subtext1 = "TBD"
    
    ## add additional subtext to title (numbering from 2 to ...)
    showcase[[showcase_tag]]$title$subtext2 = NULL ## if NULL or NA this section will not be integrated


### Second row (Workflow) ####
    ## add title to workflow
    showcase[[showcase_tag]]$workflow$title= "Workflow"
    
    showcase[[showcase_tag]]$workflow$title= "Add workflow title here"
    showcase[[showcase_tag]]$workflow$img1= "pkg/img/showcase/template/template.jpeg"
    showcase[[showcase_tag]]$workflow$img2= "pkg/img/showcase/template/template.jpeg"
    showcase[[showcase_tag]]$workflow$img3= "pkg/img/showcase/template/template.jpeg"
    showcase[[showcase_tag]]$workflow$img4= NULL
    
    showcase[[showcase_tag]]$workflow$subtext1= "Add img subtext here "
    showcase[[showcase_tag]]$workflow$subtext2= "Add img subtext here "
    showcase[[showcase_tag]]$workflow$subtext3= "Add img subtext here "
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
    