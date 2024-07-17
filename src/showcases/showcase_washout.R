### Template for showcases

### create showcase list if it doesn't exist
if(!exists("showcase")){
  showcase<-list()
}


### general specifications
showcase_tag="washout" ## create a tag to be used in the shiny app

### First row (Research question) ####

## add text to research question
showcase[[showcase_tag]]$question$text = '"Which metabolites can be identified as putative dietary biomarkers of food ingestion from chicken meal ingredients?"'

## add img to research question
showcase[[showcase_tag]]$question$img= NULL #"pkg/img/showcase/template/template.jpeg"

### First row (Title) ####

## add title to showcase
showcase[[showcase_tag]]$title$text="Showcase: Prior Exposure"

## add subtext to title
showcase[[showcase_tag]]$title$sub_text = "Here, we seek to identify putative dietary markers of food ingestion from chicken meal ingredients. Before each of the two blocks in the HuMet study , a standardized meal was given. All meals after that were highly controlled with different food compositions. Dietary biomarkers should exhibit steady decreases after food intake and exhibit minimal interference from environmental stimuli."

## add additional subtext to title (numbering from 2 to ...)
showcase[[showcase_tag]]$title$subtext2 = NULL ## if NULL or NA this section will not be integrated


### Second row (Workflow) ####
## add title to workflow
showcase[[showcase_tag]]$workflow$title= "Workflow"
showcase[[showcase_tag]]$workflow$img1= "pkg/img/showcase/all_images/Folie1.png"
showcase[[showcase_tag]]$workflow$img2= "pkg/img/showcase/all_images/Folie2.png"
showcase[[showcase_tag]]$workflow$img3= "pkg/img/showcase/all_images/Folie3.png"
showcase[[showcase_tag]]$workflow$img4= NULL

showcase[[showcase_tag]]$workflow$subtext1= "We used 3-methylhistidine as our reference metabolite, which has recently been described as a putative biomarker for chicken meat intake. This metabolite therby describes steady decreasing kinetics upon food ingestion"
showcase[[showcase_tag]]$workflow$subtext2= "Metabolites with a small Frechet distance to our reference metabolite were selected into our metabolite bag for further investigation."
showcase[[showcase_tag]]$workflow$subtext3= "Selected metabolites were visualized within the Time course module"
showcase[[showcase_tag]]$workflow$subtext4= NULL

### Third row (Results) ####
showcase[[showcase_tag]]$results$title= "Results"
showcase[[showcase_tag]]$results$img= "pkg/img/showcase/results_exposure.jpg"
showcase[[showcase_tag]]$results$img_subtext= "Metabolites with similar kinetics to 3-methylhistidine measured on the plasma Metabolon HD4 platform [P, nt-ms]."
showcase[[showcase_tag]]$results$text2= "In our participants, plasma levels of 3-methylhistidine exhibited a steady decrease after chicken intake and exhibited minimal interference from environmental stimuli as generally required for a dietary biomarker."
showcase[[showcase_tag]]$results$text3= "Within the 40 most similar metabolites to 3-methylhistidine, we found 17 xenobiotics, 13 amino acids, and several carbohydrates, cofactors and vitamins, lipids, nucleotides, and peptides in blood and urine."

### Forth row (Discussion) ####
showcase[[showcase_tag]]$discussion$title= "Discussion"
showcase[[showcase_tag]]$discussion$img= NULL
showcase[[showcase_tag]]$discussion$img_subtext= NULL
showcase[[showcase_tag]]$discussion$text2= "Our similarity search revealed 40 metabolites with a similar washout trajectory that have been declared as or may be putative dietary biomarkers of food intake. We identified already published dietary biomarkers, including s-allyscystein and alliine that are associated with garlic intake reflecting the ingredients of our standardized chicken dinner at the beginning of each two-day sampling block."
showcase[[showcase_tag]]$discussion$text3= "Further investigation of undescribed metabolites may further help improve dietary assessments by providing an objective measure of food intake."

### Fith row (Methods) ####
showcase[[showcase_tag]]$methods$title= "Methods"
showcase[[showcase_tag]]$methods$img = NULL
showcase[[showcase_tag]]$methods$img_subtext= "We use the similarity search option in the Module Selection which allows users to choose a reference metabolite and calculate the similarity to all other measured metabolites. We chose 3-methylhistidine, as this metabolite has been established as a putative dietary biomarker for chicken meat intake (Yin et al., 2017) and exhibited minimal interference."
showcase[[showcase_tag]]$methods$text2= "Add second methods text here"
showcase[[showcase_tag]]$methods$text3= NULL


### Data ####

showcase[[showcase_tag]]$table= readxl::read_xlsx(path="src/showcases/data_showcase_washout.xlsx")

showcase[[showcase_tag]]$table$id=paste0("0",showcase[[showcase_tag]]$table$id)


showcase[[showcase_tag]]$table_new = read.csv2("src/showcases/data_showcase_washout_new.csv")

showcase[[showcase_tag]]$table$id=paste0("0",showcase[[showcase_tag]]$table$id)
