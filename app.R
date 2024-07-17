#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
source("src/global.R")
source("src/header.R")
source("src/sidebar.R")
source("src/body.R") 
# Define UI for application that draws a histogram
ui <- shinydashboard::dashboardPage(
    header=header,
    sidebar=sidebar,
    body=body,
    skin="black",
    title="HuMet Repository"
)

# server ----
server <- function(input, output, session) {
    
# Bags --------------------------------------------------------------------------------------------------------------------------
    ## Observer generating main bag title
    lapply(1:5, function(x) 
        observe({output[[paste0("main_sel_bagname_",x)]]<-renderUI({ div(paste0("Bag ",x,"(", length(input[[paste0("main_sel",x)]]),")"))})})
    )
    
    ## Observe updating which bag is selected
    lapply(1:5, function(x) 
        observeEvent(input[[paste0("main_sel_bagsel_",x)]],{updateRadioButtons(session, inputId="main_sel_bagselector",selected=x)})
    )
    ## change class of selected bag to highlight bag 
    observeEvent(input[["main_sel_bagselector"]],{
        x_body=1:5
        x_current=input[["main_sel_bagselector"]]
        addClass(id = paste0("main_sel_bagsel_",x_current), class = "btn-bagsel-active")
        for(i in x_body[x_body != x_current]){
            removeClass(id = paste0("main_sel_bagsel_",i), class = "btn-bagsel-active")
        }
    })
    
    ## actionButton: divert to browser (view metabolites)
    observeEvent(input[["main_sel_divert"]],{
        updateNavbarPage(session, inputId = "menu",select="metabdetail")
    })
    
    # reactive Values ------------------------------------------------------------
    ## Save all values to be used in reactive lists
    ## contains all relevant changed inputs
    rInput<-reactiveValues(subject=1:15,timepoint=1:57,platform=options$general$platform$platform_code,
                           imputation="none", transformation="zscore", bag=1, sel_id=NULL, sel_label=NULL,selected="")
    ## debounces --------------------------------------------------------------------
    #   ## debounce main selection
    observeEvent(input[["main_sel_bagselector"]],{rInput$bag=input[["main_sel_bagselector"]]})
    
    observeEvent(c(input[["main_sel1"]],input[["main_sel2"]],input[["main_sel3"]],input[["main_sel4"]],input[["main_sel5"]], rInput$bag),{
        rInput$sel_labels=input[[paste0("main_sel",input[["main_sel_bagselector"]])]]
        rInput$sel_id=input[[paste0("main_sel",input[["main_sel_bagselector"]])]] %>% help_convert(to="ID")
    })
    
    # Observer to clear out main_sel that is active
    observeEvent(input[["main_sel_clear"]],{
        updateSelectizeInput(session, inputId = paste0("main_sel",input[["main_sel_bagselector"]]), selected="")
    })
    
    # debounce header transformation
    debounce_transformation = reactive({input$main_trans}) %>% debounce(500)
    
    observe({rInput[["transformation"]]=debounce_transformation()})
    
    debounce_imputation = reactive({input$main_imputation}) %>% debounce(500)
    observe({rInput[["imputation"]] = debounce_imputation()})
    ## debounce header subject
    debounce_subject= reactive({input$main_subject}) %>% debounce(500)
    observe({
        subject=debounce_subject()
        if(length(subject)<2){
            showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                  style="background:#ddd;text-align:center;",
                                  tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                  tags$span("Select at least two subjects",style="font-weight:700; font-size:20px;display:block;color:black"),
                                  modalButton("Dismiss")))
            updateAwesomeCheckboxGroup(session, inputId = "main_subject",selected=1:15, status="danger")
        }else{
            rInput[["subject"]]=subject
        }
    })
    
    ## debounce time slider
    debounce_timepoint = reactive({input[["study_slider"]]}) %>% debounce (2000)
    
    observe({
        check_timepoint=debounce_timepoint()
        if(length(check_timepoint)<=1){
            showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                  style="background:#ddd;text-align:center;",
                                  tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                  tags$span("Please choose a range of time points.",style="font-weight:700; font-size:20px;display:block;color:black"),
                                  modalButton("Dismiss")))
            slider_labels = info_sample$challengeTime[which(info_sample$timepoint %in% 1:56)]
            shinyWidgets::updatesliderTextInput(session, inputId = "study_slider", selected=slider_labels)
        }else{
            rInput[["timepoint"]] = check_timepoint %>% help_convert_studyslider()
        }
    })
    
    ## debounce header platform
    debounce_platform = reactive({input$main_platform}) %>% debounce (2000)
    observe({
        platform=debounce_platform()
        
        if(length(platform)<1){
            showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                  style="background:#ddd;text-align:center;",
                                  tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                  tags$span("Select at least one platform",style="font-weight:700; font-size:20px;display:block;color:black"),
                                  modalButton("Dismiss")))
            updateAwesomeCheckboxGroup(session, inputId = "main_platform",selected=options[["general"]][["platform_choices"]]%>%unlist(), status="danger")
        }else{
            rInput[["platform"]]=platform
        }
    })
    #   
    
    
    ## rData --------------------------------------------------------------
    ###  info: rData saves all current data that is being used. 
    rData<-reactiveValues(   
        allData=db_data$raw_list,
        networkData=db_network$data,
        distanceData=db_data$dist_norm_imp
    )
    
    #### rData processing -----------------------------------------------------------
    ### load  data used as rData$allData
    
    observeEvent(rInput[["imputation"]],{
      # load data for distance calculations
      imputation <- rInput[["imputation"]]

          out <- readRDS(paste0("data/measurements/df_",imputation,"_raw.rds")) %>% 
            dplyr::select(code, timepoint, subject, setdiff(names(.[]), c("code","timepoint","subject"))) %>% 
            dplyr::mutate(timepoint=as.numeric(timepoint), subject=as.numeric(subject))
          out[,-c(1:3)] = out[,-c(1:3)] %>% scale(center = TRUE, scale = TRUE) %>% as.data.frame()
          rData$distanceData = out
    })
    
    observeEvent(c(rInput[["imputation"]],rInput[["transformation"]]),{
        imp <-rInput[["imputation"]]
        trans <- rInput[["transformation"]]
        rData[["allData"]] <- readRDS(paste0("data/measurements/list_",imp,"_",trans,".rds"))
    })
    
    ## rSearch  -------------------------------------------------------------------
    rSearch<-reactiveValues(
        subject=1:15, timepoint=1:56,
        platform= options$general$platform$platform_code,
        table=selection_table_basis(info_met),
        allTable=NULL,allTable_selected=NULL,
        simTable=NULL,search_sim_table_selected=NULL,search_sim_table_show=NULL,
        keggTable=NULL,
        keggTable_selected=NULL,
        subTable=NULL, subTable_selected=NULL
    )
    
    ## rBrowser -------------------------------------------------------------------
    rBrowser<-reactiveValues(
        subject=1:15, timepoint=1:56,
        platform= options$general$platform$platform_code,
        bagShow=T,
        remove=NULL,
        switchInterpolation=list(disabled=F, mode="off"),
        annotation=F,
        plotType="oneplot",
        lineType="line"
    )
    ### rBrowser processing  ------------------------------------------------------
    
    ## rStatistic ------------------------------------------------------------
    rStatistic<-reactiveValues(
        subject=1:15,
        timepoint=1:56,
        fluid="P",
        platform= c("Metabolon HD4 [nt-ms]","Biocrates p150 [t-ms]","In-house biochemistry [chem.]","numares (Lipofit) [NMR]"),
        label=NULL,
        selectize=NULL,
        ttest_allMets=T,
        #ttest_data=db_stats[["raw_01"]],
        #ttest_data_filtered=NULL,
        ttest_table=NULL,
        ttest_platform = options$general$platform$platform_code %>% unique(),
        timepoints=1:56,
        ttest_volcano=NULL,
        pca_data=NULL
    )
    ## rPca  ----------------------------------------------------------------------
    rPCA<-reactiveValues(
        subject=1:15,
        met_label=NULL,
        timepoints=1:56,
        data=NULL
    )
    
    ### rPca processing  ----------------------------------------------------------
    
    observe({
        check_subject = input[["main_subject"]]
        if(input$menu %in% c("stats_pca")){
            if(length(check_subject)>=2){
                check_subject = check_subject
            }
            else if(length(check_subject)==1){
                showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                      style="background:#ddd;text-align:center;",
                                      tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                      tags$span("Select at least two subjects",style="font-weight:700; font-size:20px;display:block;color:black"),
                                      modalButton("Dismiss")))
                add_subject=setdiff(c(1:15), check_subject)[1]
                check_subject=c(check_subject, add_subject)
                updateAwesomeCheckboxGroup(session, inputId = "main_subject",selected=unique(check_subject), status="danger")
            }
            else if(length(check_subject)< 1){
                showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                      style="background:#ddd;text-align:center;",
                                      tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                      tags$span("Select at least two subjects",style="font-weight:700; font-size:20px;display:block;color:black"),
                                      modalButton("Dismiss")))
                check_subject=c(1:2)
                updateAwesomeCheckboxGroup(session, inputId = "main_subject",selected=check_subject, status="danger")
            }
            else{}
            rStatistic$subject=check_subject
        }else{}
    })
    
    ### statistics timepoint
    observe({
        check_timepoint=rInput[["timepoint"]]
        if(input$menu %in% c("stats_pca")){
            if(length(check_timepoint)>1){
                rStatistic$timepoint = check_timepoint
            }
            else if(length(check_timepoint)==1){
                add_timepoint = setdiff(c(1:56), check_timepoint %>% help_convert_studyslider())[1]
                new_timepoint = c(check_timepoint, add_timepoint) %>% unique()
                
                slider_labels = info_sample$challengeTime[which(info_sample$timepoint %in% new_timepoint)]
                shinyWidgets::updatesliderTextInput(session, inputId = "study_slider", selected=slider_labels)
                rStatistic$timepoint = new_timepoint
            }
            else if(length(check_timepoint)<1){
                shinyWidgets::updatesliderTextInput(session, inputId = "study_slider", selected=info_sample$challengeTime)
                rStatistic$timepoint = info_sample$challengeTime %>% help_convert_studyslider()
            }
            else{
                # do nothing
            }
        }
        else{ #no reaction
        }
    })
    ### statistics platform
    observeEvent(c(input$stats_pca_platform, rStatistic$subject),{
        check_platform <- input$stats_pca_platform
        check_menu <- input$menu %in% c("stats_pca") 
        
        
        if(check_menu & length(check_platform)>=1 & !"Lipidyzer [nt-ms]" %in% check_platform){
            rStatistic$platform=check_platform
        }
        else if(check_menu & length(check_platform)>=1 & "Lipidyzer [nt-ms]" %in% check_platform){
            #filter subjects that should not be used
            check_subject <- setdiff(input$main_subject,c(5:8))
            if(length(check_subject)>=1){
                showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                      style="background:#ddd;text-align:center;",
                                      tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                      tags$span("Lipidyzer data only available for 4 subjects!",style="font-weight:700; font-size:20px;display:block;color:black"),
                                      tags$span("By adding the Lipidyzer [nt-ms] platform only 4 subjects (5,6,7,8) are retained. All other subjects are excluded from analysis."),
                                      modalButton("Dismiss")
                ))
                updateAwesomeCheckboxGroup(session, inputId = "main_subject", selected = 5:8,status = "danger")
                rStatistic$platform=check_platform
            }
            else{
                rStatistic$platform=check_platform
            }
        }
        else if(check_menu & length(check_platform)==0){
            # if no platform is added
            showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                  style="background:#ddd;text-align:center;",
                                  tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                  tags$span("Select one platform at least",style="font-weight:700; font-size:20px;display:block;color:black"),
                                  modalButton("Dismiss")
            ))
            if(input$stats_pca_fluid=="Urine"){
                rStatistic$platform=c("Metabolon HD4 [nt-ms]","Chenomx [NMR]")
                updatePickerInput(session, inputId = "stats_pca_platform", selected= rStatistic$platform)
            }else{
                rStatistic$platform=c("Metabolon HD4 [nt-ms]","Biocrates p150 [t-ms]","In-house biochemistry [chem.]","numares (Lipofit) [NMR]")
                updatePickerInput(session, inputId = "stats_pca_platform", selected= rStatistic$platform)
            }
        }
    })
    
    
    ## statistics fluid
    observeEvent(input[["stats_pca_fluid"]],{
        check_fluid=input[["stats_pca_fluid"]]
        if(input$menu %in% c("stats_pca")){
            rStatistic$fluid= huBasic_matchFluid(fluidLong=check_fluid)
        }
    })
    
    ## statistics metabolites
    observe({
        #improve
        metabolite=rInput$sel_labels
        if(input$menu %in% c("stats_pca")){
            rStatistic$label=metabolite
        }
    })
    
    ## rTtest  --------------------------------------------------------------------
    rTtest<-reactiveValues(
        subject=1:15,
        met_label=NULL,
        timepoints=1:56,
        data=readRDS(file="data/results_ttest/none/none_01.rds") %>% 
            dplyr::select(pvalue, foldchange, conf_int_low, conf_int_high, t, df, tp_measured, timepoint, baseline, id, qvalue) %>% 
            dplyr::left_join(info_met, by=c("id"="ID")),
        platform_code = options$general$platform$platform_code %>% unique(),
        allMets=T,
        data_filtered=data.frame(),
        table=NULL,
        volcano_plot=NULL
    )
    
    ### rTtest processing ---------------------------------------------------------
    ### stats_ttest subject
    observe({
        check_subject = debounce_subject()
        is_tab =input$menu %in% c("stats_ttest")
        if(is_tab & length(check_subject)>=2){
            rTtest$subject=check_subject  #if enough subjects are availbale
        }
        else if(is_tab & length(check_subject)==1){
            showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                  style="background:#ddd;text-align:center;",
                                  tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                  tags$span("Select at least two subjects",style="font-weight:700; font-size:20px;display:block;color:black"),
                                  modalButton("Dismiss")))
            add_subject=setdiff(c(1:15), check_subject)[1]
            rTtest$subject=c(check_subject, add_subject)
            updateAwesomeCheckboxGroup(session, inputId = "main_subject",selected=unique(rTtest$subject), status="danger")
        }
        else if(is_tab& length(check_subject)< 1){
            showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                  style="background:#ddd;text-align:center;",
                                  tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                  tags$span("Select at least two subjects",style="font-weight:700; font-size:20px;display:block;color:black"),
                                  modalButton("Dismiss")))
            rTtest$subject=c(1:2)
            updateAwesomeCheckboxGroup(session, inputId = "main_subject",selected=rTtest$subject, status="danger")
        }
        else{}
    })
    
    ### stats_ttest timepoint
    observe({
        check_timepoint=debounce_timepoint()
        is_tab=input$menu %in% c("stats_ttest")
        if(is_tab & length(check_timepoint)>1){
            rTtest$timepoint = check_timepoint %>% help_convert_studyslider()
        }
        else if(is_tab & length(check_timepoint)==1){
            add_timepoint = setdiff(c(1:56), check_timepoint %>% help_convert_studyslider())[1]
            new_timepoint = c(check_timepoint %>% help_convert_studyslider(), add_timepoint) %>% unique()
            
            slider_labels = info_sample$challengeTime[which(info_sample$timepoint %in% new_timepoint)]
            shinyWidgets::updatesliderTextInput(session, inputId = "study_slider", selected=slider_labels)
            rTtest$timepoint = new_timepoint
        }
        else if(is_tab & length(check_timepoint)<1){
            shinyWidgets::updatesliderTextInput(session, inputId = "study_slider", selected=info_sample$challengeTime)
            rTtest$timepoint = info_sample$challengeTime %>% help_convert_studyslider()
        }
        else{
            # do nothing
        }
    })
    
    ### stats_ttest platform
    observe({
        platform_code = debounce_platform()
        is_tab =input$menu %in% c("stats_ttest")
        
        if(is_tab & length(platform_code)>=1){
            rTtest$platform=platform_code  #if enough platforms are availbale
        }
        else if (is_tab & length(platform_code)<1){
            showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="m", fade=T,
                                  style="background:#ddd;text-align:center;",
                                  tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                  tags$span("Select at least one platform",style="font-weight:700; font-size:20px;display:block;color:black"),
                                  modalButton("Dismiss")))
            rTtest$platform=options$general$platform_choices%>% unlist() %>% as.character()
            updateAwesomeCheckboxGroup(session, inputId = "main_platform",selected=unique(rTtest$platform), status="danger")
        }
    })
    
    ### statistics metabolites
    observe({
        metabolite=rInput$sel_labels
        is_tab= input[["menu"]] %in% c("stats_ttest")
        if(is_tab & length(metabolite)>=1){
            rTtest$met_label=metabolite
        }else{
            rTtest$met_label=NULL
        }
    })
    
    ## rNetwork ----------------
    rNetwork<-reactiveValues(
        title_code=db_network[["selected"]]$code,
        backbone="sggm",
        color="super",
        fluid=db_network[["selected"]]$fluid,
        sliderFluid=db_network[["selected"]]$fluid,
        platform=db_network[["selected"]]$platform,
        basic=db_network[["selected"]]$network,
        mod=db_network[["selected"]]$network ,
        highlight=list(metabolites=NULL, mode=NULL),
        time=2,
        slidertime=2,
        node_expansion=20
    )
    
    rDownload<-reactiveValues(
        browser_bag1 = NULL,
        browser_bag2 = NULL,
        browser_bag3 = NULL,
        browser_bag4 = NULL,
        browser_bag5 = NULL
    )
    
    ## rSession_store ----------------
    session_store <- reactiveValues()
    
    #### main functions  ---------------------------------------------------------
    
    
    ## header + render UI elements
    output[["header_subject_title"]]<-renderUI({
        # subtitle header item: subject
        title = "Subjects"
        subtitle = NULL
        status="active"
        icon = "user"
        menu = input[["menu"]]
        
        if(menu %in% c("home","about")){
            status="inactive"
        }
        else if(menu %in% c("metabselect","metabdetail","statistics","stats_pca")){
            status="active"
            subtitle = paste0(length(input[["main_subject"]]),"/15")
        }
        else if(input[["menu"]]%in%c("stats_ttest")){
            status="inactive"
            subtitle = "15/15"
        }
        else if(input[["menu"]]%in%c("network")){
            status="inactive"
            subtitle = "15/15"
        }
        div(style="display:block;height:48px;overflow:hidden;",
            div(class="mt_header_small", icon=icon(icon), style=ifelse(status=="active","","color:grey")),
            div(class="mt_header_large",
                div(title, style=paste0("display:block;margin-top:7px;",ifelse(status=="active","","color:grey"))),
                div(style="font-size:10px;",
                    div(subtitle, style=paste0("display:inline-block;",ifelse(status=="active","","color:grey"))),
                    tags$span(icon("chevron-circle-down"),style=paste0("display:inline-block;",ifelse(status=="active","","color:grey")))
                )
            )
        )
    })
    
    output[["header_challenge_title"]]<-renderUI({
        # title header item: challenges
        title = "Challenges"
        subtitle = NULL
        status="active"
        icon = "clock"
        menu = input[["menu"]]
        
        if(menu %in%c("home","about")){
            status="inactive"
        }
        else if(menu %in%c("metabselect","metabdetail","network","statistics","stats_ttest","stats_pca")){
            status="active"
            subtitle=paste0(
                ifelse(length(rInput[["timepoint"]])>=56, 56, length(rInput[["timepoint"]])),
                "/56")
        }
        else{
            status="inactive"
        }
        div(style="display:block;height:48px;overflow:hidden;",
            div(class="mt_header_small", icon=icon(icon), style=ifelse(status=="active","","color:grey")),
            div(class="mt_header_large",
                div(title, style=paste0("display:block;margin-top:7px;",ifelse(status=="active","","color:grey"))),
                div(style="font-size:10px;",
                    div(subtitle, style=paste0("display:inline-block;",ifelse(status=="active","","color:grey"))),
                    tags$span(icon("chevron-circle-down"),style=paste0("display:inline-block;",ifelse(status=="active","","color:grey")))
                )
            )
        )
    })
    
    output[["header_prep_title"]]<-renderUI({
        # title header item: pre-processing
        title = "Pre-processing"
        subtitle = NULL
        status="active"
        icon = "database"
        menu = input[["menu"]]
        
        if(menu %in% c("metabselect", "metabdetail")){
            status="active"
            subtitle=paste0(
                switch(input[["main_trans"]], "raw"="raw data", "zscore"="z-scored", "fcblock"="fold change", "fcchal"="fold change challenge"),"/",
                input[["main_imputation"]]
            )
        }
        else if(menu %in% c("stats_ttest")){
            status="active"
            subtitle=paste0(
                "log2 /",
                input[["main_imputation"]]
            )
        }
        else if(menu %in%c("network","stats_pca")){
            status="inactive"
            subtitle="log2 / rf imputation"
        }
        else{
            status="inactive"
            subtitle="error"
        }
        div(style="display:block;height:48px;overflow:hidden;",
            div(class="mt_header_small", icon=icon(icon), style=ifelse(status=="active","","color:grey")),
            div(class="mt_header_large",
                div(title, style=paste0("display:block;margin-top:7px;",ifelse(status=="active","","color:grey"))),
                div(style="font-size:10px;",
                    div(subtitle, style=paste0("display:inline-block;",ifelse(status=="active","","color:grey"))),
                    tags$span(icon("chevron-circle-down"),style=paste0("display:inline-block;",ifelse(status=="active","","color:grey")))
                )
            )
        )
    })
    
    
    output[["header_platform_title"]]<-renderUI({
        # title header item: platform
        title = "Platform / Fluid"
        subtitle = NULL
        status="active"
        icon = "cube"
        menu = input[["menu"]]
        
        if(menu%in%c("home","about")){
            status="inactive"
        }
        # active
        else if( menu %in%c("metabselect","metabdetail","statistics","stats_ttest")){
            status="active"
            subtitle=paste0(length(input$main_platform), "/9")
            
        }
        # inactive
        else if(menu %in%c("network")){
            status="inactive"
            subtitle="4/9"
        }
        else if(input[["menu"]]%in%c("stats_pca")){
            status="inactive"
            subtitle="9/9"
        }
        else{
            status="inactive"
        }
        div(style="display:block;height:48px;overflow:hidden;",
            div(class="mt_header_small", icon=icon(icon), style=ifelse(status=="active","","color:grey")),
            div(class="mt_header_large",
                div(title, style=paste0("display:block;margin-top:7px;",ifelse(status=="active","","color:grey"))),
                div(style="font-size:10px;",
                    div(subtitle, style=paste0("display:inline-block;",ifelse(status=="active","","color:grey"))),
                    tags$span(icon("chevron-circle-down"),style=paste0("display:inline-block;",ifelse(status=="active","","color:grey")))
                )
            )
        )
    })
    
    
    ## header + observe study header
    observeEvent(input[["study_btn_fasting"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[1]),as.character(info_sample$challengeTime[10])))})
    observeEvent(input[["study_btn_sldr"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[10]),as.character(info_sample$challengeTime[14])))})
    observeEvent(input[["study_btn_sld1"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[14]),as.character(info_sample$challengeTime[22])))})
    observeEvent(input[["study_btn_day1"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[1]),as.character(info_sample$challengeTime[9])))})
    observeEvent(input[["study_btn_day2"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[10]),as.character(info_sample$challengeTime[22])))})
    observeEvent(input[["study_btn_block1"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[1]),as.character(info_sample$challengeTime[22])))})
    observeEvent(input[["study_btn_ogtt"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[23]),as.character(info_sample$challengeTime[32])))})
    observeEvent(input[["study_btn_sld2"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[32]),as.character(info_sample$challengeTime[36])))})
    observeEvent(input[["study_btn_pat"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[36]),as.character(info_sample$challengeTime[42])))})
    observeEvent(input[["study_btn_oltt"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[43]),as.character(info_sample$challengeTime[53])))})
    observeEvent(input[["study_btn_stress"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[53]),as.character(info_sample$challengeTime[60])))})
    observeEvent(input[["study_btn_day3"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[23]),as.character(info_sample$challengeTime[44])))})
    observeEvent(input[["study_btn_day4"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[44]),as.character(info_sample$challengeTime[60])))})
    observeEvent(input[["study_btn_block2"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[23]),as.character(info_sample$challengeTime[60])))})
    observeEvent(input[["study_btn_all"]],{updateSliderTextInput(session, inputId = "study_slider",selected=c(as.character(info_sample$challengeTime[1]),as.character(info_sample$challengeTime[60])))})
    

    observeEvent(c(input[["menu"]], rNetwork[["basic"]], input[["main_sel"]], input[["main_platform"]]),{
        ## take care of main sel color
        tab=input[["menu"]]
        network=rNetwork[["basic"]]
        
        if(tab%in%c("network")){
            removeUI(selector=paste0("#mainSelcol"))
            insertUI("#add_mainSelcol", where="afterBegin",
                     ui=tags$head(
                         div(id=paste0("mainSelcol"),
                             tags$style(HTML(
                                 hu_selectize_exlcude(available_met = network$x$nodes$id)))))
            )
        }
        else if(tab %in% c("metabselect", "metabdetail","stats_ttest","stats_pca")){
            include= info_met$ID[which(info_met$platform_code %in% input[["main_platform"]])]
            removeUI(selector=paste0("#mainSelcol"))
            insertUI("#add_mainSelcol", where="afterBegin",
                     ui=tags$head(
                         div(id=paste0("mainSelcol"),
                             tags$style(HTML(
                                 hu_selectize_exlcude(available_met = include)))))
            )
        }
        else{
            removeUI(selector=paste0("#mainSelcol"))
        }
    })

    
    # Header -----------------------------------------------------------------------

    # Tab: home --------------------------------------------------------------------
    
    ## home divert ---------------------------------------------------------------
    
    ## actionButton: divert to browser (View metabolite from quick selection)
    observeEvent(input[["home_divert"]],{
        if(input[["home_sel"]]!=""){
            updateSelectizeInput(session=session, inputId = paste0("main_sel", rInput$bag), selected=input[["home_sel"]])
            updateNavbarPage(session=session, inputId = "menu",select="metabdetail")
        }else{
            showModal(modalDialog(title = NULL,easyClose = TRUE,footer = NULL, size="s", fade=T,
                                  style="background:#ddd;text-align:center;",
                                  tags$span(icon("warning"),style="font-weight:700; font-size:40px;display:block;color:black"),
                                  tags$span("Select metabolite first",style="font-weight:700; font-size:20px;display:block;color:black"),
                                  modalButton("Dismiss")
            ))
        }
    })
    
    observeEvent(input[["home_showcase_washout"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase_washout")})
    
    observeEvent(input[["home_showcase_network_longi"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase_networks_longi")})
    observeEvent(input[["home_showcase_platforms"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase_platforms")})
    observeEvent(input[["home_showcase_krug"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase_krug")})
    
    # Tab: tutorial ------
    observeEvent(input[["tutorial_divert_selection_all"]],{
       #switch to selection and all metabolites searcuh
      updateNavbarPage(session=session, inputId = "menu",select="metabselect")
      updateTabItems(session=session, inputId = "metabselect_tabbox",select="All metabolites")
    })
    observeEvent(input[["tutorial_divert_selection_sim"]],{
      #switch to selection and similarity search
      updateNavbarPage(session=session, inputId = "menu",select="metabselect")
      updateTabItems(session=session, inputId = "metabselect_tabbox",select="Similarity")
    })
    observeEvent(input[["tutorial_divert_selection_kegg"]],{
      #switch to selection and kegg search
      updateNavbarPage(session=session, inputId = "menu",select="metabselect")
      updateTabItems(session=session, inputId = "metabselect_tabbox",select="KEGG pathways")
    })
    observeEvent(input[["tutorial_divert_selection_anno"]],{
      #switch to selection and vendor annotation
      updateNavbarPage(session=session, inputId = "menu",select="metabselect")
      updateTabItems(session=session, inputId = "metabselect_tabbox",select="Annotated pathways")
    })
    
    observeEvent(input[["tutorial_divert_timecourse_aggregated"]],{
      #switch to browser and show aggregated
      updateNavbarPage(session=session, inputId = "menu",select="metabdetail")
      updatePickerInput(session=session, inputId="browser_plot_display", selected="aggregated")
    })
    observeEvent(input[["tutorial_divert_timecourse_individual"]],{
      #switch to browser and show individual
      updateNavbarPage(session=session, inputId = "menu",select="metabdetail")
      updatePickerInput(session=session, inputId="browser_plot_display", selected="individual")
    })
    observeEvent(input[["tutorial_divert_timecourse_minmax"]],{
      #switch to browser and kegg search
      updateNavbarPage(session=session, inputId = "menu",select="metabdetail")
      updatePickerInput(session=session, inputId="browser_plot_display", selected="minMax")
    })
    observeEvent(input[["tutorial_divert_timecourse_sd"]],{
      #switch to browser and vendor annotation
      updateNavbarPage(session=session, inputId = "menu",select="metabdetail")
      updatePickerInput(session=session, inputId="browser_plot_display", selected="errorBar")
    })
    
    observeEvent(input[["tutorial_divert_statistics_volcano"]],{
      #switch to statistics
      updateNavbarPage(session=session, inputId = "menu",select="stats_ttest")
    })
    observeEvent(input[["tutorial_divert_statistics_table"]],{
      #switch to statistics
      updateNavbarPage(session=session, inputId = "menu",select="stats_ttest")
    })
    
    ###### networks
    observeEvent(input[["tutorial_divert_networks_sggm"]],{
      updateNavbarPage(session=session, inputId = "menu",select="network")
      updateTabItems(session=session, inputId = "network_tabbox", selected="sggm")
    })
    observeEvent(input[["tutorial_divert_networks_mggm"]],{
      updateNavbarPage(session=session, inputId = "menu",select="network")
      updateTabItems(session=session, inputId = "network_tabbox", selected="mggm")
    })
    observeEvent(input[["tutorial_divert_networks_vendor"]],{
      updateNavbarPage(session=session, inputId = "menu",select="network")
      updateTabItems(session=session, inputId = "network_tabbox", selected="db")
    })
    
    observeEvent(input[["tutorial_divert_networks_superpathway"]],{
      updateNavbarPage(session=session, inputId = "menu",select="network")
      updatePickerInput(session=session, inputId="network_node_color", selected="super")
    })
    observeEvent(input[["tutorial_divert_networks_platform"]],{
      updateNavbarPage(session=session, inputId = "menu",select="network")
      updatePickerInput(session=session, inputId="network_node_color", selected="platform")
    })
    observeEvent(input[["tutorial_divert_networks_time"]],{
      updateNavbarPage(session=session, inputId = "menu",select="network")
      updatePickerInput(session=session, inputId="network_node_color", selected="time")
    })
    
    observeEvent(input[["tutorial_divert_download"]],{
      updateNavbarPage(session=session, inputId = "menu",select="download")
    })
    # Tab: Showcase (showcase) --------------------------------------------------------
    ## showcase divert -----------------------------------------------------------
    observeEvent(input[["showcase_item_washout"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase_washout")})
    observeEvent(input[["showcase_item_networks_cross"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase_networks_cross")})
    observeEvent(input[["showcase_item_networks_longi"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase_networks_longi")})
    observeEvent(input[["showcase_item_platforms"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase_platforms")})
    
    observeEvent(input[["showcase_divert_washout"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase")})
    observeEvent(input[["showcase_divert_networks_cross"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase")})
    observeEvent(input[["showcase_divert_networks_longi"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase")})
    observeEvent(input[["showcase_divert_platforms"]],{updateNavbarPage(session=session, inputId = "menu",select="showcase")})
    #showcase_divert_showcase_platforms
    # Tab: Selection (Metabolite selection) -----
    ## selection table ----
    observeEvent(input[["main_platform"]],{
        # selection table reactive to main platform
        if(input$menu=="metabselect"){
            selection_table=info_met %>% 
                selection_table_basis()
            selection_table = selection_table[which(selection_table$platform_code %in% input[["main_platform"]]),]
            rSearch$table=selection_table
            #update similarity reference metabolite
            new_ref_met=ifelse(input[["search_sim_table_refMet"]] %in% rSearch$table$labels, input[["search_sim_table_refMet"]], "none")
            if(!input[["search_sim_table_refMet"]] %in% rSearch$table$labels){
                updatePickerInput(session=session, inputId = "search_sim_table_refMet", selected=NULL, choices=rSearch$table$labels)
            }
        }
    })
    
    ## selection table_all ----------------------
    output[["search_table_all"]]<-DT::renderDataTable({
        table_all = rSearch$table
        
        if(nrow(table_all)>1){
          # process the data
          table_all =table_all %>%
            dplyr::select("graph","Metabolite","Annotated super-pathway","Annotated sub-pathway","Fluid","Platform","ChEBI","chebi_synonyms","HMDB","KEGG","IUPAC","PubChem","InchiKey","Swisslipids synonyms","Lipidmaps synonyms") %>%
            dplyr::rename(" "="graph", "ChEBI synonyms"="chebi_synonyms") # get information
          # create data widget
            DT::datatable(table_all,
                          rownames=table_all$labels,
                          filter=list(position="top"),
                          escape=F,
                          selection=list(mode="multiple",target="row", area=F),
                          extensions = 'Buttons',
                          options=list(dom='Brftlp',
                                       buttons = list(list(extend = 'colvis', 
                                                           columns = grep(x=names(table_all),pattern="IUPAC|synonyms|PubChem|InchiKey|ChEBI synonyms|HMDB|KEGG")-1, # hide these column by default
                                                           text="show/hide column")),
                                       drawCallback=DT::JS("function() {
                                                     $('#'+ this[0].id + ' tbody tr').css({'cursor': 'copy'})}"),
                                       
                                       columnDefs = list(
                                           list(targets = grep(x=names(table_all), pattern="IUPAC|synonyms|PubChem|InchiKey|ChEBI synonyms|HMDB|KEGG")-1, visible = F),
                                           list(width = '10%', targets = c(0,2,4,5,6)),
                                           list(width = '20%', targets = c(1,3)),
                                           list(searchable = F, targets = c(0))),
                                       pageLength = 15, 
                                       autoWidth = F)
            )
        }else{
          DT::datatable(data=data.frame())
        }
    })
    
    search_table_all_proxy= dataTableProxy(outputId = "search_table_all")
    
    ## add metabolites from selection table 
    observeEvent(input[["search_table_all_rows_selected"]],{
        selected_nr=input[["search_table_all_rows_selected"]]
        if(selected_nr >= 0){
            selected_labels=rSearch$table$labels[selected_nr]
            selectRows(proxy=search_table_all_proxy, selected=NULL)
            updateSelectizeInput(session=session, inputId=paste0("main_sel", rInput$bag), selected=unique(append(input[[paste0("main_sel", rInput$bag)]], selected_labels)))
        }
    })
    
    
    ## selection information -----------------------------------------------------
    output$search_sim_table_info<-renderUI({
        out_platform=NULL
        if(input[["menu"]]=="metabselect" & length(input[["main_platform"]])<9){
            out_platform= paste0(length(input[["main_platform"]]), "/9 platforms selected. Only metabolites measured on selected platforms can be used as reference metabolite.")
        }
        div(out_platform)
    })
    
    
    
    ## selection table_sim ------------------------------------------------------
    output[["search_table_sim"]]<-DT::renderDT({
        dist_meth = input[["search_sim_table_dist"]] # distance method
        ref_id = input[["search_sim_table_refMet"]] %>% help_convert(to="ID") # distance basis
        
        time = rInput$timepoint # time point range 
        my_subject = rInput[["subject"]] # subjects included
        min_overlap = input$search_similarity_minoverlap
        platform =  rInput$platform # platforms included
        use_id=info_met$ID[which(info_met$platform_code %in% platform)]
        used_metabolites=info_met$ID[which(info_met$platform_code %in% platform)]
        
        # get table
        simTable=rSearch$table
        my_data=rData$distanceData
        
        my_data <- my_data %>%
            dplyr::filter(timepoint %in% time,  # filter time points
                          subject %in% my_subject)  %>% # filter subjects
            dplyr::select("code","timepoint","subject", any_of(use_id), all_of(ref_id))
        
        # filter down dataset if ref_id is from lipidyzer platform
        if(any(ref_id %in% info_met$ID[which(info_met$platform_code=="[P, Lipidyzer]")])){
          my_data = my_data %>%
            filter(subject %in% c(5:8))
        }
        
        
        if(!ref_id %in% names(my_data) | nrow(my_data) == 0){
            
            out_table = NULL
        }
        else if(input[["menu"]] == "metabselect" & ref_id %in% info_met$ID & dist_meth =="euclidean"){
            distance_table=browser_distance_single(table=simTable,
                                                      distance_data=my_data,
                                                      min_overlap=min_overlap,
                                                      reference=ref_id,
                                                      method="euclidean")
            similarity_table= simTable %>%
                dplyr::left_join(distance_table, by="ID") %>%
              dplyr::filter(!is.na(distance)) %>%
                browser_format_dist_table(reference = ref_id, cols = c("graph","distance","Metabolite","Annotated super-pathway","Annotated sub-pathway","Fluid","Platform","ChEBI","IUPAC","PubChem","InchiKey","Swisslipids synonyms","Lipidmaps synonyms","ID","labels")) %>%
                dplyr::rename("Euclidean distance"="distance"," "="graph")

            rownames(similarity_table)<-1:nrow(similarity_table)
            rSearch$simTable=similarity_table
            out_table=rSearch$simTable[,which(!colnames(rSearch$simTable) %in% c("ID","labels"))]
        }
        
        else if (input[["menu"]] == "metabselect" & ref_id %in% info_met$ID & dist_meth =="manhattan"){
            distance_table=browser_distance_single(table=simTable,
                                                      distance_data=my_data,
                                                      min_overlap=min_overlap,
                                                      reference=ref_id,
                                                      method="manhattan")
            similarity_table= simTable %>%
                dplyr::left_join(distance_table, by="ID") %>%
              dplyr::filter(!is.na(distance)) %>%
                browser_format_dist_table(reference = ref_id, cols=c("graph","distance","Metabolite","Annotated super-pathway","Annotated sub-pathway","Fluid","Platform","ChEBI","IUPAC","PubChem","InchiKey","Swisslipids synonyms","Lipidmaps synonyms","ID","labels")) %>%
                dplyr::rename("Manhattan distance"="distance"," "="graph")

            rownames(similarity_table)<-1:nrow(similarity_table)
            rSearch$simTable=similarity_table # save table for download
            out_table=similarity_table[,which(!colnames(similarity_table) %in% c("ID","labels"))]
        }
        else if(input[["menu"]] == "metabselect" & ref_id %in% info_met$ID & dist_meth %in%c("frechet")){
          
          # define timepoints per block
            time_used_b1=my_data$timepoint[which(my_data$timepoint%in%c(1:20))] %>% unique()
            time_used_b2=my_data$timepoint[which(my_data$timepoint%in%c(21:56))] %>% unique()
            
          # calculate distance per subject
            calcTable <- lapply(my_subject, function(x){
              # get dataset and filter down to subject and all time points available for ref_id
              this_data <- my_data %>% 
                dplyr::filter(subject == x) %>% 
                dplyr::select(-any_of(c("code","subject"))) %>%
                dplyr::filter(!is.na(get(ref_id)))  %>%
                dplyr::left_join(info_sample[,c("timepoint","plot_timepoint")], by="timepoint")
              
              # only add metabolites that have >= overlap with min_overlap to reference metabolite
              met_check <- this_data %>%  is.na() %>% colSums()
              met_use <- which(met_check<(nrow(this_data)-min_overlap)) %>% names()
              
              if(nrow(this_data)!=0){
                #calculate for block 1
                b1_data = statistic_calc_frechet(data=this_data %>% 
                                                   dplyr::filter(timepoint %in% time_used_b1) %>%
                                                   dplyr::select(-timepoint,-plot_timepoint),
                                       time=this_data %>% 
                                         dplyr::filter(timepoint %in% time_used_b1) %>%
                                         pull("plot_timepoint"),
                                       id=ref_id)%>% 
                  dplyr::mutate(dist = ifelse(ID %in% c(ref_id, met_use),dist, NA)) # remove all metabolites that are lower than min_overlap
                #calculate for block 2
                b2_data = statistic_calc_frechet(data=this_data %>% 
                                                   dplyr::filter(timepoint %in% time_used_b2) %>%
                                                   dplyr::select(-timepoint,-plot_timepoint),
                                                 time=this_data %>% 
                                                   dplyr::filter(timepoint %in% time_used_b2) %>%
                                                   pull("plot_timepoint"),
                                                 id=ref_id)%>% 
                  dplyr::mutate(dist = ifelse(ID %in% c(ref_id, met_use),dist, NA)) # remove all metabolites that are lower than min_overlap
                plyr::rbind.fill(b1_data, b2_data) %>%
                  dplyr::group_by(ID) %>%
                  dplyr::summarise(dist=mean(dist, na.rm=T)) %>%
                  dplyr::ungroup()
              }else{
                #in case a metabolite is not measured for a subject
                data.frame(ID = names(this_data),dist=NA,stringsAsFactors = F)
              }
            }) %>% 
              plyr::rbind.fill() %>% 
              dplyr::group_by(ID) %>% 
              dplyr::summarise(dist=mean(dist, na.rm=T))%>%
              dplyr::ungroup() %>% 
              dplyr::mutate(dist = ifelse(is.nan(dist),NA,dist))

            # combine similarity table with annotation
            similarity_table = simTable %>% 
                dplyr::left_join(calcTable, by="ID") %>% 
                dplyr::arrange(dist)
            rownames(similarity_table)<-1:nrow(similarity_table)
            
            similarity_table <- similarity_table %>%
                select("graph","dist","Metabolite","Annotated super-pathway","Annotated sub-pathway","Fluid","Platform","ChEBI","IUPAC","PubChem","InchiKey","Swisslipids synonyms","Lipidmaps synonyms","ID","labels") %>%
                mutate(dist=round(dist,4)) %>%
               dplyr::filter(!is.na(dist)) %>%
                rename("Frechet distance"="dist"," "="graph")
              
            
            rSearch$simTable=similarity_table # save table for download
            out_table = similarity_table[,which(!colnames(similarity_table) %in% c("ID","labels"))]
        }
        else if(input[["menu"]] == "metabselect" & ref_id %in% info_met$ID & dist_meth %in%c("frechet_mean")){
          
          
            my_data_aggregated <- my_data %>% 
                aggregate.data.frame(by=list(my_data$timepoint), FUN=mean, na.rm=T) %>% 
                dplyr::select(-Group.1, -code, -subject)
            
            time_used_b1=my_data_aggregated$timepoint[which(my_data_aggregated$timepoint%in%c(1:20))] %>% unique()
            time_used_b2=my_data_aggregated$timepoint[which(my_data_aggregated$timepoint%in%c(21:56))] %>% unique()
            
            b1data <- statistic_calc_frechet(data=my_data_aggregated[my_data_aggregated$timepoint%in% time_used_b1,-1],
                                             time=info_sample$plot_timepoint[which(info_sample$timepoint%in% time_used_b1)],
                                             id=ref_id) %>% 
                dplyr::arrange(ID)%>% 
                dplyr::rename("dist1"="dist")
            
            b2data <- statistic_calc_frechet(data=my_data_aggregated[my_data_aggregated$timepoint%in% time_used_b2,-1],
                                             time=info_sample$plot_timepoint[which(info_sample$timepoint%in% time_used_b2)],
                                             id=ref_id) %>% 
                dplyr::arrange(ID) %>% 
                dplyr::rename("dist2"="dist")
            
            calcTable = b1data %>% 
                dplyr::left_join(b2data, by="ID") %>% 
                dplyr::mutate(dist = rowMeans(select(., dist1, dist2), na.rm = TRUE)) %>% 
                dplyr::select(-dist1, -dist2)
            
            similarity_table = simTable %>% 
                dplyr::left_join(calcTable, by="ID") %>% 
                dplyr::arrange(dist)
            
            rownames(similarity_table)<-1:nrow(similarity_table)
            
            similarity_table <- similarity_table %>%
                browser_format_dist_table(reference=ref_id, cols = c("graph","dist","Metabolite","Annotated super-pathway","Annotated sub-pathway","Fluid","Platform","ChEBI","IUPAC","PubChem","InchiKey","Swisslipids synonyms","Lipidmaps synonyms","ID","labels")) %>% 
              dplyr::filter(!is.na(dist)) %>%
              rename("Frechet distance"="dist"," "="graph")
            
            rSearch$simTable=similarity_table # save table for download
            out_table = similarity_table[,which(!colnames(similarity_table) %in% c("ID","labels"))]
        }
        else if(input[["menu"]] == "metabselect" & ref_id %in% info_met$ID & dist_meth %in%c("pearson_mean")){
            simTable=rSearch$table
            
            calc_table = aggregate.data.frame(my_data[,-c(1:3)], by=list(my_data$timepoint), FUN=mean, na.rm=T) %>% 
              dplyr::select(-Group.1) %>% 
              cor(method = "pearson",use="pairwise.complete.obs") %>% 
              as.data.frame() %>% 
              select(all_of(ref_id)) %>% 
              dplyr::mutate(ID= rownames(.[])) %>% 
              setNames(c("dist", "ID"))

            similarity_table =simTable %>% 
                dplyr::left_join(calc_table, by="ID") %>% 
                dplyr::mutate(dist = round(dist, 4)) %>% 
                dplyr::mutate(dist = ifelse(dist==1 & ID != ref_id, NA, dist)) %>% 
                dplyr::mutate(dist = ifelse(dist==-1 & ID != ref_id, NA, dist)) %>%
                dplyr::arrange(desc(dist)) %>% 
              dplyr::filter(!is.na(dist)) %>%
                dplyr::select("graph","dist","Metabolite","Annotated super-pathway","Annotated sub-pathway","Fluid","Platform","ChEBI","IUPAC","PubChem","InchiKey","Swisslipids synonyms","Lipidmaps synonyms","ID","labels") %>%
                dplyr::rename("Pearson correlation"="dist"," "="graph") 
            
            rownames(similarity_table)<-1:nrow(similarity_table)
            rSearch$simTable=similarity_table
            out_table = similarity_table[,which(!colnames(similarity_table) %in% c("ID","labels"))]
        }
        else if(input[["menu"]] == "metabselect" & ref_id %in% info_met$ID & dist_meth %in%c("pearson_single")){
          distance_table=browser_cor_single(table=simTable,
                                            min_overlap=min_overlap,
                                            distance_data=my_data,
                                            reference=ref_id)
          similarity_table= simTable %>%
            dplyr::left_join(distance_table, by="ID") %>%
            browser_format_dist_table(reference = ref_id, cols=c("graph","distance","Metabolite","Annotated super-pathway","Annotated sub-pathway","Fluid","Platform","ChEBI","IUPAC","PubChem","InchiKey","Swisslipids synonyms","Lipidmaps synonyms","ID","labels")) %>%
            dplyr::arrange(desc(distance)) %>%
            dplyr::filter(!is.na(distance)) %>%
            dplyr::rename("Pearson correlation"="distance"," "="graph")
          
          rownames(similarity_table)<-1:nrow(similarity_table)
          rSearch$simTable=similarity_table # save table for download
          out_table=similarity_table[,which(!colnames(similarity_table) %in% c("ID","labels"))]
        }
        
        # print table
        if(is.data.frame(out_table)){
            DT::datatable(out_table,
                          rownames=F, filter=list(position="top"),
                          extensions = 'Buttons',
                          escape=F,selection=list(mode="multiple",target="row", area=F),
                          options=list(dom='Brftlp',
                                       buttons = list(list(extend = 'colvis', 
                                                           columns = grep(x=names(out_table),pattern="IUPAC|synonyms|PubChem|InchiKey|ChEBI synonyms|HMDB|KEGG")-1, # hide these column by default
                                                           text="show/hide column")),
                                       drawCallback=DT::JS("function() {
                                                     $('#'+ this[0].id + ' tbody tr').css({'cursor': 'copy'})}"),
                                       columnDefs = list(
                                         list(targets = grep(x=names(out_table), pattern="IUPAC|synonyms|PubChem|InchiKey|ChEBI synonyms|HMDB|KEGG")-1, visible = F),
                                         list(width = '5%', targets=c(1)),
                                         list(searchable = F, targets = c(0))),
                                       pageLength = 15, autoWidth = T))
        }else{
          DT::datatable(data.frame())
        }
        
    })
    
    ## table_sim proxy
    search_table_sim_proxy= dataTableProxy(outputId = "search_table_sim")
    
    ## table_sim row_selection to main_sel
    observeEvent(input[["search_table_sim_rows_selected"]],{
        selected_nr=input[["search_table_sim_rows_selected"]]
        if(selected_nr > 0){
            selected_labels=rSearch$simTable$labels[selected_nr]
            selectRows(proxy=search_table_sim_proxy, selected=NULL)
            updateSelectizeInput(session=session, inputId=paste0("main_sel", rInput$bag), selected=unique(append(input[[paste0("main_sel", rInput$bag)]], selected_labels)))
        }
    })
    
    ## table_sim top 10 to main_sel
    observeEvent(input[["search_similarity_add"]],{
        selected_labels= rSearch$simTable$labels[1:10] # first 10
        updateSelectizeInput(session=session, inputId=paste0("main_sel",rInput$bag), selected=unique(append(input[[paste0("main_sel",rInput$bag)]],selected_labels)))
    })
    
    ## selection table_kegg ------------------------------------------------------

    ## selection table_sub -------------------------------------------------------
    output[["search_table_sub"]]<-DT::renderDT({
        pathwayName=input[["search_table_sub_refPath"]]
        if(input[["menu"]]=="metabselect"){
            subTable = rSearch[["table"]] %>%
                filter(`Annotated sub-pathway` %in% pathwayName) %>% 
              dplyr::filter(platform_code %in% input[["main_platform"]])
            
            subTable=subTable[order(subTable[["labels"]]),] # order metabolites
            rSearch$subTable=subTable # save table for visualization
            subTable = subTable %>%
                dplyr::select("graph","Metabolite","Annotated super-pathway","Annotated sub-pathway","Fluid","Platform","ChEBI","chebi_synonyms","HMDB","KEGG","IUPAC","PubChem","InchiKey","Swisslipids synonyms","Lipidmaps synonyms") %>%
                dplyr::rename(" "="graph", "ChEBI synonyms"="chebi_synonyms")
            
            DT::datatable(subTable,
                          rownames=subTable$labels,
                          filter=list(position="top"),escape=F,
                          selection=list(mode="multiple",target="row", area=F),
                          options=list(dom='Brftlp',
                                       buttons = list(list(extend = 'colvis', 
                                                           columns = grep(x=names(subTable), pattern="IUPAC|synonyms|PubChem|InchiKey|ChEBI synonyms|HMDB|KEGG")-1, # hide these column by default
                                                           text="show/hide column")),
                                       drawCallback=DT::JS("function() {
                                                     $('#'+ this[0].id + ' tbody tr').css({'cursor': 'copy'})}"),
                                       columnDefs = list(
                                         list(targets = grep(x=names(subTable), pattern="IUPAC|synonyms|PubChem|InchiKey|ChEBI synonyms|HMDB|KEGG")-1, visible = F),
                                           list(width = '10%', targets = c(0,2,4,5)),
                                           list(width = '20%', targets = c(1,3)),
                                           list(searchable = F, targets = c(0))),
                                       pageLength = 15, autoWidth = F)
            )
        }
        else{
            
        }
    })
    ### table_sub proxy
    search_table_sub_proxy= dataTableProxy(outputId = "search_table_sub")
    ### table_sub proxy rows to main_sel
    observeEvent(input[["search_table_sub_rows_selected"]],{
        selected_labels=rSearch$subTable$labels[input[["search_table_sub_rows_selected"]]]
        
        if(length(selected_labels)>0){
            selectRows(proxy=search_table_sub_proxy, selected=NULL)
            updateSelectizeInput(session=session, inputId=paste0("main_sel",rInput$bag), selected=unique(append(input[[paste0("main_sel",rInput$bag)]], selected_labels)))
        }
    })
    ### table_sub proxy rows to main_sel
    observeEvent(input[["search_table_sub_add"]],{
        updateSelectizeInput(session=session, inputId=paste0("main_sel",rInput$bag), selected=unique(append(input[[paste0("main_sel",rInput$bag)]],rSearch$subTable$labels)))
    })
    
    
    
    # tab: Browser (metdetail) -----------------------------------------------------
    
    ## browser reactive values -----------------------------------------------------
    rBags_reactive=list(
        bag1=list(name="bag1",
                  plots=reactive({
                      ## function to create plots
                      bag_nr=1
                      name=rBags_reactive[[paste0("bag",bag_nr)]]$name
                      browser_data=rData$allData
                      labels=input[[paste0("main_sel",bag_nr)]]
                      id=info_met$ID[which(info_met$labels%in%labels)]
                      subjects=rInput$subject
                      
                      # if single plots should be made
                      if(length(id>0) & input[["browser_plot_display"]]!="aggregated"){
                          ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                          for(i in id){
                              local({
                                  my_id   = i
                                  my_subjects=subjects
                                  my_name = info_met$labels[which(info_met$ID == my_id)]
                                  my_platform = info_met$Platform[which(info_met$ID == my_id)]
                                  if(my_platform=="Lipidyzer"){
                                      my_subjects=my_subjects[which(my_subjects %in% c(5,6,7,8))]
                                  }
                                  plotname <- paste0(name,"_single_",my_id)
                                  output[[plotname]]<-renderHighchart({
                                      
                                      # 1 # plot average curve with error bars
                                      if(input[["browser_plot_display"]]=="errorBar"){
                                          # plot=browser_plot_single_processing(x.data=rData$df,x.id=my_id %>% sample(size=40), x.subject=my_subjects, x.timepoint=c(1:56))
                                          # browser_plot_single_range(time_range=rInput$timepoint, plot_type = "errorBar", ylab=ylab)
                                          # 
                                          plot=browser_process_average(x.data=browser_data[my_id],subjects=my_subjects)%>%
                                              browser_plot_average_errorbar(type=rBrowser$f,timeLim=rInput$timepoint,ylab=ylab, split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                      }
                                      # 3 # plot individuals
                                      else if(input[["browser_plot_display"]]=="individual"){
                                          plot=browser_process_single(x.data=browser_data[my_id],
                                                                      subjects=my_subjects)%>%
                                              browser_plot_single(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                          
                                          
                                      }
                                      #plot average plots with min and max values as range
                                      else if(input[["browser_plot_display"]]=="minMax"){
                                          plot=browser_process_average(x.data=browser_data[my_id],
                                                                       subjects=my_subjects) %>%
                                              browser_plot_average_min_max(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                      }
                                      else{
                                          plot=NULL
                                      }
                                      plot
                                  })
                                  
                              })
                          }
                      }
                      
                  })
        ),
        bag2=list(name="bag2",
                  plots=reactive({
                      ## function to create plots
                      bag_nr=2
                      name=rBags_reactive[[paste0("bag",bag_nr)]]$name
                      browser_data=rData$allData
                      labels=input[[paste0("main_sel",bag_nr)]]
                      id=info_met$ID[which(info_met$labels%in%labels)]
                      subjects=rInput$subject
                      
                      # if single plots should be made
                      if(length(id>0) & input[["browser_plot_display"]]!="aggregated"){
                          ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                          for(i in id){
                              local({
                                  my_id   = i
                                  my_subjects=subjects
                                  my_name = info_met$labels[which(info_met$ID == my_id)]
                                  my_platform = info_met$Platform[which(info_met$ID == my_id)]
                                  if(my_platform=="Lipidyzer"){
                                      my_subjects=my_subjects[which(my_subjects %in% c(5,6,7,8))]
                                  }
                                  plotname <- paste0(name,"_single_",my_id)
                                  output[[plotname]]<-renderHighchart({
                                      
                                      # 1 # plot average curve with error bars
                                    if(input[["browser_plot_display"]]=="errorBar"){
                                     plot=browser_process_average(x.data=browser_data[my_id],subjects=my_subjects)%>%
                                        browser_plot_average_errorbar(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab, split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                    }
                                      # 3 # plot individuals
                                      else if(input[["browser_plot_display"]]=="individual"){
                                          plot=browser_process_single(x.data=browser_data[my_id],
                                                                      subjects=my_subjects)%>%
                                              browser_plot_single(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                          
                                          
                                      }
                                      #plot average plots with min and max values as range
                                      else if(input[["browser_plot_display"]]=="minMax"){
                                          plot=browser_process_average(x.data=browser_data[my_id],
                                                                       subjects=my_subjects) %>%
                                              browser_plot_average_min_max(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                      }
                                      else{
                                          plot=NULL
                                      }
                                      plot
                                  })
                                  
                              })
                          }
                      }
                      
                  })
        ),
        bag3=list(name="bag3",
                  plots=reactive({
                      ## function to create plots
                      bag_nr=3
                      name=rBags_reactive[[paste0("bag",bag_nr)]]$name
                      browser_data=rData$allData
                      labels=input[[paste0("main_sel",bag_nr)]]
                      id=info_met$ID[which(info_met$labels%in%labels)]
                      subjects=rInput$subject
                      
                      # if single plots should be made
                      if(length(id>0) & input[["browser_plot_display"]]!="aggregated"){
                          ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                          for(i in id){
                              local({
                                  my_id   = i
                                  my_subjects=subjects
                                  my_name = info_met$labels[which(info_met$ID == my_id)]
                                  my_platform = info_met$Platform[which(info_met$ID == my_id)]
                                  if(my_platform=="Lipidyzer"){
                                      my_subjects=my_subjects[which(my_subjects %in% c(5,6,7,8))]
                                  }
                                  plotname <- paste0(name,"_single_",my_id)
                                  output[[plotname]]<-renderHighchart({
                                      
                                      # 1 # plot average curve with error bars
                                      if(input[["browser_plot_display"]]=="errorBar"){
                                          plot=browser_process_average(x.data=browser_data[my_id],subjects=my_subjects)%>%
                                              browser_plot_average_errorbar(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab, split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                      }
                                      # 3 # plot individuals
                                      else if(input[["browser_plot_display"]]=="individual"){
                                          plot=browser_process_single(x.data=browser_data[my_id],
                                                                      subjects=my_subjects)%>%
                                              browser_plot_single(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                          
                                          
                                      }
                                      #plot average plots with min and max values as range
                                      else if(input[["browser_plot_display"]]=="minMax"){
                                          plot=browser_process_average(x.data=browser_data[my_id],
                                                                       subjects=my_subjects) %>%
                                              browser_plot_average_min_max(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                      }
                                      else{
                                          plot=NULL
                                      }
                                      plot
                                  })
                                  
                              })
                          }
                      }
                      
                  })
        ),
        bag4=list(name="bag4",
                  plots=reactive({
                      ## function to create plots
                      bag_nr=4
                      name=rBags_reactive[[paste0("bag",bag_nr)]]$name
                      browser_data=rData$allData
                      labels=input[[paste0("main_sel",bag_nr)]]
                      id=info_met$ID[which(info_met$labels%in%labels)]
                      subjects=rInput$subject
                      
                      # if single plots should be made
                      if(length(id>0) & input[["browser_plot_display"]]!="aggregated"){
                          ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                          for(i in id){
                              local({
                                  my_id   = i
                                  my_subjects=subjects
                                  my_name = info_met$labels[which(info_met$ID == my_id)]
                                  my_platform = info_met$Platform[which(info_met$ID == my_id)]
                                  if(my_platform=="Lipidyzer"){
                                      my_subjects=my_subjects[which(my_subjects %in% c(5,6,7,8))]
                                  }
                                  plotname <- paste0(name,"_single_",my_id)
                                  output[[plotname]]<-renderHighchart({
                                      
                                      # 1 # plot average curve with error bars
                                      if(input[["browser_plot_display"]]=="errorBar"){
                                          plot=browser_process_average(x.data=browser_data[my_id],subjects=my_subjects)%>%
                                              browser_plot_average_errorbar(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab, split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                      }
                                      # 3 # plot individuals
                                      else if(input[["browser_plot_display"]]=="individual"){
                                          plot=browser_process_single(x.data=browser_data[my_id],
                                                                      subjects=my_subjects)%>%
                                              browser_plot_single(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                          
                                          
                                      }
                                      #plot average plots with min and max values as range
                                      else if(input[["browser_plot_display"]]=="minMax"){
                                          plot=browser_process_average(x.data=browser_data[my_id],
                                                                       subjects=my_subjects) %>%
                                              browser_plot_average_min_max(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                      }
                                      else{
                                          plot=NULL
                                      }
                                      plot
                                  })
                                  
                              })
                          }
                      }
                      
                  })
        ),
        bag5=list(name="bag5",
                  plots=reactive({
                      ## function to create plots
                      bag_nr=5
                      name=rBags_reactive[[paste0("bag",bag_nr)]]$name
                      browser_data=rData$allData
                      labels=input[[paste0("main_sel",bag_nr)]]
                      id=info_met$ID[which(info_met$labels%in%labels)]
                      subjects=rInput$subject
                      
                      # if single plots should be made
                      if(length(id>0) & input[["browser_plot_display"]]!="aggregated"){
                          ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                          for(i in id){
                              local({
                                  my_id   = i
                                  my_subjects=subjects
                                  my_name = info_met$labels[which(info_met$ID == my_id)]
                                  my_platform = info_met$Platform[which(info_met$ID == my_id)]
                                  if(my_platform=="Lipidyzer"){
                                      my_subjects=my_subjects[which(my_subjects %in% c(5,6,7,8))]
                                  }
                                  plotname <- paste0(name,"_single_",my_id)
                                  output[[plotname]]<-renderHighchart({
                                      
                                      # 1 # plot average curve with error bars
                                      if(input[["browser_plot_display"]]=="errorBar"){
                                          plot=browser_process_average(x.data=browser_data[my_id],subjects=my_subjects)%>%
                                              browser_plot_average_errorbar(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab, split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                      }
                                      # 3 # plot individuals
                                      else if(input[["browser_plot_display"]]=="individual"){
                                          plot=browser_process_single(x.data=browser_data[my_id],
                                                                      subjects=my_subjects)%>%
                                              browser_plot_single(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                          
                                          
                                      }
                                      #plot average plots with min and max values as range
                                      else if(input[["browser_plot_display"]]=="minMax"){
                                          plot=browser_process_average(x.data=browser_data[my_id],
                                                                       subjects=my_subjects) %>%
                                              browser_plot_average_min_max(type=rBrowser$lineType,timeLim=rInput$timepoint,ylab=ylab,split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
                                      }
                                      else{
                                          plot=NULL
                                      }
                                      plot
                                  })
                                  
                              })
                          }
                      }
                      
                  })
        )
    )
    
    ## browser reactive values processing ----------------------------------------
    
    observeEvent(c(input[["browser_annotation"]],input[["browser_plot_display"]]),{
        #observe if bags should be annotated
        anno=input[["browser_annotation"]]
        disp=input[["browser_plot_display"]]
        rBrowser$annotation=anno
    })
    
    observeEvent(input$browser_line,{
        #add spline interpolation
        rBrowser$lineType=ifelse(input$browser_line,"spline","line")
    })
    
    output[[paste0("browser_bag1_header_title")]]<-renderUI({
        #create header_bag1 title
        bag_nr=1
        tags$span(paste0("Bag ", bag_nr), " ",
                  paste0(" (",length(input[[paste0("main_sel",bag_nr)]]),")"))
        
    })
    output[[paste0("browser_bag2_header_title")]]<-renderUI({
        #create header_bag1 title
        bag_nr=2
        tags$span(paste0("Bag ", bag_nr), " ",
                  paste0(" (",length(input[[paste0("main_sel",bag_nr)]]),")"))
        
    })
    output[[paste0("browser_bag3_header_title")]]<-renderUI({
        #create header_bag1 title
        bag_nr=3
        tags$span(paste0("Bag ", bag_nr), " ",
                  paste0(" (",length(input[[paste0("main_sel",bag_nr)]]),")"))
        
    })
    output[[paste0("browser_bag4_header_title")]]<-renderUI({
        #create header_bag1 title
        bag_nr=4
        tags$span(paste0("Bag ", bag_nr), " ",
                  paste0(" (",length(input[[paste0("main_sel",bag_nr)]]),")"))
        
    })
    output[[paste0("browser_bag5_header_title")]]<-renderUI({
        #create header_bag1 title
        bag_nr=5
        tags$span(paste0("Bag ", bag_nr), " ",
                  paste0(" (",length(input[[paste0("main_sel",bag_nr)]]),")"))
        
    })
    
    ## browser plot single -------------------------------------------------------
    output[[paste0("browser_bag1_plot_single")]]<-renderUI({
        bag_nr=1
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==F){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x) column(12, style="border-bottom:1px solid black;",
                                                       h5(paste0(info_met$labels[which(info_met$ID==x)])), ## add popover annotation
                                                       withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height)))
            )
        }
        else if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==T){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x)
                column(12, style="border-bottom:1px solid black;",
                       column(9,
                              h5(paste0(info_met$labels[which(info_met$ID==x)])),
                              withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height))),
                       column(3,style="font-size:10px;",br(),br(),browser_metabolite_annotation(x, subj=rInput$subject, tp=rInput$timepoint, rdata=rData$allData)))
            )
        }
    })
    output[[paste0("browser_bag2_plot_single")]]<-renderUI({
        bag_nr=2
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        
        if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==F){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x) column(12, style="border-bottom:1px solid black;",
                                                       h5(paste0(info_met$labels[which(info_met$ID==x)])), ## add popover annotation
                                                       withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height)))
            )
        }
        else if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==T){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x)
                column(12, style="border-bottom:1px solid black;",
                       column(9,
                              h5(paste0(info_met$labels[which(info_met$ID==x)])),
                              withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height))),
                       column(3,style="font-size:10px;",br(),br(),browser_metabolite_annotation(x, subj=rInput$subject, tp=rInput$timepoint, rdata=rData$allData)))
            )
        }
        
    })
    output[[paste0("browser_bag3_plot_single")]]<-renderUI({
        bag_nr=3
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==F){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x) column(12, style="border-bottom:1px solid black;",
                                                       h5(paste0(info_met$labels[which(info_met$ID==x)])), ## add popover annotation
                                                       withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height)))
            )
        }
        else if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==T){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x)
                column(12, style="border-bottom:1px solid black;",
                       column(9,
                              h5(paste0(info_met$labels[which(info_met$ID==x)])),
                              withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height))),
                       column(3,style="font-size:10px;",br(),br(),browser_metabolite_annotation(x, subj=rInput$subject, tp=rInput$timepoint, rdata=rData$allData)))
            )
        }
        
    })
    output[[paste0("browser_bag4_plot_single")]]<-renderUI({
        bag_nr=4
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==F){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x) column(12, style="border-bottom:1px solid black;",
                                                       h5(paste0(info_met$labels[which(info_met$ID==x)])), ## add popover annotation
                                                       withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height)))
            )
        }
        else if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==T){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x)
                column(12, style="border-bottom:1px solid black;",
                       column(9,
                              h5(paste0(info_met$labels[which(info_met$ID==x)])),
                              withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height))),
                       column(3,style="font-size:10px;",br(),br(),browser_metabolite_annotation(x, subj=rInput$subject, tp=rInput$timepoint, rdata=rData$allData)))
            )
        }
        
    })
    output[[paste0("browser_bag5_plot_single")]]<-renderUI({
        bag_nr=5
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==F){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x) column(12, style="border-bottom:1px solid black;",
                                                       h5(paste0(info_met$labels[which(info_met$ID==x)])), ## add popover annotation
                                                       withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height)))
            )
        }
        else if(length(id)>0 & input[["browser_plot_display"]]!="aggregated" & rBrowser$annotation==T){
            rBags_reactive[[paste0("bag",bag_nr)]]$plots()
            height=ifelse(input[["browser_plot_display"]]!="individual", "300px","500px")
            bag_content<-lapply(id, function(x)
                column(12, style="border-bottom:1px solid black;",
                       column(9,
                              h5(paste0(info_met$labels[which(info_met$ID==x)])),
                              withSpinner(highchartOutput(outputId=paste0("bag",bag_nr, "_single_",x), height=height))),
                       column(3,style="font-size:10px;",br(),br(),browser_metabolite_annotation(x, subj=rInput$subject, tp=rInput$timepoint, rdata=rData$allData)))
            )
        }
        
    })
    
    ## browser plot grp ----------------------------------------------------------
    output[["browser_bag1_hcGrpPlot"]] <- renderHighchart({
        if(input[["menu"]]=="metabdetail"& input[["browser_plot_display"]]=="aggregated"){
            bag_nr=1
            metID= input[[paste0("main_sel",bag_nr)]] %>%  help_convert(to="ID") #test
            if(input[["menu"]]=="metabdetail" && length(metID)>0){
                ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                # plot data
                plot_data = browser_process_average(x.data=rData$allData[metID], subjects=rInput$subject)
                rDownload$browser_bag1 = plot_data
                plot_data %>%
                    browser_plot_average_aggregated(type=rBrowser$lineType,
                                                    timeLim=rInput$timepoint,
                                                    ylab=ylab,
                                                    split_chal = ifelse(rInput[["transformation"]]=="fc_chal", TRUE, FALSE))
            }
        }
    })
    output[[paste0("browser_bag1_plot_grp")]]<-renderUI({
        bag_nr=1
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        bag_content=NULL
        
        if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==F & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px")))
        }
        else if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==T & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,
                               column(9,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px"))),
                               column(3, style="height:400px;font-size:10px;overflow-x:hidden;overflow-y:auto;",
                                      browser_metabolite_annotation(id=id, table_id="browser_annotation_table_aggregated",subj=rInput$subject, tp=rInput$timepoint, rdata=rData$allData)
                               )
            )
        }
        else{
            
        }
    })
    
    output[["browser_bag2_hcGrpPlot"]] <- renderHighchart({
        if(input[["menu"]]=="metabdetail"& input[["browser_plot_display"]]=="aggregated"){
            bag_nr=2
            metID= input[[paste0("main_sel",bag_nr)]] %>%  help_convert(to="ID") #test
            if(input[["menu"]]=="metabdetail" && length(metID)>0){
                ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                # plot data
                plot_data = browser_process_average(x.data=rData$allData[metID], subjects=rInput$subject)
                rDownload$browser_bag2 = plot_data
                plot_data %>%
                    browser_plot_average_aggregated(type=rBrowser$lineType,
                                                    timeLim=rInput$timepoint,
                                                    ylab=ylab)
            }
        }
    })
    output[[paste0("browser_bag2_plot_grp")]]<-renderUI({
        bag_nr=2
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        bag_content=NULL
        
        if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==F & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px")))
        }
        else if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==T & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,
                               column(9,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px"))),
                               column(3, style="height:400px;font-size:10px;",
                                      div(style="height:inherit;overflow-x:hidden;overflow-y:auto;",
                                          div(style="font-size:10px",
                                              tableHTML::tableHTML(obj=info_met[which(info_met$ID %in%id), c("Metabolite","SUPER.PATHWAY","SUB.PATHWAY")],border=0,
                                                                   headers=c("Metabolite","Super-pathway","Sub-pathway"),rownames = F,collapse="separate_shiny", widths = c("33%","33%","33%") )
                                          )
                                      )
                               )
            )
        }
        else{
            
        }
    })
    
    output[["browser_bag3_hcGrpPlot"]] <- renderHighchart({
        if(input[["menu"]]=="metabdetail"& input[["browser_plot_display"]]=="aggregated"){
            bag_nr=3
            metID= input[[paste0("main_sel",bag_nr)]] %>%  help_convert(to="ID") #test
            if(input[["menu"]]=="metabdetail" && length(metID)>0){
                ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                # plot data
                plot_data = browser_process_average(x.data=rData$allData[metID], subjects=rInput$subject)
                rDownload$browser_bag3 = plot_data
                plot_data %>%
                    browser_plot_average_aggregated(type=rBrowser$lineType,
                                                    timeLim=rInput$timepoint,
                                                    ylab=ylab)
            }
        }
    })
    output[[paste0("browser_bag3_plot_grp")]]<-renderUI({
        bag_nr=3
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        bag_content=NULL
        
        if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==F & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px")))
        }
        else if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==T & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,
                               column(9,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px"))),
                               column(3, style="height:400px;font-size:10px;",
                                      div(style="height:inherit;overflow-x:hidden;overflow-y:auto;",
                                          div(style="font-size:10px",
                                              tableHTML::tableHTML(obj=info_met[which(info_met$ID %in%id), c("Metabolite","SUPER.PATHWAY","SUB.PATHWAY")],border=0,
                                                                   headers=c("Metabolite","Super-pathway","Sub-pathway"),rownames = F,collapse="separate_shiny", widths = c("33%","33%","33%") )
                                          )
                                      )
                               )
            )
        }
        else{
            
        }
    })
    
    output[["browser_bag4_hcGrpPlot"]] <- renderHighchart({
        if(input[["menu"]]=="metabdetail"& input[["browser_plot_display"]]=="aggregated"){
            bag_nr=4
            metID= input[[paste0("main_sel",bag_nr)]] %>%  help_convert(to="ID") #test
            if(input[["menu"]]=="metabdetail" && length(metID)>0){
                ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                # plot data
                plot_data = browser_process_average(x.data=rData$allData[metID], subjects=rInput$subject)
                rDownload$browser_bag4 = plot_data
                plot_data %>%
                    browser_plot_average_aggregated(type=rBrowser$lineType,
                                                    timeLim=rInput$timepoint,
                                                    ylab=ylab)
            }
        }
    })
    output[[paste0("browser_bag4_plot_grp")]]<-renderUI({
        bag_nr=4
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        bag_content=NULL
        
        if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==F & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px")))
        }
        else if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==T & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,
                               column(9,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px"))),
                               column(3, style="height:400px;font-size:10px;",
                                      div(style="height:inherit;overflow-x:hidden;overflow-y:auto;",
                                          div(style="font-size:10px",
                                              tableHTML::tableHTML(obj=info_met[which(info_met$ID %in%id), c("Metabolite","SUPER.PATHWAY","SUB.PATHWAY")],border=0,
                                                                   headers=c("Metabolite","Super-pathway","Sub-pathway"),rownames = F,collapse="separate_shiny", widths = c("33%","33%","33%") )
                                          )
                                      )
                               )
            )
        }
        else{
            
        }
    })
    
    output[["browser_bag5_hcGrpPlot"]] <- renderHighchart({
        if(input[["menu"]]=="metabdetail" & input[["browser_plot_display"]]=="ag"){
            bag_nr=5
            metID= input[[paste0("main_sel",bag_nr)]] %>%  help_convert(to="ID") #test
            if(input[["menu"]]=="metabdetail" && length(metID)>0){
                ylab=ifelse(input$main_trans %in% c("zscore"), "Z score",ifelse(input$main_trans %in% c("fcblock", "fcchal"), "log2 fc", "Values"))
                # plot data
                plot_data = browser_process_average(x.data=rData$allData[metID], subjects=rInput$subject)
                rDownload$browser_bag5 = plot_data
                plot_data %>%
                    browser_plot_average_aggregated(type=rBrowser$lineType,
                                                    timeLim=rInput$timepoint,
                                                    ylab=ylab)
            }
        }
    })
    output[[paste0("browser_bag5_plot_grp")]]<-renderUI({
        bag_nr=5
        type=input[["browser_plot_display"]]
        id=info_met$ID[which(info_met$labels %in% input[[paste0("main_sel", bag_nr)]])]
        bag_content=NULL
        
        if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==F & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px")))
        }
        else if(length(id)>0 & input[["browser_plot_display"]]=="aggregated" & rBrowser$annotation==T & input[["menu"]]=="metabdetail"){
            #if aggregated and annotation F
            bag_content=column(12,
                               column(9,withSpinner(highchartOutput(outputId=paste0("browser_bag",bag_nr,"_hcGrpPlot"), height="400px"))),
                               column(3, style="height:400px;font-size:10px;",
                                      div(style="height:inherit;overflow-x:hidden;overflow-y:auto;",
                                          div(style="font-size:10px",
                                              tableHTML::tableHTML(obj=info_met[which(info_met$ID %in%id), c("Metabolite","SUPER.PATHWAY","SUB.PATHWAY")],border=0,
                                                                   headers=c("Metabolite","Super-pathway","Sub-pathway"),rownames = F,collapse="separate_shiny", widths = c("33%","33%","33%") )
                                          )
                                      )
                               )
            )
        }
        else{
            
        }
    })
    
    ## browser download --------
    output$browser_download_bag1_csv <- downloadHandler(
        filename = function() {
            paste('humet_summary_data.csv')
        },
        content = function(file) {
            if(is.null(rDownload$browser_bag1)){
                out<-data.frame(
                    metabolite=NA,
                    super_pathway=NA,
                    sub_pathway=NA,
                    mean=NA
                )
            }else{
                out <- rDownload$browser_bag1
            }
            out %>% 
                write.csv2(
                    file,
                    row.names=T)
        }
    )
    
    
    
    # tab: Network (network) -------------------------------------------------------
    
    ## network reactive ------------------------------------------------------------
    
    ## network reactive processing -------------------------------------------------
    observeEvent(input[["network_backbone"]],{
        #write input backbone to reactiveValues
        rNetwork$backbone=input[["network_backbone"]]
    })
    
    ## network options -----------------------------------------------------------
    ### network options title ----------------------------------------------------
    
    output$network_title <- shiny::renderUI({
        
        check_if=input[["network_tabbox"]]
        add_time=NULL
        if(input[["network_node_color"]]=="time"){
            add_time=paste0(" (",input[["network_time_slider"]],")")
        }
        
        
        if(check_if=="sggm"){
            title=paste0("Single fluid GGM",add_time)
        }
        else if(check_if =="mggm"){
            title=paste0("Multi fluid GGM",add_time)
        }
        else if(check_if == "db"){
            vendor_name=input[["network_db_platform"]]
            vendor_fluid=input[["network_db_fluid"]]
            title=paste0(vendor_fluid," ",vendor_name)
        }
        else{
            
            title="not fitted"
        }
        title
    })
    
    ### network options platform/fluid -------------------------------------------
    ### picker options for database networks
    observeEvent(input[["network_db_fluid"]],{
        update_id="network_db_platform"
        obs_id="network_db_fluid"
        picker_choices=options[["network"]][["databases"]]
        opt_platforms = picker_choices$platform[which(picker_choices$fluid==input[[obs_id]])]
        
        disabled_choices = !unique(picker_choices$platform) %in% opt_platforms
        updatePickerInput(session = session, inputId = update_id,choices = unique(picker_choices$platform),selected=ifelse(input[[update_id]]%in%c(opt_platforms),input[[update_id]],opt_platforms[1]),
                          choicesOpt = list(disabled = disabled_choices,style = ifelse(disabled_choices, yes = "color: rgba(119, 119, 119, 0.5);",no = "")
                          ))
    })
    observeEvent(c(input[["network_db_fluid"]],input[["network_db_platform"]],input[["network_tabbox"]]),{
        #change code
        if(input[["network_tabbox"]]=="db"){
            fluid=input[["network_db_fluid"]]
            platform=input[["network_db_platform"]]
            info=options[["network"]][["databases"]]
            code=info$code[which(info$platform==platform & info$fluid==fluid)]
            rNetwork[["title_code"]] = ifelse(length(code)==0,rNetwork[["title_code"]],code)
        }else{}
    })
    ### picker options for single fluid ggms
    observeEvent(input[["network_sggm_fluid"]],{
        obs_id="network_sggm_fluid"
        update_id="network_sggm_platform"
        picker_choices=options[["network"]][["single_ggm"]]
        fluid=input[[obs_id]]
        choices = picker_choices$platform[which(picker_choices$fluid==fluid)] %>% unique()
        disabled_choices = !unique(picker_choices$platform) %in% choices
        opt_choices = unique(picker_choices$platform)
        
        selected = ifelse(fluid=="Plasma", 2, 1)
        updatePickerInput(session = session, inputId = update_id,
                          choices = opt_choices,
                          selected= choices[selected],
                          choicesOpt = list(
                              disabled = disabled_choices,
                              style = ifelse(disabled_choices, yes = "color: rgba(119, 119, 119, 0.5);",no = "")
                          ))
        
        platform=input[["network_sggm_platform"]]
        opt_subtext = picker_choices$cutoff_subtext[which(picker_choices$fluid==fluid & picker_choices$platform==platform)]
        nr_selected=which(opt_subtext=="default")
        choices = picker_choices$cutoff[which(picker_choices$fluid==fluid)] %>% unique()
        updatePickerInput(session = session, inputId = "network_sggm_cutoff",
                          choices =choices,
                          selected= choices[nr_selected],
                          choicesOpt = list(
                              subtext = opt_subtext
                          ))
        
    })
    observeEvent(c(input[["network_sggm_fluid"]],input[["network_sggm_platform"]],input[["network_sggm_cutoff"]],input[["network_tabbox"]]),{
        if(input[["network_tabbox"]]=="sggm"){
            info=options[["network"]][["single_ggm"]]
            code=info$code[which(info$platform==input[["network_sggm_platform"]] &
                                     info$fluid==input[["network_sggm_fluid"]] &
                                     info$cutoff == input[["network_sggm_cutoff"]])]
            rNetwork[["title_code"]] = ifelse(length(code)==0,rNetwork[["title_code"]],code)
        }else{}
    })
    ### picker options for multi fluid ggms
    observeEvent(input[["network_mggm_cutoff_plasma"]],{
        obs_id="network_mggm_cutoff_plasma"
        update_id="network_mggm_cutoff_urine"
        picker_choices=options[["network"]][["multi_ggm"]]
        opt_platforms = picker_choices$cutoff_urine[which(picker_choices$cutoff_plasma==input[[obs_id]])]
        disabled_choices = !unique(picker_choices$cutoff_urine) %in% opt_platforms
        updatePickerInput(session = session, inputId = update_id,choices = unique(picker_choices$cutoff_urine),selected=ifelse(input[[update_id]]%in%c(opt_platforms),input[[update_id]],opt_platforms[1]),
                          choicesOpt = list(disabled = disabled_choices,style = ifelse(disabled_choices, yes = "color: rgba(119, 119, 119, 0.5);",no = "")
                          ))
    })
    observeEvent(c(input[["network_mggm_cutoff_plasma"]],input[["network_tabbox"]]),{
        #change code
        if(input[["network_tabbox"]]=="mggm"){
            info=options[["network"]][["multi_ggm"]]
            code=info$code[which(info$cutoff_plasma== input$network_mggm_cutoff_plasma)]
            rNetwork[["title_code"]] = ifelse(length(code)==0,rNetwork[["title_code"]],code)
        }else{}
    })
    
    ### network options highlight ------------------------------------------------
    observeEvent(c(input[["network_highlight"]], rInput$sel_id),{
        if(input[["network_highlight"]]){
            rNetwork[["highlight_id"]]=rInput$sel_id
        }else{
            rNetwork[["highlight_id"]]=NULL
        }
    })
    
    observeEvent(c(rNetwork[["title_code"]],input[["refresh_net_panel"]],input[["network_node_color"]], input[["network_singles"]]),{
        
        code=rNetwork[["title_code"]]
        network=db_network[[code]][["network"]]
        color=input[["network_node_color"]]
        highlightTime=rNetwork[["time"]]
        
        if(color=="time"){
            network$x$nodes = network$x$nodes %>% network_color_time_proxy(highlightTime =  highlightTime)
            network$x$nodes = network$x$nodes %>% network_title(time=T)
            submain=network_subtitle_proxy(timepoint =highlightTime)
            network = network %>% network_add_subtitle(subtitle = submain)
        }
        
        if(color=="super"){
            selected_pathway=input[["network_legend_super"]]
            network$x$nodes = network$x$nodes %>% network_color_super_proxy(selected=selected_pathway)
            network$x$nodes = network$x$nodes %>% network_title(time=F)
            network$x$size=40
        }
        
        if(color=="platform"){
            selected_platform=input[["network_legend_platform"]]
            network$x$nodes=network$x$nodes %>% network_color_platform_proxy(selected=selected_platform)
            network$x$nodes = network$x$nodes %>% network_title(time=F)
            network$x$nodes$size=40
        }
        
        # highlight metabolites
        if(length(rNetwork$highlight_id)>=1){
            met_highlight=rNetwork$highlight_id
            network$x$nodes = network$x$nodes %>% network_highlight(met_id = met_highlight)
        }
        
        if(!input[["network_singles"]]){
            network=network %>% network_filter_singles()
        }
        
        rNetwork[["basic"]]=network %>% 
          visPhysics(stabilization = FALSE)
    })
    
    ## network legend -----------------------------------------------------
    observeEvent(rNetwork[["basic"]],{
        if(input[["network_node_color"]]=="super"){
            choices=rNetwork$basic$x$nodes$SUPER.PATHWAY %>% unique()
            updateAwesomeCheckboxGroup(session=session, inputId = "network_legend_super",
                                       choices = choices,
                                       status="danger",
                                       inline=T,
                                       selected=choices)
        }
        if(input[["network_node_color"]]=="platform"){
            choices=rNetwork$basic$x$nodes$platform_id %>% unique()
            updateAwesomeCheckboxGroup(session=session, inputId = "network_legend_platform",
                                       choices = choices,
                                       status="danger",
                                       inline=T,
                                       selected=choices)
        }
    })
    
    ### legend time
    observeEvent(c(rNetwork[["title_code"]] ,rInput[["timepoint"]]),{
        ## update time slider to fluid or time range
        time_range<-rInput[["timepoint"]]
        title=rNetwork[["title_code"]]
        fluid=db_network[[title]]$fluid
        if("Urine" %in% fluid) fluid="Urine"
        else fluid="Plasma"
        
        if(!rNetwork$sliderFluid == fluid){
            if(fluid== "Urine"){
                choice_range = info_network %>%
                    filter(urine==T, timepoint %in% time_range)
                choice_range = choice_range$challenge_time
                updateSliderTextInput(session, inputId = "network_time_slider", choices=choice_range, selected=choice_range[1])
            }else{
                choice_range = info_network %>%
                    filter(plasma==T, timepoint %in% time_range)
                choice_range = choice_range$challenge_time
                updateSliderTextInput(session, inputId = "network_time_slider", choices=choice_range, selected=choice_range[1])
            }
        }
        rNetwork$sliderFluid=fluid
    })
    
    ## network visualization -----------------------------------------------------
    
    output[["display_network"]]<-renderVisNetwork({
        ## render network
        rNetwork[["basic"]] %>% 
          visOptions(autoResize = T)
    })
    
    ## network proxy
    observeEvent(c(input[["network_time_slider"]],input[["network_node_color"]]),{
        # register changed time to rNetwork
        if(input[["network_node_color"]]=="time"){
            slider_text=input[["network_time_slider"]]
            
            rNetwork[["time"]] = info_network$timepoint_code [which(info_network$challenge_time==slider_text)]
        }
    })
    
    
    observeEvent(c(rNetwork[["basic"]],
                   input[["network_legend_super"]],input[["network_legend_platform"]],rNetwork[["highlight_id"]],rNetwork[["time"]]),{
                       ## reactive network
                       network=rNetwork[["basic"]]
                       color=input[["network_node_color"]]
                       
                       highlightTime=rNetwork[["time"]]
                       submain=network_subtitle_proxy(timepoint =highlightTime)
                       if(color=="time" & highlightTime != rNetwork[["slidertime"]]){
                           network$x$nodes = network$x$nodes %>% network_color_time_proxy(highlightTime =  highlightTime)
                           network$x$nodes = network$x$nodes %>% network_title(time=T)
                           network = network %>% network_add_subtitle(subtitle = submain)
                           rNetwork[["slidertime"]]=highlightTime
                       }
                       
                       if(color=="super"){
                           super=input[["network_legend_super"]]
                           network$x$nodes = network$x$nodes %>% network_color_super_proxy(selected=super)
                           network$x$nodes = network$x$nodes %>% network_title(time=F)
                           network$x$nodes$size=30
                           submain=NA
                       }
                       
                       if(color=="platform"){
                           platform=input[["network_legend_platform"]]
                           network$x$nodes = network$x$nodes %>% network_color_platform_proxy(selected=platform)
                           network$x$nodes = network$x$nodes %>% network_title(time=F)
                           network$x$nodes$size=30
                           submain=NA
                       }
                       
                       # highlight metabolites
                       if(length(rNetwork$highlight_id)>=1){
                           met_selected=rNetwork[["highlight_id"]]
                           network$x$nodes = network$x$nodes %>% network_highlight(met_id = met_selected,highlight=T)
                           
                       }else{
                           network$x$nodes = network$x$nodes %>% network_highlight(met_id = NULL, highlight=F)
                       }
                       use_cols=intersect(c("id","font.size","color.border","font.background","borderWidth","size","color.background","pvalue","log2fc","title") ,names(network$x$nodes))
                       nodes=network$x$nodes[,which(colnames(network$x$nodes) %in% use_cols)]
                       
                       visNetworkProxy("display_network") %>%
                           visUpdateNodes(nodes=nodes) %>%
                           visSetTitle(submain=list(text=submain,
                                                    style="font-family:Arial, Times New Roman, Times, serif;font-size:12px;text-align:center;",
                                                    hidden=FALSE)
                           ) %>% 
                         visFocus(id="display_network",scale=1)
                       
                   })
    
    # Tab: Statistics (stats) -----------------------------------------------------
    
    ## divert statistics page
    observeEvent(input[["stats_divert_pca"]],{
        updateNavbarPage(session, inputId = "menu",select="stats_pca")
    })
    
    observeEvent(input[["stats_divert_ttest"]],{
        updateNavbarPage(session, inputId = "menu",select="stats_ttest")
    })
    
    
    # Tab: stats_pca ---------------------------------------------------------------
    observeEvent(input[["stats_pca_fluid"]],{
        update_id="stats_pca_platform"
        obs_id="stats_pca_fluid"
        picker_choices=options[["stats"]][["pca"]]
        opt_platforms = picker_choices$platform_id[which(picker_choices$fluid==input[[obs_id]])]
        
        disabled_choices = !unique(picker_choices$platform_id) %in% opt_platforms
        updatePickerInput(session = session, inputId = update_id,
                          choices = unique(picker_choices$platform_id),
                          selected = ifelse(input[[update_id]]%in%c(opt_platforms),input[[update_id]],opt_platforms[1]),
                          choicesOpt = list(disabled = disabled_choices,style = ifelse(disabled_choices, yes = "color: rgba(119, 119, 119, 0.5);",no = "")
                          ))
    })
    # PCA choices #
    
    observeEvent(c(input[["stats_pca_fluid"]],input[["stats_pca_platform"]],rStatistic$subject,input[["menu"]],rStatistic$timepoint),{
        # checks that data is suitable for calculation
        menu=input[["menu"]]
        platform = rStatistic$platform # add checks
        subject=rStatistic$subject 
        if("Lipidyzer [nt-ms]" %in% platform) subject <- 5:8# remove subjects that are not used for the lipidyzer platform)
        timepoint=intersect(rStatistic$timepoint, 1:56) # check that only main time points are used
        fluid=rStatistic$fluid # add checks
        data=db_data$dist_norm_imp
        
        if(menu!="stats_pca"){
            
        }
        else if(menu=="stats_pca" & length(platform)>0 & fluid=="P"){
            rStatistic$pca_data = huStatistics_calcPCAdata(data=data,
                                                           platform=platform,
                                                           fluid=fluid,
                                                           timepoint=timepoint,
                                                           subject=subject)
            
            rStatistic$pca_data$metaboliteInfo$Platform = rStatistic$pca_data$metaboliteInfo %>% hu_switch_platform()
            rStatistic$pca_data$metaboliteInfo$Fluid = rStatistic$pca_data$metaboliteInfo$Fluid %>% hu_switch_fluid()
            
        }
        else if(menu=="stats_pca" & any(c("Chenomx [NMR]","Metabolon HD4 [nt-ms]")%in% unique(options$stats$pca$platform_id)) & fluid=="U"){
            rStatistic$pca_data = huStatistics_calcPCAdata(data=data,
                                                           platform=platform,
                                                           fluid=fluid,
                                                           timepoint=timepoint,
                                                           subject=subject)
            rStatistic$pca_data$metaboliteInfo$Platform = rStatistic$pca_data$metaboliteInfo %>% hu_switch_platform()
            rStatistic$pca_data$metaboliteInfo$Fluid = rStatistic$pca_data$metaboliteInfo$Fluid %>% hu_switch_fluid()
        }
        else {
            rStatistic$pca_data=NULL
        }
    })
    
    output$stats_scores_plot<-renderPlotly({
        my_data = rStatistic$pca_data # load pca data
        input[["max_statistics_PCAscoresBox"]] # trigger recalculation if the max button is clicked
        
        if(input[["menu"]] =="stats_pca" & !is.null(my_data)){
            color_by=input[["stats_scores_plot_colorBy"]] #subject, timepoint
            type=ifelse(input[["stats_pca_dim"]], "3D","2D")
            
            p=huStatistics_plotScores(data=my_data,
                                      use_PC=c(1:3),
                                      color_by=color_by,
                                      type=type)
            session_store$stats_scores_plot=p
            p
        }else{
            
        }
    })
    
    output$stats_variance_explained<-renderPlotly({
        huStatistics_varianceExplained(rStatistic[["pca_data"]])
    })
    
    output$stats_loadings_plot<-renderPlotly({
        # stats_pca + render loadings plot
        my_data = rStatistic$pca_data
        input[["max_statistics_PCAloadingsBox"]] # reload if max box was clicked
        
        if(input[["menu"]]=="stats_pca" & !is.null(my_data)){
            color_by=input[["stats_loadings_plot_colorBy"]] #super, platform or default
            type= ifelse(input$stats_pca_dim, "3D","2D")
            
            p=huStatistics_plotLoadings(data=my_data,
                                        use_PC=c(1:3),
                                        color_by = color_by,
                                        type= type)
            session_store$stats_loadings_plot=p
            p
        }
        else{
            
        }
    })
    
    # Tab: stats_ttest -----------------------------------------------------------
    
    ## stats_ttest data ----------------------------------------------------------
    observeEvent(c(rInput[["timepoint"]],input[["menu"]],input[["main_imputation"]]),{
      #check and change
        if(input[["menu"]]=="stats_ttest"){
            base_tp = rInput[["timepoint"]][1] 
            load_tp = ifelse(nchar(base_tp)==1, paste0("0",base_tp),paste0(base_tp))
            type=input[["main_imputation"]]
            rTtest$data=readRDS(file=paste0("data/results_ttest/",type,"/",type,"_",load_tp,".rds"))  %>% 
                dplyr::select(pvalue, foldchange, conf_int_low, conf_int_high, t, df, tp_measured, timepoint, baseline, id, qvalue) %>% 
                dplyr::left_join(info_met, by=c("id"="ID"))
        }
    })
    
    ## stats_ttest ui ----------------------------------------------------------
    observeEvent(input[["stats_ttest_met"]],{
        if(!input[["stats_ttest_met"]]){
            updatePickerInput(session = session, inputId = "stats_ttest_thresh",
                              selected="bonferroni",
                              choices=list("no correction"="none","Bonferroni"="bonferroni","FDR"="fdr"),
                              choicesOpt = list(disabled = c(T,F,F),
                                                style = ifelse(c(T,F,F), yes = "color: rgba(119, 119, 119, 0.5);",no = "")
                              ))
            
        }else{
            updatePickerInput(session = session, inputId = "stats_ttest_thresh",
                              selected="none",
                              choices=list("no correction"="none","Bonferroni"="bonferroni","FDR"="fdr"),
                              choicesOpt = list(disabled = c(F,F,F),style = ifelse(c(F,F,F), yes = "color: rgba(119, 119, 119, 0.5);",no = "")
                              ))
        }
    })
    
    ## filter ttest data ----
    observeEvent(c(input[["stats_ttest_thresh"]],input[["stats_ttest_tp"]],input[["stats_ttest_met"]],rTtest$platform,rTtest$met_label, rTtest$timepoint,input[["menu"]],rTtest$data),{
        my_base=rTtest$data$baseline %>% unique() %>% as.numeric()
        my_range=c(rInput[["timepoint"]][[1]],rInput[["timepoint"]][length(rInput[["timepoint"]])])
        if(input[["menu"]]=="stats_ttest" & my_range[1] == my_base){
            #get information
            my_id=info_met$ID[which(info_met$labels %in% rTtest$met_label)]
            my_data = rTtest$data %>% 
                dplyr::filter(!is.na(pvalue), platform_code %in% rTtest$platform)%>%
                dplyr::mutate(timepoint= as.numeric(timepoint))
            
            # get inputs
            type_tp<-input[["stats_ttest_tp"]] #"Last vs. first"="selected","All vs. first"="all"
            
            if(type_tp=="selected"){
                my_data = my_data %>% 
                    dplyr::filter(timepoint %in% my_range)
            }
            else if(type_tp=="all"){
                my_data = my_data %>% 
                    dplyr::filter(timepoint <=my_range[2])
            }
            
            type_met = ifelse(input[["stats_ttest_met"]], "bag", "all")
            type_thresh <- input[["stats_ttest_thresh"]]  #"no correction"="none","Bonferroni"="bonferroni","FDR"="fdr")
            
            if(type_met=="all" & type_thresh=="fdr"){
                rTtest$data_filtered = my_data %>%
                    dplyr::filter(qvalue <=0.05, timepoint<=my_range[2])
            }
            else if(type_met=="all" & type_thresh=="bonferroni"){
                rTtest$data_filtered = my_data %>% 
                    dplyr::filter(pvalue <=0.05/2652/56, timepoint<=my_range[2])
            }
            else if(type_met=="bag" & length(my_id)>=1 & type_thresh =="none"){
                rTtest$data_filtered = my_data %>% 
                    dplyr::filter(id %in% my_id, timepoint<=my_range[2])
            }
            else if(type_met=="bag" & length(my_id)>=1 & type_thresh =="bonferroni"){
                rTtest$data_filtered = my_data %>% 
                    dplyr::filter(id %in% my_id, pvalue <=0.05/2652/56, timepoint<=my_range[2])
            }
            else if(type_met=="bag" & length(my_id)>=1 & type_thresh == "fdr"){
                rTtest$data_filtered = my_data %>% 
                    dplyr::filter(id %in% my_id,qvalue <=0.05, timepoint<=my_range[2])
            }
            else{
                rTtest$data_filtered=data.frame()
            }
        }
    })
    
    ## stats_ttest table ----------------------------------------------------------
    output[["stats_ttest_table"]]<-renderDT({
        if(nrow(rTtest$data_filtered)>0){
            my_table=rTtest$data_filtered 
            if(input[["menu"]]=="stats_ttest"){
                my_table = my_table %>% 
                    dplyr::arrange(pvalue, foldchange) %>% 
                    dplyr::select(all_of(c("Metabolite","baseline","timepoint","pvalue","qvalue","foldchange","Fluid","platform_name","SUPER.PATHWAY","SUB.PATHWAY"))) %>% 
                    dplyr::rename("p Value"="pvalue", "q Value"="qvalue", "Base"="baseline","Log2 fc"="foldchange","TP"="timepoint", "Platform"="platform_name","Super-pathway"="SUPER.PATHWAY", "Sub-pathway"="SUB.PATHWAY") %>% 
                    dplyr::mutate(new_tp = as.numeric(TP),
                                  new_base = as.numeric(Base)) %>% 
                    dplyr::left_join(info_sample %>% dplyr::select(timepoint, challengeTime), by=c("new_tp"="timepoint")) %>% 
                    dplyr::rename("tp_challengeTime"="challengeTime") %>% 
                    dplyr::left_join(info_sample %>% dplyr::select(timepoint, challengeTime), by=c("new_base"="timepoint")) %>% 
                    dplyr::rename("base_challengeTime"="challengeTime") 
                
                session_store$ttest_table=my_table
                
                # add tooltips to base and table column
                my_table[["Base"]] = sapply(1:nrow(my_table),function(x) paste0(rep_tooltip(title=my_table[["Base"]][x],
                                                                                            tooltip=my_table[["base_challengeTime"]][x])))
                
                my_table[["TP"]] = sapply(1:nrow(my_table),function(x) paste0(rep_tooltip(title=my_table[["TP"]][x],
                                                                                          tooltip=my_table[["tp_challengeTime"]][x])))
                my_table = my_table %>% 
                    dplyr::select(-tp_challengeTime, -base_challengeTime, -new_base, -new_tp)
                # display the table
                DT::datatable(my_table,
                              rownames = F,
                              filter=list(position="top"),escape=F,
                              selection = "none",
                              #callback = DT::JS("$.fn.dataTable.ext.errMode = 'none';setTimeout(function(){table.ajax.reload();}, 3000);"),
                              #selection=list(mode="multiple",target="row", area=F),
                              options=list(dom='fitlp',
                                           # drawCallback=DT::JS("function() {
                                           #                   $('#'+ this[0].id + ' tbody tr').css({'cursor': 'copy'})}"),
                                           columnDefs = list(
                                               list(width = '10%', targets = c(0,2,4,5,6,7)),
                                               list(width = '20%', targets = c(1,3)),
                                               list(searchable = F, targets = c(0))),
                                           pageLength = 15, autoWidth = F)
                )%>%
                    formatSignif(columns=c("p Value", "q Value","Log2 fc"),digits= 2, interval=1)
            }
            else{
                my_table=data.frame("Metabolite"=character(),
                                    "pValue"=numeric(),
                                    "qValue"=numeric(),
                                    "FoldchangeLog2"=numeric(),
                                    "Fluid"=character(),"Platform"=character(),"Baseline"=numeric(),"TP"=numeric(),"SUPER.PATHWAY"=character(),"SUB.PATHWAY"=character())
                session_store$ttest_table=my_table
                DT::datatable(my_table,
                              filter=list(position="top"),escape=F,
                              #callback = DT::JS("$.fn.dataTable.ext.errMode = 'none';setTimeout(function(){table.ajax.reload();}, 3000);"),
                              selection = "none",
                              options=list(dom='fitlp',
                                           # drawCallback=DT::JS("function() {
                                           #                   $('#'+ this[0].id + ' tbody tr').css({'cursor': 'copy'})}"),
                                           columnDefs = list(
                                               list(width = '10%', targets = c(0,2,4,5,6,7)),
                                               list(width = '20%', targets = c(1,3)),
                                               list(searchable = F, targets = c(0))),
                                           pageLength = 15, autoWidth = F)
                ) %>%
                    formatSignif(columns=c("pValue", "qValue","FoldchangeLog2"),digits= 2, interval=1)
            }
        }
    })
    
    ## ttest placeholder ----
    output$stats_ttest_volcano_placeholder<-renderUI({
        # input$main_sel1
        # input$main_sel2
        # input$main_sel3
        # input$main_sel4
        # input$main_sel5
      my_id <- rInput[["sel_id"]]
      only_highlighted <- input[["stats_ttest_met"]]
        #bag_nr=rInput[["bag"]]
        #my_id=input[[paste0("main_sel",bag_nr)]]
        my_data = rTtest$data_filtered
        #type_met = ifelse(input[["stats_ttest_met"]], "bag", "all")

        if(only_highlighted && length(my_id)==0){
            div(tags$b("please select a metabolite"))
        }
        else if(nrow(my_data)==0){
            div(tags$b("No significant results due to correction for multiple testing (change cutoff)"))
        }else{
          div()
        }
    })
    
    output$stats_ttest_table_placeholder<-renderUI({
        my_id=rInput$sel_labels
        my_data = rTtest$data_filtered
        type_met = ifelse(input[["stats_ttest_met"]], "bag", "all")
        if(type_met=="bag" & length(my_id)==0){
            div(tags$b("please select a metabolite"))
        }
        else if(nrow(my_data)==0){
            div(tags$b("No significant results due to correction for multiple testing (change cutoff)"))
        }
    })
    ## ttest volcano ----
    output$stats_ttest_volcano<-renderPlotly({
        input[["refresh_stats_ttest_volcano_box"]] # refresh box
        input[["max_stats_ttest_volcano_box"]]  # maximize box
        
        type_met = ifelse(input[["stats_ttest_met"]], "bag", "all")
        my_data = rTtest$data_filtered
        
        #draw plot
        my_color=input[["stats_ttest_color"]] #list("Annotated super-pathways"="super","Vendor platform"="platform")
        my_tresh <- input[["stats_ttest_thresh"]]  #"no correction"="none","Bonferroni"="bonferroni","FDR"="fdr")
        
        if(nrow(my_data)>=1 & type_met=="bag"){
            stats_volcano_plot=ttest_plot_volcano(x.data=my_data,
                                                  type_thresh = my_tresh,
                                                  colorby=my_color)
            session_store$stats_volcano_plot <- stats_volcano_plot
            session_store$stats_volcano_plot
        }
        else if(nrow(my_data)>=1 & type_met=="all"){
            stats_volcano_plot=ttest_plot_volcano(x.data=my_data,
                                                  type_thresh = my_tresh,
                                                  colorby=my_color)
            session_store$stats_volcano_plot <- stats_volcano_plot
            session_store$stats_volcano_plot
        }else{
            
        }
        
    })
    
    ## plot info ----
    
    output$stats_ttest_volcano_description<-renderUI({
        my_data = rTtest$data_filtered
        if(input[["menu"]]=="stats_ttest" & nrow(my_data)!=0){
            base=rTtest$data$baseline %>% unique() %>% as.numeric()
            nMetabolites=length(rInput$sel_labels)
            tps = rTtest$timepoint[-1]
            tags$span("Figure depicts the metabolic changes of",ifelse(input$stats_ttest_met, paste0(nMetabolites), nrow(info_met)),
                      "metabolite(s) between time point ",paste0(base), "(baseline) and",
                      ifelse(input$stats_ttest_tp=="all", paste0("all other selected(",length(tps),") time points"), paste0("time point ", max(tps, na.rm = T))))
        }
        else{
            
        }
    })
    
    
    
    # Tab: showcase_network --------------------------------------------------------
    observeEvent(input$showcase_networks_longi_load_plot,{
        ## divert when clicking network plot
        updateNavbarPage(session, inputId = "menu",select="network")
        updateTabsetPanel(session, inputId="network_tabbox", selected="mggm")
        updateSliderTextInput(session, inputId = "network_time_slider", selected = as.character(info_sample$challengeTime)[2])
        updatePickerInput(session, inputId="network_node_color", selected="time")
        updateSliderTextInput(session, inputId = "network_time_slider", selected = as.character(info_sample$challengeTime)[2])
    })
    
    observeEvent(input$showcase_networks_cross_load_plot,{
        ## divert when clicking network plot
        updateNavbarPage(session, inputId = "menu",select="network")
        updateTabsetPanel(session, inputId="network_tabbox", selected="sggm")
        updatePickerInput(session, inputId="network_node_color", selected="time")
    })
    
    # Tab: showcase_washout --------------------------------------------------------
    
    output[["showcase_washout_hcplot2"]]<- renderHighchart({
      rData$allData["01090614"] %>%
            browser_process_average(subjects=1:15)%>%
            browser_plot_average_aggregated(type="line",timeLim=c(1,56),ylab="Z score")
    })
    
    output[["showcase_washout_hcplot1"]]<- renderHighchart({
        
        met_id=info_met$ID[which(info_met$labels %in%showcase[["washout"]][["table_new"]][["id"]][1:10])]
        rData$allData[met_id] %>%
            browser_process_average(subjects=1:15)%>%
            browser_plot_average_aggregated(type="line",timeLim=c(1,56),ylab="Z score")
    })
    
    observeEvent(input[["showcase_washout_divertplot1"]],{
        updateNavbarPage(session, inputId = "menu",select="metabdetail")
        updatePickerInput(session, inputId = "browser_plot_display",selected="aggregated")
        updatePickerInput(session, inputId = "main_imputation",selected="none")
        updateSelectizeInput(session, inputId = "main_sel1", selected=info_met$labels[which(info_met$ID %in% showcase$washout$table$id[1:10])])
    })
    observeEvent(input[["showcase_washout_divertplot2"]],{
        updateNavbarPage(session, inputId = "menu",select="metabdetail")
        updatePickerInput(session, inputId = "browser_plot_display",selected="aggregated")
        updatePickerInput(session, inputId = "main_imputation",selected="none")
        updateSelectizeInput(session, inputId = "main_sel1", selected="3-methylhistidine [P, nt-ms]")
    })
    
    output[["showcase_washout_table"]]<-renderDT({
        showcase[["washout"]][["table_new"]] %>% 
             dplyr::rename("Putative exposure"="Putative.exposure","Frechét distance"="Frechét.distance", "Metabolite"="Metabolite.name","Super-pathway"="Super.pathway","Sub-pathway"="Sub.pathway",
                           "Hits in FooDB"="Hits.in.FooDB","Food ingredient FooDB"="Food.ingredient.FooDB", "Potential food ingredient FooDB"="Potential.food.ingredient.FooDB", "Link_fdb"="FooDB.ID",
                           "Hits in Exposome Explorer"="Hits.in.Exposome.Explorer","Food ingredient Exposome Explorer"="Food.ingredient.Exposome.Explorer","Link_e"="Exposome.Explorer.ID") %>%
            #dplyr::rename("Link_fdb"="FooDB ID", "Link_e"="Exposome Explorer ID") %>% 
        dplyr::select(-label, -id) %>% 
            dplyr::mutate(Link_fdb=ifelse(Link_fdb!="",
                                          paste0('<a href="https://foodb.ca/compounds/',Link_fdb,'/" target="_blank" style="color:#d2310b;">
  <span>',Link_fdb,'</span> </a>'),NA)) %>%
            dplyr::mutate(Link_e=ifelse(Link_e!="",
                                        paste0('<a href="http://exposome-explorer.iarc.fr/compounds/',Link_e,'/" target="_blank" style="color:#d2310b;">
  <span>',Link_e,'</span> </a>'),NA)) %>%
            dplyr::rename("Link to FooDB"="Link_fdb", "Link to Exposome Explorer"="Link_e", "Annotated super-pathway"="Super-pathway" , "Annotated sub-pathway"="Sub-pathway") %>% 
            DT::datatable(escape=F,rownames = F, selection = "none", options=list(dom="tlp",pageLength = 5)#,
                          #callback = DT::JS("$.fn.dataTable.ext.errMode = 'none';setTimeout(function(){table.ajax.reload();}, 3000);")
                          )
    })
    # Tab: showcase_platform --------------------------------------------------------
    output[["showcase_platform_plot1"]]<- renderHighchart({
        metId=c("01090870","01010042")
        plot.data=browser_process_average(x.data=rData$allData[metId],subjects=1:15) %>%
            browser_plot_average_aggregated(type="line",timeLim=c(1,56), ylab="Z score")
    })
    
    output[["showcase_platform_plot2"]]<- renderHighchart({
        metId=c("01090715","01010058","01090857")
        plot.data=browser_process_average(x.data=rData$allData[metId],subjects=1:15) %>%
            browser_plot_average_aggregated(type="line",timeLim=c(1,56), ylab="Z score")
    })
    
    
    observeEvent(input[["showcase_platform_divertplot1"]],{
        updateNavbarPage(session, inputId = "menu",select="metabdetail")
        updatePickerInput(session, inputId = "browser_plot_display",selected="aggregated")
        selected=info_met$labels[which(info_met$ID %in% c("01090715","01010058","01090857"))]
        updateSelectizeInput(session, inputId = "main_sel", selected=selected)
    })
    
    observeEvent(input[["showcase_platform_divertplot2"]],{
        updateNavbarPage(session, inputId = "menu",select="metabdetail")
        updatePickerInput(session, inputId = "browser_plot_display",selected="aggregated")
        selected=info_met$labels[which(info_met$ID %in% c("01090870","01010042"))]
        updateSelectizeInput(session, inputId = "main_sel", selected=selected)
    })
    
    # Tab: Download (download) -----------------------------------------------------
    observeEvent(input[["download_bulk_data_divertselection"]],{
      updateNavbarPage(session, inputId = "menu",select="metabselect")
    })
    observeEvent(input[["download_bulk_data_diverttimecourse"]],{
      updateNavbarPage(session, inputId = "menu",select="metabdetail")
    })
  
    
    output$download_bulk_data_info <- renderUI(
             tags$ul(
               tags$li(paste0("Data transformation: ", input[["main_trans"]])),
               tags$li(paste0("Imputation: ",input[["main_imputation"]])),
               tags$li(paste0("Platforms: ", paste0(length(input[["main_platform"]])))),
               tags$li(paste0("Subjects: ", paste0(length(input[["main_subject"]])))),
               tags$li(paste0("Challenges/Time points: ", length(rInput[["timepoint"]])))
             )
    )
    
    output$download_bulk_info_info <- renderUI(
      tags$ul(
        #tags$li(paste0("N metabolites: ", length(info_met$ID[which(info_met$platform_code %in% input[["main_platform"]])]))),
        tags$li(paste0("Platforms: ", paste0(length(input[["main_platform"]]))))
      )
    )
    
    output$postprandial_non_imputed_csv <- downloadHandler(
        filename = function() {
            paste('postprandial_non_imputed.csv')
        },
        content = function(file) {
            readRDS(file="data/download/postprandial_non_imputed_log2.rds") %>% 
                write.csv2(
                    file,
                    row.names=T)
        }
    )
  
    
    output$postprandial_non_imputed_excel <- downloadHandler(
        filename = function() {
            paste('postprandial_non_imputed.xlsx')
        },
        content = function(file) {
          openxlsx::write.xlsx(x = readRDS(file="data/download/postprandial_non_imputed_log2.rds"),
                               file = file)
          
        }
    )
    
    output$postprandial_imputed_csv <- downloadHandler(
        filename = function() {
            paste('postprandial_imputed.csv')
        },
        content = function(file) {
            readRDS(file="data/download/postprandial_imputed_log2.rds") %>% 
                write.csv2(
                    file,
                    row.names=T)
        }
    )
    
    output$postprandial_imputed_excel <- downloadHandler(
        filename = function() {
            paste('postprandial_imputed.xlsx')
        },
        content = function(file) {
          openxlsx::write.xlsx(x = readRDS(file="data/download/postprandial_imputed_log2.rds"),
                               asTable=T,
                               file = file)
        }
    )
    
    output$postprandial_info_csv <- downloadHandler(
        filename = function() {
            paste('postprandial_info.csv')
        },
        content = function(file) {
            readRDS(file="data/download/postprandial_info.rds") %>% 
                write.csv2(
                    file,
                    row.names=T)
        }
    )
    
    output$postprandial_info_excel <- downloadHandler(
        filename = function() {
            paste('bile_acid_info.xlsx')
        },
        content = function(file) {
          openxlsx::write.xlsx(x=read.csv2(file="data/download/bile_acid_info_met.csv"),
                               file = file)
        }
    )
    
    output$bile_acid_info_csv <- downloadHandler(
        filename = function() {
            paste('bile_acid_info.csv')
        },
        content = function(file) {
            read.csv2(file="data/download/bile_acid_info_met.csv") %>% 
                write.csv2(
                    file,
                    row.names=T)
        }
    )
    
    output$bile_acid_info_excel <- downloadHandler(
        filename = function() {
            paste('bile_acid_info.xlsx')
        },
        content = function(file) {
          openxlsx::write.xlsx(x=read.csv2(file="data/download/bile_acid_info_met.csv"),
                               overwrite = TRUE,
                               file)
        }
    )
    
    output$bile_acid_csv <- downloadHandler(
        filename = function() {
            paste('bile_acid_data.csv')
        },
        content = function(file) {
            read.csv2(file="data/download/bile_acid_data.csv") %>% 
                write.csv2(
                    file,
                    row.names=T)
        }
    )
    
    output$bile_acid_excel <- downloadHandler(
        filename = function() {
            paste('bile_acid_data.xlsx')
        },
        content = function(file) {
          openxlsx::write.xlsx(x = read.csv2(file="data/download/bile_acid_data.csv"),
                           file)
        }
    )

    # Download handlers -----
    ## Selection -----
    output$selection_simTable_csv <- downloadHandler(
        filename = function() {
            dist_name=switch(input$search_sim_table_dist,
                             "frechet"="Frechet distance",
                             "frechet_mean"="average Frechet distance",
                             "pearson"="Pearson correlation",
                             "pearson_mean"="average Pearson correlation",
                             "euclidean"="Euclidean distance",
                             "manhattan"="Manhattan distance")
            met_name=input$search_sim_table_refMet
            paste(dist_name,"(",met_name,")",'.csv', sep='')
        },
        content = function(file) {
            table=rSearch[["simTable"]] %>% 
              dplyr::select(-any_of(c("ID","labels"," "))) %>% 
              dplyr::mutate(ChEBI = ChEBI %>% rep_link_rm(type="ChEBI"))%>% 
              dplyr::mutate(PubChem = PubChem %>% rep_link_rm(type="PubChem"))
            write.csv2(table, file, row.names = T)
        }
    )
    
    output$selection_simTable_excel <- downloadHandler(
      filename = function() {
        dist_name=switch(input$search_sim_table_dist,
                         "frechet"="Frechet distance",
                         "frechet_mean"="average Frechet distance",
                         "pearson"="Pearson correlation",
                         "pearson_mean"="average Pearson correlation",
                         "euclidean"="Euclidean distance",
                         "manhattan"="Manhattan distance")
        met_name=input$search_sim_table_refMet
        paste(dist_name,"(",met_name,")",'.xlsx', sep='')
      },
      content = function(file) {
        table=rSearch[["simTable"]] %>% 
          dplyr::select(-any_of(c("ID","labels"," "))) %>% 
          dplyr::mutate(ChEBI = ChEBI %>% rep_link_rm(type="ChEBI"))%>% 
          dplyr::mutate(PubChem = PubChem %>% rep_link_rm(type="PubChem"))
        # writexl::write_xlsx(x=table, path=file)
        openxlsx::write.xlsx(x = table,
                             file = file)
      }
    )
    
    ## Statistics ----
    output$stats_ttest_volcano_html <- downloadHandler(
        filename = function() {
            n_mets=length(unique((session_store$ttest_table$Metabolite)))
            cut=input[["stats_ttest_thresh"]]
            cut=switch(cut,
                       "none"="none",
                       "bonferroni"="Bonferroni",
                       "fdr"="FDR")
            paste0('ttest_volcano_N(',n_mets,"),cutoff(",cut,") .html")
        },
        content = function(file) {
                htmlwidgets::saveWidget(session_store$stats_volcano_plot, file, selfcontained = T)
        }
    )
    
    output$stats_ttest_table_excel <- downloadHandler(
        filename = function() {
            n_mets=length(unique(session_store$ttest_table$Metabolite))
            cut=switch(input[["stats_ttest_thresh"]],
                       "none"="none",
                       "bonferroni"="Bonferroni",
                       "fdr"="FDR")
            paste0('ttest_results_n(',n_mets,"),cutoff(",cut,").xlsx")
        },
        content = function(file) {
          save_cols <- c("Metabolite","Base","TP","p Value", "q Value","Log2 fc","Fluid","Platform","Super-pathway","Sub-pathway", "base_challengeTime","tp_challengeTime")
            table=session_store$ttest_table %>%
              dplyr::select(any_of(save_cols))
            if(nrow(table)<=1){
                table=data.frame(Metabolite=character(),
                                 'p Value' = numeric(),
                                 'Log2 fc' = numeric(),
                                 'Fluid' = character(),
                                 'Platform' = character(),
                                 'Baseline' = numeric(),
                                 'Time point' = numeric(),
                                 "Super-pathway"= character(),
                                 "Sub-pathway"= character()
                )
            }
            
           # writexl::write_xlsx(x=table,path=file)
            openxlsx::write.xlsx(x = table,
                                 file = file)
        }
    )
    output$stats_ttest_table_csv <- downloadHandler(
      filename = function() {
        n_mets=length(input[["main_sel"]])
        cut=switch(input[["stats_ttest_thresh"]],
                   "none"="none",
                   "bonferroni"="Bonferroni",
                   "fdr"="FDR")
        paste0('ttest_results_n(',n_mets,"),cutoff(",cut,").csv")
      },
      content = function(file) {
        save_cols <- c("Metabolite","Base","TP","p Value", "q Value","Log2 fc","Fluid","Platform","Super-pathway","Sub-pathway", "base_challengeTime","tp_challengeTime")
        table=session_store$ttest_table %>%
          dplyr::select(any_of(save_cols))
        if(nrow(table)<=1){
          table=data.frame(Metabolite=character(),
                           'p Value' = numeric(),
                           'Log2 fc' = numeric(),
                           'Fluid' = character(),
                           'Platform' = character(),
                           'Baseline' = numeric(),
                           'Time point' = numeric(),
                           "Super-pathway"= character(),
                           "Sub-pathway"= character()
          )
        }
        write.csv2(x=table,
                   file,
                   row.names=F)
      }
    )
    #   
    output$stats_pca_scores_html <- downloadHandler(
        filename = function() {
            paste0('humet_pca_scores_',Sys.Date(),".html")
        },
        content = function(file) {
            visNetwork::visSave(session_store$stats_scores_plot, file, selfcontained = T)
        }
    )
    
    output$stats_pca_loadings_html <- downloadHandler(
        filename = function() {
            paste0('pca_loadings_',Sys.Date(),".html")
        },
        content = function(file) {
            visNetwork::visSave(session_store$stats_loadings_plot, file, selfcontained = T)
        }
    )
    
    ## Networks ----
      output$network_sgmm_download_html <- downloadHandler(
        filename = function() {
          #network
          paste('network_single_fluid_ggm_',Sys.Date(), '.html', sep='')
        },
        content = function(con) {
          if(input$network_node_color=="time"){
            network=rNetwork[["basic"]]
            network$x$nodes = network$x$nodes %>%
              network_color_time_proxy(highlightTime =  rNetwork[["time"]])
            network = network %>%
              network_add_subtitle(subtitle = network_subtitle_proxy(timepoint =rNetwork[["time"]]))
            network%>% visExport() %>% visPhysics(enabled = FALSE) %>% visEdges(smooth = FALSE) %>% visSave(con)
          }else{
            rNetwork[["basic"]] %>% visExport() %>% visPhysics(enabled = FALSE) %>% visEdges(smooth = FALSE) %>% visSave(con)
          }
        }
      )

      output$network_sggm_download_csv <- downloadHandler(
        filename = function() {
          paste('edges_single_fluid_ggm_',Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv2(x=rNetwork[["basic"]]$x$edges %>% download_network_edges(),
                    file,
                    row.names=T)
        }
      )

      output$network_sggm_download_excel <- downloadHandler(
        filename = function() {
          paste('edges_single_fluid_ggm_',Sys.Date(), '.xlsx', sep='')
        },
        content = function(file) {
          # writexl::write_xlsx(x=rNetwork[["basic"]]$x$edges %>% download_network_edges(),
          #                     path=file)
          openxlsx::write.xlsx(x = rNetwork[["basic"]]$x$edges %>% download_network_edges(),
                               file = file)
        }
      )
      output$network_mgmm_download_html <- downloadHandler(
        filename = function() {
          #network
          paste('network_multi_fluid_ggm_',Sys.Date(), '.html', sep='')
        },
        content = function(con) {
          if(input$network_node_color=="time"){
            network=rNetwork[["basic"]]
            network$x$nodes = network$x$nodes %>%
              network_color_time_proxy(highlightTime =  rNetwork[["time"]])
            network = network %>%
              network_add_subtitle(subtitle = network_subtitle_proxy(timepoint =rNetwork[["time"]]))
            network%>% visExport() %>% visPhysics(enabled = FALSE) %>% visEdges(smooth = FALSE) %>% visSave(con)
          }else{
            rNetwork[["basic"]] %>% visExport() %>% visPhysics(enabled = FALSE) %>% visEdges(smooth = FALSE) %>% visSave(con)
          }
        }
      )

      output$network_mggm_download_csv <- downloadHandler(
        filename = function() {
          paste('edges_multi_fluid_ggm_',Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv(x=rNetwork[["basic"]]$x$edges %>% download_network_edges(),
                    file,
                    row.names=F)
        }
      )
      output$network_mggm_download_excel <- downloadHandler(
        filename = function() {
          paste('edges_multi_fluid_ggm_',Sys.Date(), '.xlsx', sep='')
        },
        content = function(file) {
          # writexl::write_xlsx(x=rNetwork[["basic"]]$x$edges %>% download_network_edges(),
          #                     path=file)
          openxlsx::write.xlsx(x = rNetwork[["basic"]]$x$edges %>% download_network_edges(),
                               file = file)
        }
      )

      output$network_vendor_download_html <- downloadHandler(
        filename = function() {
          #network
          paste('network_vendor_',Sys.Date(), '.html', sep='')
        },
        content = function(con) {
          if(input$network_node_color=="time"){
            network=rNetwork[["basic"]]
            network$x$nodes = network$x$nodes %>%
              network_color_time_proxy(highlightTime =  rNetwork[["time"]])
            network = network %>%
              network_add_subtitle(subtitle = network_subtitle_proxy(timepoint =rNetwork[["time"]]))
            network%>% visExport() %>% visPhysics(enabled = FALSE) %>% visEdges(smooth = FALSE) %>% visSave(con)
          }else{
            rNetwork[["basic"]] %>% visExport() %>% visPhysics(enabled = FALSE) %>% visEdges(smooth = FALSE) %>% visSave(con)
          }
        }
      )

      output$network_vendor_download_csv <- downloadHandler(
        filename = function() {
          paste('edges_vendor_',Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          write.csv(x=rNetwork[["basic"]]$x$edges %>% download_network_edges(),
                    file,
                    row.names=F)
        }
      )
      
      output$network_vendor_download_excel <- downloadHandler(
        filename = function() {
          paste('edges_vendor_',Sys.Date(), '.xlsx', sep='')
        },
        content = function(file) {
          # writexl::write_xlsx(x=rNetwork[["basic"]]$x$edges %>% download_network_edges(),
          #                     path=file)
          openxlsx::write.xlsx(x = rNetwork[["basic"]]$x$edges %>% download_network_edges(),
                               file = file)
        }
      )
      
      output$download_browser_data_xlsx <- downloadHandler(
        filename = function() {
          paste('humet_plot_data_',Sys.Date(), '.xlsx', sep='')
        },
        content = function(file) {
          # get inputs
          my_subjects=rInput$subject
          # met_name = list(bag1 = input[["main_sel1"]],
          #                 bag2 = input[["main_sel2"]],
          #                 bag3 = input[["main_sel3"]],
          #                 bag4 = input[["main_sel4"]],
          #                 bag5 = input[["main_sel5"]])
          browser_data=rData[["allData"]]
          display = input[["browser_plot_display"]]
          # met_labels <- unlist(met_name) %>% unique()
          # met_ids <- info_met$ID[which(info_met$labels %in% met_labels)]
          met_ids <- rInput$sel_id
          # process data
          download_data=data.frame()
          out=NULL
          if(length(met_ids)>=1){
            download_data <- lapply(met_ids,function(y){
              if(display %in% c("errorBar", "minMax","aggregated") & length(y)==1){
                this_data_out<-browser_process_average(x.data=browser_data[y],subjects=my_subjects) %>% .[[1]] %>% dplyr::select(timepoints,mean) %>% dplyr::mutate(subject="average")
                names(this_data_out) <- c("timepoints",info_met$labels[which(info_met$ID ==y )], "subject")
                this_data_out
              }else if(display == "individual"& length(y)==1){
                this_data <- browser_process_single(x.data=browser_data[y],subjects=my_subjects) %>% .[[1]]
                this_data_out <- lapply(setdiff(names(this_data),"timepoints"), function(split_data) data.frame(timepoints=this_data$timepoints, mean=this_data[[split_data]], subject = split_data))
                this_data_out <- do.call(what=rbind, this_data_out)
                names(this_data_out) <- c("timepoints",info_met$labels[which(info_met$ID ==y )], "subject")
                this_data_out
              }else{
              }
            })
            out = download_data[[1]] %>%
              dplyr::left_join(info_sample[,c("plot_timepoint","timepoint","day","day_time","challengeTime")],by=c("timepoints"="plot_timepoint")) %>% 
              dplyr::select(all_of(setdiff(names(.[]),info_met$labels)), all_of(intersect(names(.[]), info_met$labels))) %>% 
              dplyr::select(-timepoints)
          }
          
          if(length(download_data)>1){
            out <- download_data[[1]]
            for(i in 2:length(download_data)){
              out <- out %>% 
                dplyr::full_join(download_data[[i]],by=c("timepoints"="timepoints","subject"="subject"))
            }
            out = out %>% 
              dplyr::left_join(info_sample[,c("plot_timepoint","timepoint","day","day_time","challengeTime")],by=c("timepoints"="plot_timepoint")) %>% 
              dplyr::select(all_of(setdiff(names(.[]),info_met$labels)), all_of(intersect(names(.[]), info_met$labels)))%>% 
              dplyr::select(-timepoints)
          }
          #writexl::write_xlsx(x=out,path=file)
          openxlsx::write.xlsx(x = out,
                               file = file)
        })
      
      output$download_browser_data_csv <- downloadHandler(
        filename = function() {
          paste('humet_plot_data_',Sys.Date(), '.csv', sep='')
        },
        content = function(file) {
          # get inputs
          my_subjects=rInput$subject
          # met_name = list(bag1 = input[["main_sel1"]],
          #                 bag2 = input[["main_sel2"]],
          #                 bag3 = input[["main_sel3"]],
          #                 bag4 = input[["main_sel4"]],
          #                 bag5 = input[["main_sel5"]])
          browser_data=rData[["allData"]]
          display = input[["browser_plot_display"]]
          # met_labels <- unlist(met_name) %>% unique()
          # met_ids <- info_met$ID[which(info_met$labels %in% met_labels)]
          met_ids <- rInput$sel_id
          # process data
          download_data=data.frame()
          out=NULL
          if(length(met_ids)>=1){
            download_data <- lapply(met_ids,function(y){
              if(display %in% c("errorBar", "minMax","aggregated") & length(y)==1){
                this_data_out<-browser_process_average(x.data=browser_data[y],subjects=my_subjects) %>% .[[1]] %>% dplyr::select(timepoints,mean) %>% dplyr::mutate(subject="average")
                names(this_data_out) <- c("timepoints",info_met$labels[which(info_met$ID ==y )], "subject")
                this_data_out
              }else if(display == "individual"& length(y)==1){
                this_data <- browser_process_single(x.data=browser_data[y],subjects=my_subjects) %>% .[[1]]
                this_data_out <- lapply(setdiff(names(this_data),"timepoints"), function(split_data) data.frame(timepoints=this_data$timepoints, mean=this_data[[split_data]], subject = split_data))
                this_data_out <- do.call(what=rbind, this_data_out)
                names(this_data_out) <- c("timepoints",info_met$labels[which(info_met$ID ==y )], "subject")
                this_data_out
              }else{
              }
            })
            out = download_data[[1]] %>%
              dplyr::left_join(info_sample[,c("plot_timepoint","timepoint","day","day_time","challengeTime")],by=c("timepoints"="plot_timepoint")) %>% 
              dplyr::select(all_of(setdiff(names(.[]),info_met$labels)), all_of(intersect(names(.[]), info_met$labels)))%>% 
              dplyr::select(-timepoints)
          }
          
          if(length(download_data)>1){
            out <- download_data[[1]]
            for(i in 2:length(download_data)){
              out <- out %>% 
                dplyr::full_join(download_data[[i]],by=c("timepoints"="timepoints","subject"="subject"))
            }
            out = out %>% 
              dplyr::left_join(info_sample[,c("plot_timepoint","timepoint","day","day_time","challengeTime")],by=c("timepoints"="plot_timepoint")) %>% 
              dplyr::select(all_of(setdiff(names(.[]),info_met$labels)), all_of(intersect(names(.[]), info_met$labels)))%>% 
              dplyr::select(-timepoints)
          }
          
          write.csv(x=out,
                    file,
                    row.names=F)
        }
      )
      

      output$download_bluk_data_csv <- downloadHandler(
        filename = function() {
          paste('humet_data_',
                input[["main_trans"]],"_",
                input[["main_imputation"]], "_",
                "subjects",length(input[["main_subject"]]), "_",
                "tp",length(rInput[["timepoint"]]),
                '.csv', sep='')
        },
        content = function(file) {
          showModal(modalDialog("Compiling data ... (May take a few seconds)", footer=NULL)) 
          on.exit(removeModal())
          this_subject <- as.numeric(input[["main_subject"]])
          this_timepoint <- as.numeric(rInput[["timepoint"]])
          id <-info_met$ID[which(info_met$platform_code %in% input[["main_platform"]])]
          
          out <- lapply(id, function(x){
            rData[["allData"]][[x]] %>% 
              dplyr::mutate(time = substr(x=rownames(.[]),start=3,stop=4)) %>% 
              reshape2::melt(id.vars="time",variable.name="subject",value.name=info_met$labels[which(info_met$ID==x)])%>% 
              dplyr::mutate(subject=as.numeric(subject), time=as.numeric(time))
          }) %>% 
            plyr::join_all(by=c("time"="time","subject"="subject")) %>% 
            dplyr::filter(subject %in% this_subject,
                          time %in% this_timepoint)
          #make sure that output of lipidyzer only has subject 5-8
          out[which(out$subject %in% c(1:4, 9:15)),which(names(out) %in% info_met$labels[which(info_met$platform_name=="Lipidyzer [nt-ms]")])]  = NA
          my_workbook <- createWorkbook()
          addWorksheet(wb = my_workbook,sheetName = "data")
          writeData(my_workbook,sheet = 1, x=out)
          saveWorkbook(my_workbook, file = file)
          
          #openxlsx::write.xlsx(x = out,file = file)
        }
      )
      
      output$download_bluk_data_xlsx <- downloadHandler(
        filename = function() {
          paste('humet_data_',
                input[["main_trans"]],"_",
                input[["main_imputation"]], "_",
                "subjects",length(input[["main_subject"]]), "_",
                "tp",length(rInput[["timepoint"]]),
                '.xlsx', sep='')
        },
        content = function(file) {
          showModal(modalDialog("Compiling data ... (May take a few seconds)", footer=NULL)) 
          on.exit(removeModal())
          this_subject <- as.numeric(input[["main_subject"]])
          this_timepoint <- as.numeric(rInput[["timepoint"]])
          id <-info_met$ID[which(info_met$platform_code %in% input[["main_platform"]])]

          out <- lapply(id, function(x){
            rData[["allData"]][[x]] %>% 
              dplyr::mutate(time = substr(x=rownames(.[]),start=3,stop=4)) %>% 
              reshape2::melt(id.vars="time",variable.name="subject",value.name=info_met$labels[which(info_met$ID==x)])%>% 
              dplyr::mutate(subject=as.numeric(subject), time=as.numeric(time))
          }) %>% 
            plyr::join_all(by=c("time"="time","subject"="subject")) %>% 
            dplyr::filter(subject %in% this_subject,
                          time %in% this_timepoint)
          #make sure that output of lipidyzer only has subject 5-8
          out[which(out$subject %in% c(1:4, 9:15)),which(names(out) %in% info_met$labels[which(info_met$platform_name=="Lipidyzer [nt-ms]")])]  = NA
          write.csv(x=out, file, row.names=F)
        }
      )
      
      
      output$download_bluk_info_xlsx <- downloadHandler(
        filename = function() {
          paste('humet_info.xlsx')
        },
        content = function(file) {
          showModal(modalDialog("Loading", footer=NULL))
          on.exit(removeModal())
          out <-info_met[which(info_met$platform_code %in% input[["main_platform"]]),] %>% 
            dplyr::select(Metabolite, SUPER.PATHWAY, SUB.PATHWAY, Fluid, platform_name, Axis, RI, CAS,pubchem, chebi) %>% 
            dplyr::mutate(Fluid=ifelse(Fluid == "U", "urine",Fluid)) %>% 
            dplyr::mutate(Fluid=ifelse(Fluid == "P", "plasma",Fluid)) %>% 
            dplyr::mutate(Fluid=ifelse(Fluid == "BA", "breath air",Fluid)) %>% 
            dplyr::mutate(Fluid=ifelse(Fluid == "BC", "breath condensate",Fluid)) %>% 
            dplyr::rename("metabolite"="Metabolite", "super_pathway"="SUPER.PATHWAY", "sub_pathway"="SUB.PATHWAY", "fluid"="Fluid","platform_unit"= "Axis", "retention_index"="RI","PubChem"="pubchem","ChEBI"="chebi")
        
          #writexl::write_xlsx(x=out,path=file)
          openxlsx::write.xlsx(x = out,
                               file = file)
        }
      )
      
      output$download_bluk_info_csv <- downloadHandler(
        filename = function() {
          paste('humet_info',
                '.csv', sep='')
        },
        content = function(file) {
          showModal(modalDialog("Loading", footer=NULL))
          on.exit(removeModal())
          out <-info_met[which(info_met$platform_code %in% input[["main_platform"]]),] %>% 
            dplyr::select(Metabolite, SUPER.PATHWAY, SUB.PATHWAY, Fluid, platform_name, Axis, RI, CAS,pubchem, chebi) %>% 
            dplyr::mutate(Fluid=ifelse(Fluid == "U", "urine",Fluid)) %>% 
            dplyr::mutate(Fluid=ifelse(Fluid == "P", "plasma",Fluid)) %>% 
            dplyr::mutate(Fluid=ifelse(Fluid == "BA", "breath air",Fluid)) %>% 
            dplyr::mutate(Fluid=ifelse(Fluid == "BC", "breath condensate",Fluid)) %>% 
            dplyr::rename("metabolite"="Metabolite", "super_pathway"="SUPER.PATHWAY", "sub_pathway"="SUB.PATHWAY", "fluid"="Fluid","platform_unit"= "Axis", "retention_index"="RI","PubChem"="pubchem","ChEBI"="chebi")
          
          write.csv(x=out,
                    file,
                    row.names=F)
        }
      )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
