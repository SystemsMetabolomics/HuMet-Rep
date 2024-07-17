#### header  ----------------------------
header = shinydashboardPlus::dashboardHeader(
  title = p(tags$span("HuMet", style="color:#d2310b"), tags$span("Repository")),
  fixed = TRUE,
  controlbarIcon=NULL,
  leftUi = tagList(
    div(class="dropdown ", style = "width:100%;height:42.5px",`data-display-if` = 'input.menu=="metabdetail" || input.menu=="network"|| input.menu=="metabselect" || input.menu=="stats_pca" || input.menu=="stats_ttest"',
        div(style="display:inline-block",
            div(style="display:inline-block",
                actionButton(inputId="main_sel_bagsel_1", label=uiOutput("main_sel_bagname_1"), class="btn-bagsel-active btn-bagsel")
            ),
            div(style="display:inline-block",
                actionButton(inputId="main_sel_bagsel_2", label=uiOutput("main_sel_bagname_2"), class="btn-bagsel")),
            div(style="display:inline-block",`data-display-if` = 'input.main_sel_bag_add > "0"',
                actionButton(inputId="main_sel_bagsel_3", label=uiOutput("main_sel_bagname_3"), class="btn-bagsel")),
            div(style="display:inline-block",`data-display-if` = 'input.main_sel_bag_add > "1"',
                actionButton(inputId="main_sel_bagsel_4", label=uiOutput("main_sel_bagname_4"), class="btn-bagsel")),
            div(style="display:inline-block",`data-display-if` = 'input.main_sel_bag_add > "2"',
                actionButton(inputId="main_sel_bagsel_5", label=uiOutput("main_sel_bagname_5"), class="btn-bagsel"))
        ),
        div(style="display:inline-block",`data-display-if` = 'input.main_sel_bag_add < "3"',
            actionButton(inputId="main_sel_bag_add", label="+ bag", class="btn-bagadd")
        )
    ),
    div(radioButtons(inputId="main_sel_bagselector",label="Dummy",choices=1:5, selected=1),style="display:none")
  ),
  mt_header_dropdown_new(label=uiOutput("header_prep_title"), 
                         icon="database",
                         div(style="width:max-content;",
                             div(style="display:block; float:left;max-width:300px;font-size:12px;",
                                 conditionalPanel(condition='input.menu=="metabdetail"',
                                                  div(
                                                    #tags$b("Data transformation", style="margin-bottom:10px;font-size:14px;"),
                                                    prettyRadioButtons(inputId = "main_trans", shape="round",outline=T, status="danger",label = "Data transformation", 
                                                                       selected = "zscore",inline = F,
                                                                       choices = list("Concentrations and relative abundances"="raw","Z-scored data" = "zscore","log2 fold change block*"="fcblock", "log2 fold change challenge"="fcchal")),
                                                    div(
                                                      style="color:grey; font-size:12px",
                                                      tags$p("*Log2 fold changes are calculated between block baseline and respective time points.")
                                                      
                                                    )
                                                  )
                                 ),
                                 conditionalPanel(condition='input.menu=="metabselect"',
                                                  div(style="color:lightgrey; pointer-events:none;",
                                                    prettyRadioButtons(inputId = "main_trans_metabselect", shape="round",outline=T, status="danger",label = "Data transformation*", 
                                                                       selected = "zscore",inline = F,
                                                                       choices = list("Concentrations and relative abundances"="raw","Z-scored data" = "zscore","log2 fold change block*"="fcblock", "log2 fold change challenge"="fcchal")),
                                                    div(
                                                      style="color:grey; font-size:12px", #asdf
                                                      tags$p("*Data transformation is deactivated. Z-scored data used to calculate distances for similarity search")
                                                      
                                                    )
                                                  )
                                 ),
                                 conditionalPanel(condition='input.menu=="stats_ttest"',
                                                  div(style="color:lightgrey; pointer-events:none;",
                                                      prettyRadioButtons(inputId = "dummy_trans_ttest", shape="round",outline=T, status="danger",label = "Data transformation*",selected = "raw",inline = F,
                                                                         choices = list("Concentrations and relative abundances"="raw","Z-scored data" = "zscore","log2 fold change block"="fcblock","log2 fold change challenge"="fcchal")),
                                                      tags$p('Data transformation is deactivated. t Tests are calculated on log2 transformed data')
                                                  )
                                 ),
                                 conditionalPanel(condition='input.menu=="network"',
                                                  div(style="color:lightgrey; pointer-events:none;",
                                                      prettyRadioButtons(inputId = "dummy_trans_network", shape="round",outline=T, status="danger",label = "Data transformation*",selected = "raw",inline = F,
                                                                         choices = list("Concentrations and relative abundances"="raw","Z-scored data" = "zscore","log2 fold change block"="fcblock","log2 fold change challenge"="fcchal")),
                                                      tags$p('*Data transformation is deactivated. GGMs are generated on log2 transformed data')
                                                  )
                                 ),
                                 conditionalPanel(condition='input.menu=="stats_pca"',
                                                  div(style="color:lightgrey; pointer-events:none;",
                                                      prettyRadioButtons(inputId = "dummy_trans_pca", shape="round",outline=T, status="danger",label = "Data transformation*",selected = "raw",inline = F,
                                                                         choices = list("Concentrations and relative abundances"="raw","Z-scored data" = "zscore","log 2 fold change"="fc")),
                                                      tags$p('* Data transformation is deactivated. PCA is calculated on imputed log transformed data')
                                                  )
                                 )
                             ),
                             div(style="display:block; float:left;max-width:300px;font-size:12px;",
                                 #h4("Imputation",style="text-align:center"),
                                 conditionalPanel(condition='input.menu=="stats_ttest" || input.menu=="metabselect" || input.menu=="metabdetail"',
                                                  div(
                                                    #tags$b("missForest imputation", style="margin-bottom:10px;font-size:14px;"),
                                                    #switchInput(inputId = "main_imp", label=NULL, value=FALSE, onLabel="on", offLabel="off"),
                                                    prettyRadioButtons(inputId = "main_imputation", shape="round",outline=T, status="danger",label = "Data imputation",selected = "none",inline = F,
                                                                       choices = list("none"="none","random forest" = "rf","knn-obs-sel"="knn","linear"="linear", "pmm"="pmm")),
                                                    conditionalPanel(condition='input.main_imputation=="none"',tags$span("Missing values were not imputed",style="font-size:12px; color:grey;")),
                                                    conditionalPanel(condition='input.main_imputation=="rf"',tags$p("*Nonparametric missing value imputation using random forest (missForest, R Package missForest 1.4)",  
                                                                                                                    a(href="https://cran.r-project.org/web/packages/missForest",target="_blank", rep_ui_icon("info-circle"), style="color:inherit"),
                                                                                                                    style="font-size:12px; color:grey;")),
                                                    conditionalPanel(condition='input.main_imputation=="knn"',tags$p("*Imputed by k nearest neighbor (KNN) imputation (mt_pre_impute_knn, R Package maplet 1.0). Missingness of a complete sample can't be imputed using this method.",
                                                                                                                     a(href="https://github.com/krumsieklab/maplet/tree/main",target="_blank", rep_ui_icon("info-circle"), style="color:inherit"),
                                                                                                                     style="font-size:12px; color:grey;")),
                                                    conditionalPanel(condition='input.main_imputation=="linear"',tags$span("Linear imputation using the approx function with parameters set to method='linear'. Values were imputed per subject per block and only if the previous and subsequent time point are available. R Package version: stats 4.2.3. Missingness of a complete sample can't be imputed using this method.",
                                                                                                                           style="font-size:12px; color:grey;")),
                                                    conditionalPanel(condition='input.main_imputation=="pmm"',tags$span("Missing values were imputed by predictive mean matching (PMM) (futuremmice, R package mice 3.15.0).",
                                                                                                                        a(href="https://cran.r-project.org/web/packages/mice/index.html",target="_blank", rep_ui_icon("info-circle"), style="color:inherit"),
                                                                                                                        style="font-size:12px; color:grey;"))
                                                                                                      )
                                 ),
                                 conditionalPanel(condition='input.menu=="network"',
                                                  div(style="color:lightgrey; pointer-events:none;",
                                                      prettyRadioButtons(inputId = "dummy_imputation_network", shape="round",outline=T, status="danger",label = "Data imputation",selected = "rf",inline = F,
                                                                         choices = list("none"="none","random forest" = "rf","knn-obs-sel"="knn","linear"="linear","pmm"="pmm")),
                                                      tags$p("*Deactivated.All platforms and log2 transformed and imputed data was used as default within this network module. This data processing was chosen as it is most applicable for longitudinal network inferrance.")
                                                  )
                                 ),
                                 conditionalPanel(condition='input.menu=="stats_pca"',
                                                  div(style="color:lightgrey; pointer-events:none;",
                                                      prettyRadioButtons(inputId = "dummy_imputation_pca", shape="round",outline=T, status="danger",label = "Data imputation",selected = "rf",inline = F,
                                                                         choices = list("none"="none","random forest" = "rf","knn-obs-sel"="knn","linear"="linear")),
                                                      
                                                      tags$p("*Deactivated. Only imputed data was used to calculate the PCA")
                                                  )
                                 )
                                 
                                 
                             )
                         )
  ),
  mt_header_dropdown_new(label=uiOutput("header_platform_title"),
                         icon="cube",
                         div(
                           #h4("Platforms",style="text-align:center"),
                           div(style="font-size:12px;display:flex;flex-direction:row;width:fit-content;",
                               div(style="width:230px;color:grey;pointer-events:none;",`data-display-if`='input.menu=="network"',
                                   awesomeCheckboxGroup(inputId = "main_network_platform", label="Platforms network*",choices=options[["general"]][["platform_choices"]],
                                                        selected=c("[P, nt-ms]", "[U, nt-ms]","[P, t-ms]", "[P, chem.]"),inline = FALSE, status = "danger")),
                               div(style="width:230px;color:grey;pointer-events:none;",`data-display-if`='input.menu=="stats_pca"',
                                   awesomeCheckboxGroup(inputId = "main_stats_pca_platform", label="Platforms PCA*",
                                                        choices=options[["general"]][["platform_choices"]],
                                                        selected=c("[P, nt-ms]", "[P, t-ms]", "[P, chem.]", "[P, NMR]"),
                                                        inline = FALSE, status = "danger")),
                               div(style="width:230px;",`data-display-if`='input.menu=="metabselect"||input.menu=="metabdetail"||input.menu=="stats_ttest"',
                                   tags$b("Platforms"),br(),
                                   div(
                                     style="margin-top:-2px;",
                                     awesomeCheckboxGroup(inputId = "main_platform", label= NULL,#"Platforms",
                                                          choices=options[["general"]][["platform_choices"]],
                                                          selected=options[["general"]][["platform_choices"]] %>% unlist(),
                                                          inline = FALSE, status = "danger"))
                                   ),
                               div(style="overflow:auto;width:max-content; max-width:600px;white-space:no-wrap",
                                   tableHTML::tableHTML(options$general$platform %>% select(Type, Medium, Subjects, Time, Metabolites), 
                                                        headers = c("Type","Fluid","Subjects","Timepoints","Metabolites"), 
                                                        rownames=F,border=0, spacing="3px", round=1, collapse="separate_shiny", widths = c("250px","120px","40px","90px","40px"))
                               )
                           )
                         ),
                         div(`data-display-if`='input.menu=="network"', tags$span("*Choice of data platforms is deactivated. All platforms and log2 transformed and imputed data was used as default within this network module. This data processing was chosen as it is most applicable for longitudinal network inferrance.",style="color:grey;font-size:12px")),
                         div(`data-display-if`='input.menu=="stats_pca"', tags$span("*Choice of data platforms is deactivated. All platforms are used.",style="color:grey;font-size:12px"))
  ),
  mt_header_dropdown_new(
    label= uiOutput("header_subject_title"),
    div(
      #h3("Study participants",style="text-align:center;width:max-content"),
      div(tags$b("Study participants",style="margin-bottom:10px; font-size:14px;height:40px;")),
      div(style="font-size:12px;display:flex;",
          
          conditionalPanel(condition='input.menu=="metabselect"||input.menu=="metabdetail"|| input.menu=="stats_pca" ',
                           style="width:300px;",
                           div(
                             tags$b("Study participants"),
                             awesomeCheckboxGroup(inputId = "main_subject", label=NULL,
                                                  choices =list("01"=1,"02"=2,"03"=3,"04"=4,"05"=5,"06"=6,"07"=7, "08"=8, "09"=9, "10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15), 
                                                  selected = 1:15,inline = F, status = "danger")
                           )
          ),
          conditionalPanel(condition='input.menu=="stats_ttest"',
                           style="width:300px;",
                           div(style="color:lightgrey; pointer-events:none;",
                               tags$b("Study participants"),
                               awesomeCheckboxGroup(inputId = "dummy_subject_ttest", label=NULL,
                                                    choices =list("01"=1,"02"=2,"03"=3,"04"=4,"05"=5,"06"=6,"07"=7, "08"=8, "09"=9, "10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15), 
                                                    selected = 1:15,inline = F, status = "danger"),
                               tags$p("*Deactivated. All study participants were used to calculate paired t Tests.")
                           )
          ),
          conditionalPanel(condition='input.menu=="network"',
                           style="width:300px;",
                           div(style="color:lightgrey; pointer-events:none;",
                               tags$b("Study participants"),
                               awesomeCheckboxGroup(inputId = "dummy_subject_network", label=NULL,
                                                    choices =list("01"=1,"02"=2,"03"=3,"04"=4,"05"=5,"06"=6,"07"=7, "08"=8, "09"=9, "10"=10,"11"=11,"12"=12,"13"=13,"14"=14,"15"=15), 
                                                    selected = 1:15,inline = F, status = "danger"),
                               tags$p("*Deactivated. All study participants were used within this network module. Participants were not excluded as longitudinal network inferrance is more accurate with growing n (participants).")
                           )
          ),
          div(
            style="width:500px;",
            tableHTML::tableHTML(about[["baseline_anthropometry"]],
                                 rownames=F,border=0, spacing="3px", round=1, collapse="separate_shiny")
          )
          
      )
    )
  ),
  mt_header_dropdown_new(
    label= uiOutput("header_challenge_title"),
    div(style="max-width:1000px", 
        tags$b("Study design and challenges",style="margin-bottom:10px; font-size:14px;"),
        #h3("Study design and challenges",style="text-align:center"),
        ui_elements[["study_design"]],
        div(style="text-align:center",
            tags$span("Set the selected time range to a challenge by clicking on the challenge name or define an individual time range of interest by moving the slider")
        )
    )
  )
)

header[["children"]][[3]][["attribs"]]$style="background-color:#2c323a;"
header[["children"]][[2]][["attribs"]]$style="background:transparent; padding:0px;color:white;position:fixed;"
header[["children"]][[3]][["children"]][[2]][["attribs"]]$style="border-right:none"
header[["attribs"]]$class="main-header"
header[["children"]][[3]][["children"]][[4]][["children"]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style = "display:none"
