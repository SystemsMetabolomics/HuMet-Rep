# Body (dashboardBody)####
# 
body= shinydashboard::dashboardBody(
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css/rep_style_header.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/rep_ui.css"),
    # remove all below once integrated
    tags$link(rel = "stylesheet", type = "text/css", href = "css/mt_general.css"),  
    tags$link(rel = "stylesheet", type = "text/css", href = "css/mt_dropdown.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/mt_sidebar.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/mt_showcase.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/rep_tooltip.css"),  #rm if integrated
    tags$link(rel = "stylesheet", type = "text/css", href = "css/mt_design.css"),  
    tags$link(rel = "stylesheet", type = "text/css", href = "css/mt_loading.css"),  
    tags$link(rel = "stylesheet", type = "text/css", href = "css/mt_box.css"),      #rm if integrated
    tags$link(rel = "stylesheet", type = "text/css", href = "css/mt_style.css"),    #rm if integrated
    
    tags$link(rel = "stylesheet", type = "text/css", href = "css/h_style.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/h_subject_color.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/h_showcase.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/h_pathway_color.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/h_platform_color.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "css/h_about.css"),
    tags$script(src="js/h_functions.js")
  ),
  fluidRow(
    
    # Main bag selection ####
    div(style="padding:10px 5px 5px 5px;display:none;margin:0;flex:1;background:#505b6b;",
        `data-display-if` = 'input.menu=="metabdetail"  || input.menu=="network"|| input.menu=="metabselect" || input.menu=="stats_pca" || input.menu=="stats_ttest"',
        div(id="add_mainSelcol",style="width:100%;display:flex",
            div(style="flex:1",
                div(# bag1
                  `data-display-if` = 'input.main_sel_bagselector == 1',
                  selectizeInput(inputId = 'main_sel1',label = NULL,choices = info_met$labels, multiple = T,width="100%",size=5,
                                 options = list(
                                   plugins = list('remove_button', 'drag_drop'),
                                   valueField = "label",
                                   labelField = "label",
                                   placeholder = 'select metabolites',
                                   searchField = c("label", "synonym"),
                                   options = lapply(1:nrow(info_met), function(i) {
                                     list(label = info_met$labels[i], synonym = info_met$synonym[i])
                                   }))
                                 )
                  ),
                div(# bag2
                  `data-display-if` = 'input.main_sel_bagselector== 2',
                  selectizeInput(inputId = 'main_sel2',label = NULL,choices = info_met$labels, multiple = T,width="100%",size=5,
                                 options = list(
                                   plugins = list('remove_button', 'drag_drop'),
                                   valueField = "label",
                                   labelField = "label",
                                   placeholder = 'select metabolites',
                                   searchField = c("label", "synonym"),
                                   options = lapply(1:nrow(info_met), function(i) {
                                     list(label = info_met$labels[i], synonym = info_met$synonym[i])
                                   }))
                  )
                ),
                div(# bag3
                  `data-display-if` = 'input.main_sel_bagselector== 3',
                  selectizeInput(inputId = 'main_sel3',label = NULL,choices = info_met$labels, multiple = T,width="100%",size=5,
                                 options = list(
                                   plugins = list('remove_button', 'drag_drop'),
                                   valueField = "label",
                                   labelField = "label",
                                   placeholder = 'select metabolites',
                                   searchField = c("label", "synonym"),
                                   options = lapply(1:nrow(info_met), function(i) {
                                     list(label = info_met$labels[i], synonym = info_met$synonym[i])
                                   }))
                  )
                ),
                div(# bag4
                  `data-display-if` = 'input.main_sel_bagselector== 4',
                  selectizeInput(inputId = 'main_sel4',label = NULL,choices = info_met$labels, multiple = T,width="100%",size=5,
                                 options = list(
                                   plugins = list('remove_button', 'drag_drop'),
                                   valueField = "label",
                                   labelField = "label",
                                   placeholder = 'select metabolites',
                                   searchField = c("label", "synonym"),
                                   options = lapply(1:nrow(info_met), function(i) {
                                     list(label = info_met$labels[i], synonym = info_met$synonym[i])
                                   }))
                  )
                ),
                div(# bag5
                  `data-display-if` = 'input.main_sel_bagselector== 5',
                  selectizeInput(inputId = 'main_sel5',label = NULL,choices = info_met$labels, multiple = T,width="100%",size=5,
                                 options = list(
                                   plugins = list('remove_button', 'drag_drop'),
                                   valueField = "label",
                                   labelField = "label",
                                   placeholder = 'select metabolites',
                                   searchField = c("label", "synonym"),
                                   options = lapply(1:nrow(info_met), function(i) {
                                     list(label = info_met$labels[i], synonym = info_met$synonym[i])
                                   }))
                  )
                )
                
            ),
            
            div(actionButton(inputId="main_sel_divert", label=NULL, icon=rep_ui_icon("chart-area"), class="btn-main_sel_clear", style="color:#d2310b;"),`data-display-if` = 'input.menu=="metabselect"',style="padding-bottom:5px"), #add JS
            div(actionButton(inputId="main_sel_clear", label=NULL,icon=rep_ui_icon("remove"), class="btn-main_sel_clear"),style="padding-bottom:5px")
        ),
        div(style="margin-left:10px",
            tags$span("Strikethrough metabolites are not included currently filtered dataset.", style="font-size:10px;color:grey;"),
            tags$span(rep_tooltip(title=rep_ui_icon("question-circle"),
                                  tooltip="Metabolites can be filtered by platform, fluid and time point (challenges) in the header dropdowns (top right). If the here selected metabolites isn't within the filtered set they are highlighted by strikethrough.", style="font-size:12px;color:grey;")))
    )),
  fluidRow(
    tabItems(
      
      #Home (home)####
      tabItem("home",
              fluidRow(style='background-image:URL("img/logo/logo_humet_runner.png");background-repeat:no-repeat;background-position: right;background-size:500px;text-align:center;min-height:200px;margin-top: 70px',
                       column(12,
                              div(tags$span("The ",style='font-size:60px;font-weight:1000; color:black'),
                                  tags$span("HuMet",style='font-size:60px;font-weight:1000; color:#d2310b;margin-right:10px'),
                                  tags$span(" Repository",style='font-size:60px;font-weight:1000; color:black;'),
                                  style="text-align:center"),
                              div(tags$span("Time-resolved responses of the", style="color:black; font-size:24px;"),
                                  tags$span("hu", style="color:#d2310b; font-size:24px;"),
                                  tags$span("man ", style="color:black; font-size:24px;position:relative; left:-4px;"),
                                  tags$span("met", style="color:#d2310b; font-size:24px;"),
                                  tags$span("abolism", style="color:black; font-size:24px;position:relative; left:-4px;"),
                                  style="text-align:center"
                              )
                       ),
                       
                       column(12,
                              div(style="max-width:100%; width:800px; margin-left:auto; margin-right:auto; display:flex",
                                  div(class="home",
                                      selectizeInput(
                                        width="100%",
                                        inputId = "home_sel",
                                        label = NULL,
                                        choices = NULL,
                                        multiple=F,
                                        options = list(
                                          #plugins = list('remove_button', 'drag_drop'),
                                          valueField = "label",
                                          labelField = "label",
                                          placeholder = 'select metabolites',
                                          searchField = c("label", "synonym"),
                                          options = lapply(1:nrow(info_met), function(i) {
                                            list(label = info_met$labels[i], synonym = info_met$synonym[i])
                                          })
                                        )
                                      ),
                                      # pickerInput(inputId="home_sel", label=NULL, choices=info_met$labels, selected = NULL, multiple = FALSE,width="100%",
                                      #             options = list(title = 'search metabolite',style = "mt_search-bar",'live-search'=TRUE)),
                                      style="flex:1"),
                                  
                                  div(actionButton("home_divert", label=NULL,icon = rep_ui_icon("chart-area"),class="mt_search-submit")
                                      
                                  )
                              )
                       ),
                       column(12,
                              style="padding:15px",
                              rep_home_stats(id= "header_info_metabolites",title="2656",subtitle="Metabolites"),
                              rep_home_stats(id= "header_info_platforms",title="9",subtitle="Platforms"),
                              rep_home_stats(id= "header_info_timepoints",title="56",subtitle="Time points"),
                              rep_home_stats(id= "header_info_challenges",title="6",subtitle="Challenges"),
                              rep_home_stats(id= "header_info_subjects",title="15",subtitle="Healthy subjects")
                       )
              ),
              fluidRow(style="margin:0px 20px;",
                       column(12,
                              div(style="text-align:center;padding:10px;background:white;border:1px solid black; border-radius:5px;",
                                "The HuMet Repository enables exploration of dynamic metabolic responses to various physiological challenges in healthy individuals.", br(),
                                "Browse through the time-resolved metabolomics data from 15 young men at 56 timepoints over 4 days.",br(), 
                                "Query the database to find out more about how your favourite metabolites behave after food intake, exercise, and stress!",
                                a(href="https://www.biorxiv.org/content/10.1101/2023.08.08.550079v1",target="_blank", ">> Preprint out now <<", 
                                  style="color:#d2310b;display:block;text-align:center;font-size:20px;margin-top:10px;"))
                              )),
              fluidRow(style="margin:0px; 20px;",
                       column(12,
                              style="margin:20px 20px;text-align:center",
                              h1("Showcases", style="font-weight:700;"),
                              h4("Explore author selected biological showcases")
                       ),
                       column(12,
                              rep_showcase_button(id="home_showcase_washout",
                                                  title="Prior exposure",
                                                  subtitle="Putative biormarkers indicating consumption of specific food ingredients",
                                                  img_src="img/logo/logo_showcase_washout.png"),
                              rep_showcase_button(id="home_showcase_network_longi",
                                                  title="Systemic metabolic responses",
                                                  subtitle="Holistic overview of temporal responses to metabolic challenges",
                                                  img_src="img/logo/logo_showcase_network.png"),
                              rep_showcase_button(id="home_showcase_platforms",
                                                  title="Comparison of metabolic platforms",
                                                  subtitle="Targeted MS (Biocrates p150) versus non-targeted MS (Metabolon HD4)",
                                                  img_src="img/logo/logo_showcase_platform.png")
                       )
              ),
              fluidRow(style="margin:0px; 20px;",
                       column(12,
                              style="margin:20px 20px;text-align:center",
                              div(h1("Modules", style="font-weight:700;")),
                              div(
                                tags$span("Modular framework structure to visualize aspects of dynamic changes",style="font-size:20px")
                                )
                       ),
                       column(3,
                              tags$div(style="text-align:center",
                                       tags$img(src="img/home/icon_search.png", 
                                                style="margin-left:auto;margin-right:auto;border-radius:50%;height:50px;")
                                ),
                              tags$div(
                                       style="text-align:center;color:black;font-size:20px;",
                                       "Metabolite selection"
                                       ),
                              tags$div(
                                style="text-align:center;",
                                "Search for your metabolite of interest using the available tables. Filter the results by platform, biofluid, pathway membership, and implemented distance measures to find metabolites with similar kinetics.",
                                       )
                              ),
                       # shinydashboardPlus::userBox(width = 3,background = "gray", closable = F,collapsible=F,status="black",
                       #                                   src = "img/home/icon_search.png",
                       #                                   backgroundUrl = "img/home/background_search.png",
                       #                                   column(12,h3("Metabolite selection",style="text-align:center")),
                       #                                   footer = "Search for your metabolite of interest using the available tables. Filter the results by platform, biofluid, pathway membership, and implemented distance measures to find metabolites with similar kinetics."
                       # ),
                       column(3,
                              tags$div(style="text-align:center",
                                       tags$img(src="img/home/icon_browser.png", 
                                                style="margin-left:auto;margin-right:auto;border-radius:50%;height:50px;")
                              ),
                              tags$div(
                                style="text-align:center;color:black;font-size:20px;",
                                "Temporal plots"
                              ),
                              tags$div(
                                style="text-align:center;",
                                "Visualize your metabolic trajectories using mean or individual metabolite abundances in interactive plots. This visualization provides quick insight into metabolic kinetics over time."
                              )
                       ),
                       # shinydashboardPlus::userBox(width = 3,background = "gray", closable = F,collapsible=F,status="black",
                       #                                   src = "img/home/icon_browser.png",
                       #                                   backgroundUrl = "img/home/background_browser.png",
                       #                                   column(12,h3("Temporal plots",style="text-align:center")),
                       #                                   footer = "Visualize your metabolic trajectories using mean or individual metabolite abundances in interactive plots. This visualization provides quick insight into metabolic kinetics over time."
                       # ),
                       column(3,
                              tags$div(style="text-align:center",
                                       tags$img(src="img/home/icon_network.png", 
                                                style="margin-left:auto;margin-right:auto;border-radius:50%;height:50px;")
                              ),
                              tags$div(
                                style="text-align:center;color:black;font-size:20px;",
                                "Networks"
                              ),
                              tags$div(
                                style="text-align:center;",
                                "Obtain a comprehensive overview of all metabolites measured using targeted and non-targeted MS-based platforms in plasma and urine. Networks can be animated over time to highlight temporal metabolic changes."
                              )
                       ),
                       # shinydashboardPlus::userBox(width = 3,background = "gray", closable = F,collapsible=F, status="black",
                       #                                   src = "img/home/icon_network.png",
                       #                                   backgroundUrl = "img/home/background_network.png",
                       #                                   column(12,h3("Networks",style="text-align:center")),
                       #                                   footer = "Obtain a comprehensive overview of all metabolites measured using targeted and non-targeted MS-based platforms in plasma and urine. Networks can be animated over time to highlight temporal metabolic changes."
                       # ),
                       column(3,
                              tags$div(style="text-align:center",
                                       tags$img(src="img/home/icon_statistic.png", 
                                                style="margin-left:auto;margin-right:auto;border-radius:50%;height:50px;")
                              ),
                              tags$div(
                                style="text-align:center;color:black;font-size:20px;",
                                "Statistical analysis"
                              ),
                              tags$div(
                                style="text-align:center;",
                                "Perform statistical tests to identify metabolites with significant changes during a challenge of interest. All plots and generated data can be downloaded."
                              )
                       )
                       # shinydashboardPlus::userBox(width = 3,background = "gray", closable = F,collapsible=F,status="black",
                       #                                   src = "img/home/icon_statistic.png",
                       #                                   backgroundUrl = "img/home/background_statistic.png",
                       #                                   column(12,h3("Statistical analysis",style="text-align:center")),
                       #                                   footer = "Perform statistical tests to identify metabolites with significant changes during a challenge of interest or analyze the observed metabolomics variance using PCA. All plots and generated data can be downloaded."
                       # )
              ),
              fluidRow(style="margin:20px; 20px;",
                       column(12,
                              style="text-align:center",
                              div(h1("Futher reading", style="font-weight:700;")),
                              div(tags$span("Papers using the HuMet data",style="font-size:20px"))
                       ),
                       column(6,
                              rep_href(href="https://www.biorxiv.org/content/10.1101/2023.08.08.550079v1",label="The HuMet Repository: Watching human metabolism at work"),
                              tags$span(",Weinisch P., Raffler J., Römisch-Margl W., Arnold M., Mohney R., Rist MJ., Prehn C., Skurk T., Hauner H., Daniel H., Suhre K., Kastenmüller G.,"),
                              tags$b("biorxiv, 2023"),
                              style="padding:15px;"),
                       column(6,
                              rep_href(href="https://www.ncbi.nlm.nih.gov/pubmed/22426117",label="The dynamic range of the human metabolome revealed by challenges"),
                              tags$span(", Krug S., Kastenmüller G., Stückler F., Rist MJ., Skurk T., Sailer M., Raffler J., Römisch-Margl W., Adamski J., Prehn C., Frank T., Engel KH., Hofmann T., Luy B., Zimmermann R., Moritz F., Schmitt-Kopplin P., Krumsiek J., Kremer W., Huber F., Oeh U., Theis FJ., Szymczak W., Hauner H., Suhre K., Daniel H.,"),
                              tags$b("FASEB J. 2012 Jun;26(6):2607-19."),
                              style="padding:15px;"),
                       column(6,
                              rep_href(href="https://pubmed.ncbi.nlm.nih.gov/36211489/",label="Dynamic patterns of postprandial metabolic responses to three dietary challenges"),
                              tags$span(", Weinisch P., Fiamoncini J., Schranner D., Raffler J., Skurk T., Rist MJ., Römisch-Margl W., Prehn C., Adamski J., Hauner H., Daniel H., Suhre K., Kastenmüller G.,"),
                              tags$b("Frontiers in Nutrition, 2022"),
                              style="padding:15px;"),
                       column(6,
                              rep_href(href="https://pubmed.ncbi.nlm.nih.gov/35967802/",label="Dynamics and determinants of human plasma bile acid profiles during dietary challenges"),
                              tags$span(", Fiamoncini J., Rist MJ., Frommherz L., Giesbertz P., Pfrang B., Kremer W., Huber F., Kastenmüller G., Skurk T., Hauner H., Suhre K., Daniel H., Kulling SE.,"),
                              tags$b("Frontiers in Nutrition, 2022"),
                              style="padding:15px;")
              ),
              
              fluidRow(style="margin: 0 20px 20px 20px;",
                       column(12,
                              style="margin:20px 20px;text-align:center",
                              div(h1("The development team", style="font-weight:700;")),
                              div(tags$span("Scientists participating in the repository development", style="font-size:20px"))
                       ),
                       column(width=2,
                              tags$img(src = "img/about/developer_patrick.jpg", style="margin-left:auto; margin-right:auto;border-radius:50%;display:block;"),
                              #tags$span("PhD Student",br(),"Longitudinal metabolomics",style="color:black;display:block;text-align:center;"),
                              tags$b(a(href="https://www.helmholtz-munich.de/en/icb/research-groups/kastenmueller-lab",target="_blank", font="black","Patrick Weinisch", style="color:black;display:block;text-align:center;")),
                              div(style="font-size:20px; text-align:center",
                                  rep_social_button(url = "https://www.linkedin.com/in/patrick-weinisch-1b900a148/", type="linkedin"),
                                  rep_social_button(url="patrick@weinisch.com",  type="mail"))
                       ),
                       column(width=2,
                              tags$img(src = "img/about/developer_johannes.jpg", style="margin-left:auto; margin-right:auto;border-radius:50%;display:block;"),
                              #tags$span("Team Leader",br(),"Data Visualization and Integration Tools",style="color:black;display:block;text-align:center;"),
                              tags$b("Dr. Johannes Raffler", style="color:black;display:block;text-align:center;"),
                              div(style="font-size:20px; text-align:center",
                                  rep_social_button(url = "https://twitter.com/JohannesRaffler",type = "twitter")
                              )
                       ),
                       column(width=2,
                              tags$img(src = "img/about/developer_gabi.jpg", style="margin-left:auto; margin-right:auto;border-radius:50%;display:block;"),
                              #tags$span("Group Leader @ICB",br(),"Systems Metabolomics",style="color:black;display:block;text-align:center;"),
                              tags$b(a(href="https://www.helmholtz-munich.de/en/icb/pi/gabi-kastenmueller",target="_blank", font="black", "Dr. Gabi Kastenmüller", style="color:black;display:block;text-align:center;")),
                              div(style="font-size:20px; text-align:center",
                                  rep_social_button(url = "https://twitter.com/KastenmullerLab",type = "twitter"),
                                  rep_social_button(url = "https://www.linkedin.com/in/gabi-kastenm%C3%BCller-8b4b5576/", type="linkedin"),
                                  rep_social_button(url="g.kastenmueller@helmholtz-munich.de",  type="mail"))
                       ),
                       column(width=3,
                              tags$img(src = "img/about/developer_werner.jpg", style="margin-left:auto; margin-right:auto;border-radius:50%;display:block;max-height:133px;"),
                              tags$b(a(href="https://www.helmholtz-munich.de/en/icb/research-groups/kastenmueller-lab/staff/werner-roemisch-margl",target="_blank", font="black","Dr. Werner Römisch-Margl", style="color:black;display:block;text-align:center;")),
                              div(style="font-size:20px; text-align:center",
                                  rep_social_button(url="werner.roemischmargl@helmholtz-munich.de",  type="mail"))
                       ),
                       column(width=2,
                              div(
                                div(tags$b("Former developers:")), 
                                div("Maria Littmann")
                              )
                       )
                       
              )
              
      ),
      # Showcase (showcase) ------------------------------------------------------
      tabItem("showcase",
              fluidRow(style="margin:0",
                       h1("Showcase",style='font-size:60px;font-weight:1000;text-align:center'),
                       h4("The showcases provide an easy access to display groups of metabolites with interesting curve trajectories and results from statistical analysis. All showcases were selected by the authors.",style="text-align:center;margin:0 10%;"),
                       rep_ui_vline(),
                       div(style="text-align:center",
                           rep_showcase_button(id="showcase_item_washout",title="Prior exposures: identify metabolites with washout-like temporal profiles",
                                               subtitle="",#"Putative dietary biormarkers indicating consumption of specific food ingredients",
                                               img_src = "img/logo/logo_showcase_washout.png"),
                           rep_showcase_button(id="showcase_item_networks_longi", title="Systemic metabolic responses: Reveal & compare responses to challenges (example extended fasting)",
                                               subtitle="",#"Network overview of temporal responses to metabolic challenges",
                                               img_src="img/logo/logo_showcase_network.png"),
                           rep_showcase_button(id="showcase_item_networks_cross", title="Systemic metabolic responses: Reveal & compare responses to challenges (example: comparison of nutritional challenges)",
                                               subtitle="",#"Comparison of different challenges based on the inferred network",
                                               img_src="img/show/network/outcome_showcase_network2.png"),
                           rep_showcase_button(id="showcase_item_platforms", title="Platform comparison: Compare metabolites across platforms",
                                               subtitle="",#"Targeted MS (Biocrates p150) versus non-targeted MS (Metabolon HD4)",
                                               img_src="img/logo/logo_showcase_platform.png")
                       )
              )
              
      ),
      # Tutorial (tutorial) -----
      tabItem("tutorial",
              fluidRow(style="margin:0;background:#ddd;",
                        rep_showcase_header(title="Tutorial",
                                            question="How to navigate the HuMet Repository",
                                            img=NULL,
                                            buttons=column(12,style="padding-bottom:3px",
                                                           #tags$span("Jump to:", style="color:white"),
                                                           rep_jump_to(label="Video tutorial", id="box_tutorial_video"),
                                                           rep_jump_to(label="Sidebar", id="box_tutorial_general_sidebar"),
                                                           rep_jump_to(label="Header", id="box_tutorial_general_header"),
                                                           rep_jump_to(label="Selection", id="box_tutorial_selection"),
                                                           rep_jump_to(label="Time course", id="box_tutorial_time_course"),
                                                           rep_jump_to(label="Statistics", id="box_tutorial_statistics"),
                                                           rep_jump_to(label="Networks", id="box_tutorial_networks"),
                                                           rep_jump_to(label="Download", id="box_tutorial_download")
                                            )
                        )
              ),
              fluidRow( style="margin:0;background:#ddd;",
                         rep_showcase_panel(width=12, title="Tutorial", id="box_tutorial_video",
                                            column(12,
                                                   style="margin:0",
                                                   div(style="text-align:center",
                                                       tags$video(src = "video/humet_tutorial.mp4",  controls = "controls", width="700px")
                                                   )
                                            )
                         ),
                        rep_showcase_panel(width=12,title = "Left sidebar - getting started",id="box_tutorial_general_sidebar",
                                           fluidRow(
                                             column(4,
                                                    tags$b("Navigate tabs - left sidebar"),
                                                    div(style="text-align:center",
                                                      style="padding-left:5px;height:200px;",
                                                      img(src="img/tutorial/tutorial_home.png",style="max-height:100%;max-width:100%"),
                                                    )),
                                             column(6,
                                                    tags$b("Modules"),
                                                    div(
                                                      style="padding-left:5px;",
                                                      tags$ul(
                                                        tags$li(tags$b("Selection:")," Navigate and select metabolites for analysis. Streamline workflow by adding to a 'bag' for further module analysis."),
                                                        tags$li(tags$b("Time Course:")," Visualize metabolite level changes over time. Generate plots for interactive temporal exploration"),
                                                        tags$li(tags$b("Statistics:")," Apply statistical methods to analyze metabolite data. Uncover significant patterns and relationships."),
                                                        tags$li(tags$b("Networks:")," Explore metabolic pathways and interactions. Highlight selected metabolites within the network for insights."),
                                                        tags$li(tags$b("Showcase:")," Real-world examples demonstrate HuMet's modules in action. Case studies highlight biological insights and metabolic research applications."),
                                                        tags$li(tags$b("About:")," Background infromation on the underlying study, provided data and analysis workflow."),
                                                        tags$li(tags$b("Download:")," Bulk download feature for extensive metabolite data. Facilitates transparent data sharing."),
                                                      )))
                                             )
                                           ),
                        rep_showcase_panel(width=12,title = "Header - getting started",id="box_tutorial_general_header",
                                           fluidRow(
                                             column(6,   
                                                    tags$b("Bags - header center"),br(),
                                                    div(style="text-align:center",
                                                      style="padding-left:5px;",
                                                      img(src="img/tutorial/tutorial_bags.png",style="max-height:100%;max-width:100%"),
                                                      tags$p("Users can select and group various metabolites of interest, adding them to a centralized collection known as a 'bag'. 
                                                           This collection can then be utilized across different analytical modules for comprehensive visualization in singular plots like time courses or statistical analysis, 
                                                           as well as for targeted highlighting within metabolic networks.")
                                                    )),
                                             column(6,
                                                    tags$b("Selection of metabolites - header bottom"),br(),
                                                    div(style="text-align:center",
                                                      style="padding-left:5px;",
                                                      img(src="img/tutorial/tutorial_search.png",style="max-height:100%;max-width:100%")
                                                    ),
                                                    div(style="padding-left:5px;",
                                                        tags$p("Select any from a broad range of 2,656 metabolites, adding them to a specified 'bag' for detailed analysis or visualization.")
                                                    )
                                             )
                                           ),
                                           fluidRow(
                                             column(12,style="border-top:1px solid #ddd;margin:10px 0;"),
                                             column(6,
                                                    tags$b("Filtering and modification - right header"),br(),
                                                    div(style="padding-left:5px;",
                                                        img(src="img/tutorial/tutorial_headerfiltering.png",style="max-height:100%;max-width:100%")
                                                    )),
                                             column(6,
                                                    div(style="padding-left:5px;",
                                                        tags$b("The underlying data for plots and statistical analysis can be changed and filtered based on:"),
                                                        tags$ul(
                                                          tags$li(tags$b("Pre-processing:"), " Select data transformation and imputation methods during pre-processing for tailored plot and analysis results."),
                                                          tags$li(tags$b("Platform / Fluid:"), " Narrow down datasets by selecting specific fluids or platforms to ensure relevant data analysis."),
                                                          tags$li(tags$b("Subjects:"), " Customize your analysis by filtering the dataset based on subjects."),
                                                          tags$li(tags$b("Challenge:"), " Adapt the dataset by selection multiple time points or challenges to analyze specific physiological challenges.")
                                                        )
                                                    )
                                                    
                                             )
                                           )
                                          
                                           
                        ),
                        rep_showcase_panel(width=12,title = "Selection",id="box_tutorial_selection",
                                           column(3,
                                                  tags$b("All metabolites"),
                                                  div(
                                                    style="height:120px;padding-left:5px;",
                                                    img(src="img/tutorial/tutorial_selection_search.png",style="max-height:100%;max-width:100%")
                                                  ),
                                                  div(
                                                    style="padding-left:5px;",
                                                    tags$p("Choose your metabolites from a comprehensive list that encompasses all available metabolites for detailed analysis."),
                                                    div(
                                                      style="text-align:center",
                                                      actionButton(inputId ="tutorial_divert_selection_all",label="View all metabolites")
                                                    )
                                                    )
                                                  ),
                                           column(3,
                                                  tags$b("Similarity search"),
                                                  div(
                                                    style="height:120px;padding-left:5px;",
                                                    img(src="img/tutorial/tutorial_selection_similarity.png",style="max-height:100%;max-width:100%")),
                                                  div(style="padding-left:5px;",
                                                    tags$p("Identify metabolites that exhibit a trajectory similar to that of a reference metabolite for comparison."),
                                                    div(
                                                      style="text-align:center",
                                                      actionButton(inputId ="tutorial_divert_selection_sim",label="View similarity search")
                                                    )
                                                  )
                                           ),
                                           column(3,
                                                  tags$b("Kegg pathways"),
                                                  div(
                                                    style="height:120px;padding-left:5px;",
                                                    img(src="img/tutorial/tutorial_selection_kegg.png",style="max-height:100%;max-width:100%")),
                                                  div(style="padding-left:5px;",
                                                    tags$p("Pick metabolites specifically mapped to a selected KEGG pathway to focus on known biological pathways."),
                                                    div(
                                                      style="text-align:center",
                                                      actionButton(inputId ="tutorial_divert_selection_kegg",label="View kegg pathways")
                                                    )
                                                  )
                                           ),
                                           column(3,
                                                  tags$b("Annotated pathways"),
                                                  div(
                                                    style="height:120px;padding-left:5px;",
                                                    img(src="img/tutorial/tutorial_selection_vendor.png",style="max-height:100%;max-width:100%")
                                                  ),
                                                  div(
                                                    style="padding-left:5px;",
                                                    tags$p("Select metabolites based on their mapping from various vendors or proprietary in-house platforms for analysis."),
                                                    div(
                                                      style="text-align:center",
                                                      actionButton(inputId ="tutorial_divert_selection_anno",label="View annotated pathways")
                                                    )
                                                  )
                                           )
                        ),
                        rep_showcase_panel(width=12,title = "Time course",id="box_tutorial_time_course",
                                           fluidRow(
                                             column(3, 
                                                    tags$b("Aggregated plots (means of metabolites)"),br(),
                                                    img(src="img/tutorial/tutorial_timecourse_aggregated.png",style="max-height:120px;"),
                                                    div(
                                                      style="padding-left:5px;",
                                                      "Generate a single plot for each 'bag' that displays the average values for each subject at every time point."),
                                                    div(style="text-align:center",actionButton(inputId="tutorial_divert_timecourse_aggregated", label="View aggregated plots"))
                                             ),
                                             column(3, 
                                                    tags$b("Individual plots"), br(),
                                                    
                                                    div(
                                                      style="padding-left:5px;",
                                                      img(src="img/tutorial/tutorial_timecourse_individual.png",style="max-height:120px;"),br(),
                                                      "Show a plot for each metabolite, displaying the temporal trajectories of all selected participants."
                                                    ),
                                                    div(style="text-align:center",actionButton(inputId="tutorial_divert_timecourse_individual", label="View individual plots"))
                                             ),
                                             column(3, 
                                                    tags$b("Average + sd"),
                                                    div(
                                                      style="padding-left:5px;",
                                                      img(src="img/tutorial/tutorial_timecourse_sd.png",style="max-height:120px;"),br(),
                                                      "Show a unique plot for each metabolite, displaying the average across all subjects for each time point, with standard deviation represented as an error bar."
                                                    ),
                                                    div(style="text-align:center",actionButton(inputId="tutorial_divert_timecourse_sd", label="View average + sd"))),
                                             column(3, tags$b("Average + min/max"),br(),
                                                    div(
                                                      img(src="img/tutorial/tutorial_timecourse_minmax.png",style="max-height:120px;"),br(),
                                                      style="padding-left:5px;",
                                                      "Show a plot for every metabolite, indicating the average across all subjects at each time point, and depict the range using minimum and maximum values."
                                                    ),
                                                    div(style="text-align:center",actionButton(inputId="tutorial_divert_timecourse_minmax", label="View average + min/max plots"))
                                             )
                                           )
                        ),
                        
                        rep_showcase_panel(width=12,title = "Statistics",id="box_tutorial_statistics",
                                           column(6,
                                                  tags$b("Volcano plots"),
                                                  div(style="padding-left:5px",
                                                      img(src="img/tutorial/tutorial_statistics_volcano.png",style="max-height:120px;"),br(),
                                                      tags$p("Explore statistical findings through interactive volcano plot visualization.")
                                                  ),
                                                  div(
                                                    style="text-align:center",
                                                    actionButton(inputId="tutorial_divert_statistics_volcano", label="View volcano plots"))
                                                  
                                           ),
                                           column(6,
                                                  tags$b("Tables with statistical results"),
                                                  div(style="padding-left:5px",
                                                      img(src="img/tutorial/tutorial_statistics_table.png",style="max-height:120px;"),br(),
                                                      tags$p("Explore statistical results in debth by using tables visualizing the results.")
                                                  ),
                                                  div(
                                                    style="text-align:center",
                                                    actionButton(inputId="tutorial_divert_statistics_table", label="View statistical results"))
                                           )         
                                           
                        ),
                        rep_showcase_panel(width=12,title = "Networks",id="box_tutorial_networks",
                                           fluidRow(
                                             column(12,tags$b("Networks"),br(),
                                                    tags$p("Holistic network overview of metabolic changes altered in response to phyisological challenges")),
                                             column(4, tags$b("Single fluid networks"),
                                                  div(
                                                    style="padding-left:5px;",
                                                    img(src="img/tutorial/tutorial_networks_sggm.png",style="max-height:200px;max-width:100%"),br(),
                                                    "Data driven networks based on one platform/fluid"),
                                                  div(style="text-align:center",actionButton(inputId="tutorial_divert_networks_sggm", label="View single fluid networks")
                                                  )
                                                  ),
                                           column(4, tags$b("Multi fluid networks"),br(),
                                                  div(
                                                    style="padding-left:5px;",
                                                    
                                                    img(src="img/tutorial/tutorial_networks_mggm.png",style="max-height:200px;max-width:100%"),br(),
                                                    "Data driven networks based on multiple platforms/fluids"),
                                                  div(style="text-align:center",actionButton(inputId="tutorial_divert_networks_mggm", label="View multi fluid networks")
                                                  )
                                           ),
                                           column(4, tags$b("Annotated networks"),br(),
                                                  div(
                                                    style="padding-left:5px;",
                                                    img(src="img/tutorial/tutorial_networks_vendor.png",style="max-height:200px;max-width:100%"),br(),
                                                    "Networks based on super-pathway annotation by vendors"),
                                                  div(style="text-align:center",actionButton(inputId="tutorial_divert_networks_vendor", label="View annotated networks")
                                                  )
                                           )
                                           ),
                                           fluidRow(
                                             column(12, style="border-top:1px solid #ddd;margin:10px 0"),
                                             column(4, 
                                               tags$b("Color by super-pathway"),br(),
                                                    div(
                                                      style="padding-left:5px;",
                                                      img(src="img/tutorial/tutorial_networks_superpathway.png",style="max-height:200px;max-width:100%"),
                                                      "Networks based on super-pathway coloring provided by vendors."),
                                                    div(style="text-align:center",actionButton(inputId="tutorial_divert_networks_superpathway", label="View colored networks")
                                                    )
                                                    ),
                                             column(4, 
                                                    tags$b("Color by platform"),br(),
                                                    div(
                                                      style="padding-left:5px;",
                                                      img(src="img/tutorial/tutorial_networks_platform.png",style="max-height:200px;max-width:100%"),
                                                      "Networks colored by platforms."),
                                                    div(style="text-align:center",actionButton(inputId="tutorial_divert_networks_platform", label="View colored networks")
                                                    )
                                             ),
                                             column(4, 
                                                    tags$b("Coloring by change"),br(),
                                                    div(
                                                      style="padding-left:5px;",
                                                      img(src="img/tutorial/tutorial_networks_temporal.png",style="max-height:200px;max-width:100%"),
                                                      "Networks colored by fold change between challenge beginn and specified time point. Node size depicts significance of changes (-log10(pval))."),
                                                    div(style="text-align:center",actionButton(inputId="tutorial_divert_networks_time", label="View colored networks")
                                                    )
                                             )
                                           )

                                           
                        ),
                        rep_showcase_panel(width=12,title = "Download",id="box_tutorial_download",
                                           column(4,
                                                  tags$b("Bulk download of data within the HuMet respository"),br(),
                                                  div(
                                                    style="padding-left:5px",
                                                    icon("download")
                                                  ),
                                                  div(
                                                    style="text-align:center",
                                                    actionButton(inputId="tutorial_divert_download", label=tags$span("View download options")
                                                  )
                                           ))
                        )
              )
      ),
                       
      # Selection (metabselect)####
      tabItem("metabselect",
              #fluidRow(style="height:90px"),
              fluidRow(style="padding:15px 30px;",class="mt_design",
                       shinydashboard::tabBox(id = "metabselect_tabbox",width=12,
                                              shiny::tabPanel(title="All metabolites",
                                                              fluidRow(style="margin:0",
                                                                       column(12, class="mt_tabbox-submenu",
                                                                              div(style="margin-left:10px;border-radius:5px; background:transparent; width:80%; float:right;",
                                                                                  #tags$b("Info", style="display:block;margin-bottom:5px; margin-left:10px;"),
                                                                                  tags$span("List of all identified metabolites.", style="margin-left:10px;")
                                                                              )
                                                                       ),
                                                                       column(12,class="mt_tabbox-content",style="overflow:scroll;",
                                                                              withSpinner(DT::dataTableOutput("search_table_all"), type=5,color="#d2310b"))
                                                              )
                                              ),
                                              shiny::tabPanel(title="Similarity",
                                                              fluidRow(style="margin:0",
                                                                       column(12, class="mt_tabbox-submenu",
                                                                              div(style="display:inline-block",
                                                                                  div(style="display:inline-block;margin:0 5px;",
                                                                                      pickerInput(inputId="search_sim_table_refMet", label=tags$span("Reference metabolite",rep_tooltip(title=rep_ui_icon("question-circle"),tooltip="Filtered metabolites are ranked against the reference metabolite according to the user selected distance.", style="")),selected=NULL,choices=info_met$labels, width='150px',options = list(title = 'Select a reference metabolite','live-search'=TRUE))
                                                                                  ),
                                                                                  div(style="display:inline-block;margin:0 5px;",
                                                                                      pickerInput(inputId="search_sim_table_dist", label=tags$span("Distance measure",rep_tooltip(title=rep_ui_icon("question-circle"),
                                                                                                                                                                                  tooltip="The chosen distance measure is used to calculate the similairty/dissimilarity between the reference metabolite and measured metabolite subset.", style="")), width="150px",
                                                                                                  choices = list("Frechet"="frechet","Frechet (on average trajectories)"="frechet_mean",
                                                                                                                 "Pearson correlation (on average trajectories)"="pearson_mean",
                                                                                                                 "Pearson correlation"="pearson_single",
                                                                                                                 "Euclidean"="euclidean","Manhattan"="manhattan"),
                                                                                                  selected="frechet_mean")
                                                                                  ),
                                                                                  div(style="display:inline-block;margin:0 5px;",
                                                                                      tags$b("Download", style="display:block;margin-bottom:5px;"),
                                                                                      dropdownButton(circle=F,size="default",label=NULL,icon=rep_ui_icon("download"),width="100%",right=F,
                                                                                                     tags$b("Download distance ranking table as:"),
                                                                                                     downloadButton(outputId="selection_simTable_csv", label=".csv", class="mt_download"),
                                                                                                     downloadButton(outputId="selection_simTable_excel", label=".xlsx*", class="mt_download"),
                                                                                                     br(),tags$span("*xlsx download only available for Chrome"))
                                                                                  ),
                                                                                  div(style="display:inline-block;margin:0 5px;",
                                                                                      tags$b("Add first 10", style="display:block;margin-bottom:5px;margin-left:10px;"),
                                                                                      actionButton(inputId="search_similarity_add", label=rep_ui_icon("plus-circle"), style="height:33px; width:100%")
                                                                                  ),
                                                                                  div(style="display:inline-block;margin:0 5px;width:110px;",
                                                                                      `data-display-if` = 'input.search_sim_table_dist=="frechet"  || input.search_sim_table_dist=="euclidean"|| input.search_sim_table_dist=="manhattan" || input.search_sim_table_dist=="pearson_single"',
                                                                                      tags$b("Min overlap", style="display:block;margin-bottom:5px;margin-left:10px;",
                                                                                             tags$span(rep_tooltip(title=rep_ui_icon("question-circle"),
                                                                                                                  tooltip="Similarity is only calculated if a metabolite pair has a minimum of set overlap per person. If overlap is lower, the metabolite pair is excluded.", style="font-size:12px;color:grey;"))),
                                                                                      numericInput(inputId = "search_similarity_minoverlap",label=NULL, value=4, min=2,max=40, step=1)
                                                                                  )
                                                                              ),
                                                                              div(style="flex:1;margin-left:10px;border-radius:5px; background:transparent;padding:5px;",
                                                                                  div(
                                                                                      div(`data-display-if` = 'input.search_sim_table_dist=="frechet_mean"',
                                                                                          tags$span("Distance info: The Fréchet distance is calculated on the averaged metabolite curves (averaged across subjects per time points) with the distFrechet function implemented within the", rep_href(href="https://cran.r-project.org/src/contrib/Archive/kmlShape/",label="kmlShape"),"R package")),
                                                                                      div(`data-display-if` = 'input.search_sim_table_dist=="frechet"',
                                                                                          tags$span("Distance info: The Fréchet distance is calculated on the z-scored dataset with the distFrechet function implemented within the", rep_href(href="https://cran.r-project.org/src/contrib/Archive/kmlShape/",label="kmlShape"),"R package based on the trajectories of each individual, with distances being averaged across all chosen participants. Only metabolites meeting the ‘min overlap criteria’ are displayed.")),
                                                                                      div(`data-display-if` = 'input.search_sim_table_dist=="pearson_mean"',
                                                                                          tags$span("Distance info: the", rep_href(href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html",label="stats")," R package was used to calculate the paired Pearson correlation on the trajectories averaged across all selected participants.")),
                                                                                      div(`data-display-if` = 'input.search_sim_table_dist=="pearson_single"',
                                                                                          tags$span("Distance info: the", rep_href(href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html",label="stats")," R package was used to calculate the paired Pearson correlation on the trajectories of each individual, with distances being averaged across all chosen participants. Only metabolites meeting the ‘min overlap criteria’ are displayed.")),
                                                                                      div(`data-display-if` = 'input.search_sim_table_dist=="euclidean"',
                                                                                          tags$span("Distance info: the z-scored dataset was used to calculate the Euclidean distance between two metabolite curves within one subject and subsequently aggregating the similarity measure across all chosen participants. We used the dist function implemented within the",rep_href(href="https://cran.r-project.org/web/packages/proxy/index.html",label="proxy")," R package to calculate the Euclidean distance. Only metabolites meeting the ‘min overlap criteria’ are displayed.")),
                                                                                      div(`data-display-if` = 'input.search_sim_table_dist=="manhattan"',
                                                                                          tags$span("Distance info: the z-scored dataset was used to calculate the Manhattan distance between two metabolite curves within one subject and subsequently aggregating the similarity measure across all chosen participants. We used the dist function implemented within the",rep_href(href="https://cran.r-project.org/web/packages/proxy/index.html",label="proxy")," R package to calculate the Manhattan distance. Only metabolites meeting the ‘min overlap criteria’ are displayed."))
                                                                                      ),
                                                                                  uiOutput(outputId="search_sim_table_info")
                                                                              )
                                                                       ),
                                                                       column(12, class="mt_tabbox-content",
                                                                              conditionalPanel('input.search_sim_table_refMet== ""',
                                                                                               h4("Select a reference metabolite and optionally choose a distance measure", style="color:#d2310b; height:100px;")),
                                                                              conditionalPanel('input.search_sim_table_refMet!= ""',
                                                                                               style="padding:5px;overflow:scroll;",
                                                                                               withSpinner(DT::dataTableOutput("search_table_sim"), type=5,color="#d2310b")
                                                                              )
                                                                       )
                                                              )
                                              ),
                                              shiny::tabPanel(title="KEGG pathways",
                                                              fluidRow(
                                                                style="margin:0; background-color:#ddd",
                                                                tags$p("Kegg tables are only available for the online version at", rep_href(href="https://www.humet.org",label="https://www.humet.org"),
                                                                       style="padding:20px 10px")
                                                              )
                                                              
                                              ),
                                              shiny::tabPanel(title="Annotated pathways",
                                                              fluidRow(style="margin:0",
                                                                       column(12, class="mt_tabbox-submenu",
                                                                              div(
                                                                                div(style="display:inline-block",
                                                                                    pickerInput("search_table_sub_refPath", 
                                                                                                label=tags$span("Annotated sub-pathway",rep_tooltip(title=rep_ui_icon("question-circle"),tooltip="The metabolite sub-pathway structure is defined by the kit vendor", style="")), 
                                                                                                width="200px",
                                                                                                choices= info_met$SUB.PATHWAY %>% unique() %>% hu_subpathway_labels(),
                                                                                                selected = "Carnitine Metabolism",
                                                                                                options = list(noneSelectedText = 'choose Annotated sub-pathway',`live-search`=T,size = 5,`actions-box` = TRUE,styleBase="form-control"))
                                                                                ),
                                                                                div(
                                                                                  style="display:inline-block",
                                                                                  tags$b("Add all", style="display:block;margin-bottom:5px;"),
                                                                                  actionButton(inputId="search_table_sub_add", label=rep_ui_icon("plus-circle"), style="height:33px; width:100%")
                                                                                )
                                                                              ),
                                                                              div(style="flex:1;margin-left:10px;border-radius:5px; background:transparent;padding:5px;",
                                                                                  "Info: to provide an overview of measured metabolites, we implemented a super-pathway and sub-pathway structure. 
                                                                                  Metabolites identified from our in-house platforms were manually assigned to this structure and matched to knowledge-based platforms such as KEGG, PubChem, and HMDB.
                                                                                  For identified metabolites from vendor-based kits, we used annotation provided by the Metabolon HD4 and Biocrates p150 platforms to classify them into our super- and sub-pathway structure."
                                                                                  
                                                                              ) 
                                                                       ),
                                                                       column(12,class="mt_tabbox-content",
                                                                              
                                                                              style="overflow:scroll;",
                                                                              withSpinner(DT::dataTableOutput("search_table_sub"), type=5,color="#d2310b")
                                                                       )
                                                                       
                                                              )
                                              )
                       )
              )
              
      ),
# Browser (Metabolite Browser = metabdetail) ####
      tabItem(tabName = "metabdetail",
              fluidRow(style="background:#ddd;margin:0;",
                       column(12,style="padding:10px 15px;;color:black;border-top:1px solid #efefefef",
                              div(style="display:inline-block;margin:0 5px;",
                                  tags$b("Display metabolites", style="display:block;margin-bottom:5px;"),
                                              pickerInput("browser_plot_display", label=NULL,options=list(style = "mt_submenu_picker"), width='100%',
                                                          choices=list("Aggregated: means of metabolites"="aggregated",
                                                                       "One per metabolite: average + sd"="errorBar",
                                                                       "One per metabolite: average + min/max"="minMax",
                                                                       "One per metabolite: individual study participants"="individual"))
                              ),
                              div(style="display:inline-block;margin:0 5px;",
                                  tags$b("Spline interpolation", style="display:block;margin-bottom:5px;"),
                                  switchInput("browser_line",label=NULL,value=F,width = "100px", onLabel="on", offLabel = "off", onStatus = "warning",offStatus = "secondary")
                                  ),
                              div(style="display:inline-block;margin:0 5px;",
                                  tags$b("Annotations", style="display:block;margin-bottom:5px;"),
                                  switchInput(inputId = "browser_annotation", label = NULL,width="100px", value = F,onLabel="on", offLabel = "off", offStatus = "secondary",onStatus = "warning")
                                  ),
                              
                              div(style="display:inline-block;margin:0 5px;",
                                  tags$b("Download", style="display:block;margin-bottom:5px;"),
                                  dropdownButton(circle=F,size="default",label=NULL,icon=rep_ui_icon("download"),width="100%",right=F,
                                                 tags$b("Download selected metabolite data:"),
                                                 shiny::downloadButton(outputId="download_browser_data_csv", label="csv", class="mt_download"),
                                                 shiny::downloadButton(outputId="download_browser_data_xlsx", label="xlsx*", class="mt_download"),
                                                 tags$span("*xlsx only available for Chrome"))
                              )
                       )
              ),
              fluidRow(style="padding:15px;margin:0;border-top:1px solid #a9a9a9;", 
                       class="mt_design",
                       mt_box(id="bag1",width=12,collapsible = T, closable=F,refresh=F,title_color = "#efefef",
                              title=uiOutput("browser_bag1_header_title", style="height:100%"),
                              conditionalPanel(condition='input.main_sel1==""',tags$b("please select a metabolite", class=".mtd_text_red"   )),
                              conditionalPanel(condition='input.browser_plot_display=="aggregated"', uiOutput(paste0("browser_bag1_plot_grp"))),
                              conditionalPanel(condition='input.browser_plot_display=="errorBar"||input.browser_plot_display=="minMax"||input.browser_plot_display=="individual"',uiOutput(paste0("browser_bag1_plot_single")))
                       ),
                       mt_box(id="bag2",width=12,collapsible = T, closable=F,refresh=F,title_color = "#efefef",
                              title=uiOutput("browser_bag2_header_title", style="height:100%"),
                              conditionalPanel(condition='input.main_sel2==""',tags$b("please select a metabolite", class=".mtd_text_red")),
                              conditionalPanel(condition='input.browser_plot_display=="aggregated"', uiOutput(paste0("browser_bag2_plot_grp"))),
                              conditionalPanel(condition='input.browser_plot_display=="errorBar"||input.browser_plot_display=="minMax"||input.browser_plot_display=="individual"',uiOutput(paste0("browser_bag2_plot_single")))
                       ),
                       mt_box(id="bag3",width=12,collapsible = T, closable=F,refresh=F,title_color = "#efefef",display_if = 'input.main_sel_bag_add > "0"',
                              title=uiOutput("browser_bag3_header_title", style="height:100%"),
                              conditionalPanel(condition='input.main_sel3==""',tags$b("please select a metabolite", class=".mtd_text_red")),
                              conditionalPanel(condition='input.browser_plot_display=="aggregated"', uiOutput(paste0("browser_bag3_plot_grp"))),
                              conditionalPanel(condition='input.browser_plot_display=="errorBar"||input.browser_plot_display=="minMax"||input.browser_plot_display=="individual"',uiOutput(paste0("browser_bag3_plot_single")))
                       ),
                       mt_box(id="bag4",width=12,collapsible = T, closable=F,refresh=F,title_color = "#efefef",
                              title=uiOutput("browser_bag 4_header_title", style="height:100%"),display_if = 'input.main_sel_bag_add > "1"',
                              conditionalPanel(condition='input.main_sel4==""',tags$b("please select a metabolite", class=".mtd_text_red")),
                              conditionalPanel(condition='input.browser_plot_display=="aggregated"', uiOutput(paste0("browser_bag4_plot_grp"))),
                              conditionalPanel(condition='input.browser_plot_display=="errorBar"||input.browser_plot_display=="minMax"||input.browser_plot_display=="individual"',uiOutput(paste0("browser_bag4_plot_single")))
                       ),
                       mt_box(id="bag5",width=12,collapsible = T, closable=F,refresh=F,title_color = "#efefef",
                              title=uiOutput("browser_bag5_header_title", style="height:100%"),display_if = 'input.main_sel_bag_add > "2"',
                              conditionalPanel(condition='input.main_sel5==""',tags$b("please select a metabolite", class=".mtd_text_red")),
                              conditionalPanel(condition='input.browser_plot_display=="aggregated"', uiOutput(paste0("browser_bag5_plot_grp"))),
                              conditionalPanel(condition='input.browser_plot_display=="errorBar"||input.browser_plot_display=="minMax"||input.browser_plot_display=="individual"',uiOutput(paste0("browser_bag5_plot_single")))
                       )
              )
      ),
      #Network (Metabolite networks = network) ####
      tabItem(tabName = "network",
              fluidRow(class="mt_design",
                       uiOutput("net_css"),
                       shinydashboard::tabBox(id="network_tabbox", width=12,
                                              shiny::tabPanel(title="Single fluid networks",value="sggm",
                                                              fluidRow(class="mt_tabbox-submenu",
                                                                       column(6,
                                                                              mt_submenu_item(title="Fluid",
                                                                                              width = "auto",
                                                                                              
                                                                                              pickerInput(inputId = "network_sggm_fluid",width="200px",label = NULL,
                                                                                                          selected = options$network$single_ggm$fluid[22],
                                                                                                          choices = unique(options$network$single_ggm$fluid)),
                                                                                              footer=NULL
                                                                              ),
                                                                              mt_submenu_item(title="Platform",
                                                                                              width = "auto",
                                                                                              pickerInput(inputId = "network_sggm_platform",width="200px",
                                                                                                          label = NULL,
                                                                                                          selected = options$network$single_ggm$platform[22],
                                                                                                          choices = unique(options$network$single_ggm$platform)),
                                                                                              footer=NULL
                                                                              ),
                                                                              mt_submenu_item(title="Cutoff",
                                                                                              width = "auto",
                                                                                              pickerInput(inputId = "network_sggm_cutoff",width="200px",
                                                                                                          label = NULL,
                                                                                                          selected = options$network$single_ggm$cutoff[22],
                                                                                                          choicesOpt = list(subtext =options$network$single_ggm$cutoff_subtext),
                                                                                                          choices = unique(options$network$single_ggm$cutoff)),
                                                                                              footer=NULL
                                                                              ),
                                                                              mt_submenu_item(title="Download",
                                                                                              width = "auto",
                                                                                              dropdownButton(circle=F,size="default",label="",icon=rep_ui_icon("download"),width="300px",right=F,
                                                                                                             tags$b("Download depicted network:"),
                                                                                                             downloadButton(outputId="network_sgmm_download_html", label="standalone .html", class="mt_download"),
                                                                                                             br(),tags$b("Download network edges as table:"),
                                                                                                             downloadButton(outputId="network_sggm_download_csv", label=".csv", class="mt_download"),
                                                                                                             downloadButton(outputId="network_sggm_download_excel", label=".xlsx*", class="mt_download"),
                                                                                                             br(),tags$span("*xlsx download only available for Chrome")),
                                                                                              footer=NULL
                                                                              )
                                                                       ),
                                                                       column(6,
                                                                              tags$p("Single-fluid gaussian graphical models (GGMs) are based on partial correlations. Network inference closely follows procedures established by ", 
                                                                                     rep_href(href="https://www.nature.com/articles/s41540-017-0029-9",label="Do et al. 2017"),
                                                                                     " but applying the dynamic partial correlation approach described by ",
                                                                                     rep_href(href="https://www.ine.pt/revstat/pdf/rs060103.pdf",label="Opgenrhein & Strimmer 2006"),
                                                                                     " which takes the factor ‘time’ into account. The shrinkage estimator approach ‘GeneNet’ was used as the dataset includes fewer samples (840) than variables (2656). 
                                           Network inference was based on the log2 transformed metabolomics data to better meet the assumption of a multivariate normal distribution ",
                                                                                     "(",rep_href(href="https://pubmed.ncbi.nlm.nih.gov/31639475/",label="Altenbuchinger et al., 2020"),").",
                                                                                     "Furthermore, we assume that relationships between variables are linear and introduce sparsity (implemented within the", rep_href(href="https://cran.rstudio.com/web/packages/GeneNet/index.html",label="GeneNet") ,"approach). ")
                                                                              )
                                                                       
                                                              )
                                              ),
                                              shiny::tabPanel("Multi fluid networks", value="mggm",
                                                              fluidRow(class="mt_tabbox-submenu",
                                                                       column(8,
                                                                              mt_submenu_item(title="Platform plasma",
                                                                                              width = "auto",
                                                                                              pickerInput(inputId = "network_multi_platform_plasma",width="200px",
                                                                                                          label = NULL,
                                                                                                          selected = options$network$multi_ggm$platform_plasma[1],
                                                                                                          choices = unique(options$network$multi_ggm$platform_plasma))
                                                                              ),
                                                                              mt_submenu_item(title="Cutoff plasma",
                                                                                              width = "auto",
                                                                                              pickerInput(inputId = "network_mggm_cutoff_plasma",width="200px",
                                                                                                          label = NULL,
                                                                                                          selected = options$network$multi_ggm$cutoff_plasma[1],
                                                                                                          choices = unique(options$network$multi_ggm$cutoff_plasma))
                                                                              ),
                                                                              mt_submenu_item(title="Platform urine",
                                                                                              width = "auto",
                                                                                              pickerInput(inputId = "network_mggm_platform_urine",width="200px",
                                                                                                          label = NULL,
                                                                                                          selected = options$network$multi_ggm$platform_urine[1],
                                                                                                          choices = unique(options$network$multi_ggm$platform_urine))
                                                                              ),
                                                                              mt_submenu_item(title="Cutoff urine",
                                                                                              width = "auto",
                                                                                              pickerInput(inputId = "network_mggm_cutoff_urine",width="200px",
                                                                                                          label = NULL,
                                                                                                          selected = options$network$multi_ggm$cutoff_urine[1],
                                                                                                          choices = unique(options$network$multi_ggm$cutoff_urine))
                                                                              ),
                                                                              mt_submenu_item(title="Download",
                                                                                              width = "auto",
                                                                                              dropdownButton(circle=F,size="default",label="",icon=rep_ui_icon("download"),width="300px",right=F,
                                                                                                             tags$b("Download depicted network:"),
                                                                                                             downloadButton(outputId="network_mgmm_download_html", label="standalone .html", class="mt_download"),
                                                                                                             br(),tags$b("Download network edges as table:"),
                                                                                                             downloadButton(outputId="network_mggm_download_csv", label=".csv", class="mt_download"),
                                                                                                             downloadButton(outputId="network_mggm_download_excel", label=".xlsx*", class="mt_download"),
                                                                                                             br(),tags$span("*xlsx download only available for Chrome")),
                                                                                              footer=NULL
                                                                              )
                                                                       ),
                                                                       column(4,
                                                                              tags$p("Single-fluid / Multi-fluid gaussian graphical models (GGMs) are based on partial correlations. Network inference closely follows procedures established by ", 
                                                                                     rep_href(href="https://www.nature.com/articles/s41540-017-0029-9",label="Do et al. 2017"),
                                                                                     " but applying the dynamic partial correlation approach described by ",
                                                                                     rep_href(href="https://www.ine.pt/revstat/pdf/rs060103.pdf",label="Opgenrhein & Strimmer 2006"),
                                                                                     " which takes the factor ‘time’ into account. The shrinkage estimator approach ‘GeneNet’ was used as the dataset includes fewer samples (840) than variables (2656). 
                                           Network inference was based on the log2 transformed metabolomics data to better meet the assumption of a multivariate normal distribution ",
                                                                                     "(",rep_href(href="https://pubmed.ncbi.nlm.nih.gov/31639475/",label="Altenbuchinger et al., 2020"),").",
                                                                                     "Furthermore, we assume that relationships between variables are linear and introduce sparsity (implemented within the", rep_href(href="https://cran.rstudio.com/web/packages/GeneNet/index.html",label="GeneNet") ,"approach). ")
                                                                       )
                                                                              
                                                              )
                                              ),
                                              shiny::tabPanel("Annotated pathways",value="db",
                                                              fluidRow(class="mt_tabbox-submenu",
                                                                       column(7,
                                                                              mt_submenu_item(title="Fluid",
                                                                                              width = "auto",
                                                                                              pickerInput(inputId = "network_db_fluid",width="200px",
                                                                                                          label = NULL,
                                                                                                          selected = options$network$databases$fluid[1],
                                                                                                          choices = unique(options$network$databases$fluid))
                                                                              ),
                                                                              mt_submenu_item(title="Vendor",
                                                                                              width = "auto",
                                                                                              pickerInput(inputId = "network_db_platform",width="200px",
                                                                                                          label = NULL,
                                                                                                          selected = options$network$databases$platform[1],
                                                                                                          choices = unique(options$network$databases$platform))
                                                                              ),
                                                                              mt_submenu_item(title="Download",
                                                                                              width = "auto",
                                                                                              dropdownButton(circle=F,size="default",label="",icon=rep_ui_icon("download"),width="300px",right=F,
                                                                                                             tags$b("Download depicted network:"),
                                                                                                             downloadButton(outputId="network_vendor_download_html", label="Network as .html", class="mt_download"),
                                                                                                             br(),tags$b("Download network edges as table:"),
                                                                                                             downloadButton(outputId="network_vendor_download_csv", label=".csv", class="mt_download"),
                                                                                                             downloadButton(outputId="network_vendor_download_excel", label=".xlsx*", class="mt_download"),
                                                                                                             br(),tags$span("*xlsx download only available for Chrome")
                                                                                                             ),
                                                                                              footer=NULL
                                                                              )
                                                                       ),
                                                                       column(5,
                                                                       "Knowledge-based networks were constructed based on the super- and sub-pathway structure that we implemented. This structure provides a quick overview of available metabolites from different platforms.")
                                                                       
                                                                       
                                                                       )
                                              )
                       )),
              mt_box(id="net_panel", min_height="750px",title=uiOutput("network_title"),maximizable=TRUE,width = 12,title_color = "#ddd",
                     header_button = tagList(
                       mt_box_right_item(title="Highlight bag:", title_inline = F,switchInput(inputId = "network_highlight", value = TRUE, width="auto", onLabel="on",offLabel="off",offStatus = "secondary",onStatus = "warning")),
                       mt_box_right_item(title="Color by:",title_inline = F,
                                         pickerInput("network_node_color", label=NULL, width="100%",options=list(`dropdown-align-right`=T),choices=list("Annotated super-pathways"="super", "Metabolomics platforms"="platform", "Temporal changes"="time"),choicesOpt = list(subtext = c(" ", " ","animated from challenge baseline")))),
                       mt_box_right_item(title="Show single nodes:",title_inline = F,switchInput(inputId = "network_singles",value=T, onLabel="on",offLabel = "off", width = "auto",offStatus = "secondary",onStatus = "warning"))
                     ),
                     div(   # network body
                       #style="width:100%; height:100%;",
                       div(style="height:600px;width:100%;",
                           conditionalPanel('input.network_node_color=="time"',
                                            style="padding:10px;",
                                            uiOutput("networks-legendSliderbuttonsUI"),
                                            div(id="class_net_slider",
                                                sliderTextInput(inputId = "network_time_slider", label=NULL,hide_min_max=T,grid=T,width="100%",force_edges=T,
                                                                choices=as.character(info_network$challenge_time)[which(info_network$timepoint %in% 1:56)],
                                                                animate= animationOptions(interval = 1000, loop = TRUE,
                                                                                          playButton =actionButton("networks-sliderbuttonPlay",rep_ui_icon("play"),interval = 2000,label="play",style="background-color:#d2310c;color:black;opacity:1;"),
                                                                                          pauseButton = actionButton("networks-sliderbuttonPause",rep_ui_icon("pause"),label="pause",style="background-color:##d2310c;")),
                                                                selected=as.character(info_sample$challengeTime)[2])
                                            )
                           ),
                           visNetworkOutput("display_network",width="100%",height="100%")
                       ),
                       div( # network legend
                         style="width:100%;height:20%;font-size:12px;",
                         ## network legend color
                         conditionalPanel('input.network_node_color=="time"',style="text-align:center;color:grey;",
                                          column(3,
                                                 tags$b("Fold change", id="tt_col_fc"),
                                                 bsTooltip("tt_col_fc",title="Bubble color depicts the log2 foldchange between two time points."),
                                                 img(src = "img/network/network_foldchange.png", style="height:30px;margin-left:auto;margin-right:auto;display:block;")
                                          ),
                                          column(3,
                                                 tags$b("p Value", id="tt_col_pval"),
                                                 bsTooltip("tt_col_pval",title="Bubble size depicts the -log10(p value) between two time points."),
                                                 img(src = "img/network/network_pvalue.png", style="height:30px;margin-left:auto;margin-right:auto;display:block;")
                                          ),
                                          column(3,
                                                 tags$b("Selected metabolite", id="tt_col_highlight"),
                                                 bsTooltip("tt_col_highlight",title="Yellow border color and label background highlight metabolites selected within the current bag."),
                                                 img(src = "img/network/network_highlight.png", style="width:30px;margin-left:auto;margin-right:auto;display:block;")
                                          )
                                          ),
                         conditionalPanel('input.network_node_color=="platform"',
                                          div(
                                            style="float:left",
                                          awesomeCheckboxGroup(inputId="network_legend_platform", label="Selected platforms",status = "danger",
                                                               selected= unique(db_network$selected$network$x$nodes$platform_name),
                                                               choices = unique(db_network$selected$network$x$nodes$platform_name),
                                                               inline=F)
                                          )
                         ),
                         conditionalPanel('input.network_node_color=="super"',
                                          div(
                                            style="float:left",
                                            awesomeCheckboxGroup(inputId="network_legend_super", label="Selected super-pathways",status = "warning",
                                                                 selected=unique(db_network$selected$network$x$nodes$SUPER.PATHWAY),
                                                                 choices = unique(db_network$selected$network$x$nodes$SUPER.PATHWAY),
                                                                 inline=F)
                                          )
                                          
                         ),
                         conditionalPanel('input.network_tabbox=="mggm"', 
                         div(
                           style="float:left;padding-left:20px;",
                           div(tags$b("Node shape"),style="padding-bottom:10px"),
                           div(
                             tags$span(tags$span(HTML("&#x25CB"),style="font-size:20px;"), tags$span("plasma metabolite")),
                             tags$span(style="margin-left:10px",tags$span(HTML("&#x25A1"),style="font-size:20px;"), tags$span("urine metabolite"))
                           )
                           )
                         ),
                         conditionalPanel('input.network_tabbox=="db"',
                                          div(
                                            style="float:left;padding-left:20px;",
                                            div(tags$b("Node shape"),style="padding-bottom:10px"),
                                            div(
                                              tags$span(tags$span(HTML("&#x25CB"),style="font-size:20px;"), tags$span("metabolite")),
                                              tags$span(style="margin-left:10px",tags$span(HTML("&#x2B2D"),style="font-size:20px;"),tags$span("super-pathway")), 
                                              tags$span(style="margin-left:10px",tags$span(HTML("&#x25B3"),style="font-size:20px;"), tags$span("sub-pathway"))
                                            )
                                          )
                         ),
                         
                         ## network legend edges
                         conditionalPanel('input[["networks-backbone"]]=="kegg"',style="text-align:center;color:grey;",
                                          tags$b("Measured metabolite"),
                                          img(src = "img/network/network_legend_measured.png", style="width:20px;margin-left:auto;margin-right:auto;display:block;"),
                                          tags$b("Not measured"),
                                          img(src = "img/network/network_legend_not_measured.png", style="width:20px;margin-left:auto;margin-right:auto;display:block;")
                         )
                       )
                       
                     ),
                     footer =NULL
              )
      ),
      #    Statistics (Statistical analysis = statistics) ####
      tabItem(tabName = "statistics",
              fluidRow(style="margin:0",
                       h1("Statistical methods",style='font-size:60px;font-weight:1000;text-align:center'),
                       h4("Interactive visualization of selected statistical methods for longitudinal exploration of the HuMet dataset.",style="text-align:center"),
                       rep_ui_vline(),
                       column(12, style="text-align:center",
                              rep_showcase_button(id="stats_divert_ttest",title="t Test",subtitle="Comparison of two or more time points by paired t tests",
                                                  img_src = "img/logo/logo_ttest.png")#,
                              # rep_showcase_button(id="stats_divert_pca",title="PCA",subtitle="Principle component analysis with multiple platforms",
                              #                     img_src = "img/logo/logo_pca.png")
                       )
                       
              )
      ),
      tabItem(tabName = "stats_ttest",
              fluidRow(style="padding:10px 5px; color:black; border-top:1px solid #efefefef;background:#ddd",
                       column(12,
                              mt_submenu_item(title="Only include metabolites in bag",
                                              switchInput(inputId = "stats_ttest_met", value = TRUE, width="100%", onLabel="on",offLabel="off",offStatus = "secondary",onStatus = "warning")
                              ),
                              mt_submenu_item(title="Time points",
                                              pickerInput(inputId = "stats_ttest_tp",label=NULL, selected="all",choices=list("Last vs. first"="selected","All vs. first"="all"),width='auto')
                              ),
                              mt_submenu_item(title="Multiple testing correction",
                                              pickerInput(inputId = "stats_ttest_thresh",label=NULL,
                                                          choices=list("no correction"="none","Bonferroni"="bonferroni","FDR"="fdr"),
                                                          choicesOpt = list(disabled = c(F,F,F),style = ifelse(c(T,F,F), yes = "color: rgba(119, 119, 119, 0.5);",no = "")),
                                                          selected="none",width='auto')
                              ),
                              mt_submenu_item(title="Download",
                                              dropdownButton(circle=F,size="default",label="",icon=rep_ui_icon("download"),width="300px",right=F,
                                                             tags$b("Download volcano plot as:"),
                                                             downloadButton(outputId="stats_ttest_volcano_html", label="standalone .html", class="mt_download"),
                                                             #downloadButton(outputId="stats_ttest_volcano_png", label="Volcano plot as .png", class="mt_download"),
                                                             br(),
                                                             tags$b("Download T test results as:"),
                                                             downloadButton(outputId="stats_ttest_table_csv", label=".csv", class="mt_download"),
                                                             downloadButton(outputId="stats_ttest_table_excel", label=".xlsx*", class="mt_download"),
                                                             br(),tags$span("*xlsx download only available for Chrome")
                                              )
                              )
                       )
                       
              ),
              fluidRow(style="margin:0;margin-top:20px;",
                       mt_box(title="Volcano plot",closable = F, collapsible = T,solidHeader = T, id="stats_ttest_volcano_box",maximizable = T,title_color = "#efefef",
                              header_button=pickerInput("stats_ttest_color",label=NULL,choices= list("Annotated super-pathways"="super"),width='auto'), #,"Metabolomics platforms"="platform"
                              uiOutput("stats_ttest_volcano_placeholder"),
                              #div(`data-display-if` = '!input.stats_ttest_met',style="min-height:600px;",
                                  shinycssloaders::withSpinner(type=5, color="#d2310b", proxy.height="600px",
                                                               plotlyOutput('stats_ttest_volcano', width = "100%", height="600px")
                                  
                              ),
                              footer=div(`data-display-if` = '!input.stats_ttest_met',uiOutput("stats_ttest_volcano_description"))
                       ),
                       mt_box(title=tags$span("Pairwise t Test results",style="padding:5px"),closable = F, collapsible = T,solidHeader = T,title_color = "#efefef",
                              id="stats_ttest_table_box",maximizable = T,
                              uiOutput("stats_ttest_table_placeholder"),
                              div(style="min-height:627px; overflow:scroll;",DTOutput("stats_ttest_table", width="100%", height="100%")
                              )
                       )
              )
      ),
      # add stats pca ####
      tabItem(tabName = "stats_pca",
              fluidRow(style="padding:10px 5px; color:black; border-top:1px solid #efefefef;background:#ddd",
                       column(12,
                              mt_submenu_item(title="Fluid",
                                              pickerInput(inputId = "stats_pca_fluid", label = NULL,selected=unique(options$stats$pca$fluid)[1],
                                                          choices = unique(options$stats$pca$fluid), multiple = FALSE,width =  '100%'),
                                              footer=NULL),
                              mt_submenu_item(title="Platforms",
                                              pickerInput(inputId = "stats_pca_platform", label = NULL,
                                                          selected=c("Metabolon HD4 [nt-ms]","Biocrates p150 [t-ms]","In-house biochemistry [chem.]","numares (Lipofit) [NMR]"),
                                                          choices = c("Metabolon HD4 [nt-ms]","Biocrates p150 [t-ms]","In-house biochemistry [chem.]","numares (Lipofit) [NMR]","Lipidyzer [nt-ms]", "Chenomx [NMR]"),
                                                          multiple = T,width =  '100%'),
                                              footer=NULL),
                              mt_submenu_item(title="PC dimensions",
                                              switchInput(inputId="stats_pca_dim", onLabel="3D", offLabel="2D", onStatus = "secondary", offStatus = "secondary", width='100%'),
                                              footer=NULL),
                              mt_submenu_item(title="Download",
                                              dropdownButton(circle=F,size="default",label="",icon=rep_ui_icon("download"),width="300px",right=F,
                                                             tags$b("Plots:"),
                                                             downloadButton(outputId="stats_pca_scores_html", label="Scores plot as .html", class="mt_download"),
                                                             downloadButton(outputId="stats_pca_loadings_html", label="Loadings plot as .html", class="mt_download")#,
                                                             #br(),tags$b("PCA data:"),
                                                             #downloadButton(outputId="stats_pca_table_csv", label="PCA data as .csv", class="mt_download")
                                              ),
                                              footer=NULL
                              )
                       )
              ),
              fluidRow(style="margin:0;margin-top:20px;",
                       mt_box(title="Scores plot",closable = F, collapsible = T,solidHeader = T, id="statistics_PCAscoresBox",maximizable = T, title_color = "#efefef",
                              header_button=tagList(
                                #mt_box_right_item(title="Dynamic time points:", title_inline = T,
                                #                  div(switchInput(inputId="stats_scores_plot_addTrace",value=F, onLabel="show", offLabel = "hide",onStatus="warning", offStatus = "secondary", width="auto"))
                                #),
                                mt_box_right_item(title=NULL, title_inline = F,
                                                  pickerInput("stats_scores_plot_colorBy",label=NULL,width='auto',
                                                              choices=list("Subject"="subject","Time point"="timepoint","Challenge"="challenge", "Default"="default"))
                                )
                              ),
                              shinycssloaders::withSpinner(type=5, color="#d2310b", proxy.height="600px",
                                                           plotly::plotlyOutput("stats_scores_plot",width="100%",height="100%")),
                              footer= div(
                                div(`data-display-if` = 'input.stats_scores_plot_addTrace == true',
                                    sliderInput(inputId="stats_scores_plot_time",label="Add trace",min = 1, max = 56, value = c(21,29))
                                ),
                                p("This scores plot depicts the first two/three principle components and corresponds to the observations (samples) of the HuMet study."))
                       ),
                       mt_box(title="Loadings plot", closable = F, collapsible = T,solidHeader = T, id="statistics_PCAloadingsBox",maximizable = T,title_color = "#ddd",
                              header_button=tagList(
                                mt_box_right_item(title=NULL, title_inline = T,
                                                  pickerInput("stats_loadings_plot_colorBy",label=NULL,width='auto',choices=list("Annotated super-pathways"="super", "Metabolomics platforms"="platform","No color"="default")))),
                              shinycssloaders::withSpinner(type=5, color="#d2310b", proxy.height="600px",
                                                           plotly::plotlyOutput("stats_loadings_plot",width="100%",height="100%")),
                              footer=p("This loadings plot depicts the variables (metabolites) that have the largest influence on the PCA. Variables with high loadings are more likely to be important for discriminationg groups that are separated in the scores plot.")
                       )
              )
      ),
      
      # Download ( = download) ####
      tabItem(tabName = "download",
              fluidRow(
                h1("Download data", style="text-align:center")
              ),
              fluidRow(style="margin:0;",
                       shinydashboardPlus::box(
                         solidHeader = T,
                         width=12,
                         closable = F,
                         title = "Bulk download",
                         tags$table(
                           style = "width:100%; border:1px solid black;",
                           tags$tr(
                             style="background: #2c323a; color:white",
                             tags$th("Download", style="padding:5px;"),
                             tags$th("Selected information", style="padding:5px;border-left:1px solid grey;"),
                             tags$th("Description", style="padding:5px;border-left:1px solid grey;"),
                             tags$th("Changes", style="padding:5px;border-left:1px solid grey;")
                           ),
                           tags$tr(
                             tags$td(
                               style="padding:5px",
                               div(shiny::downloadLink(outputId = "download_bluk_data_csv", label = "humet_data.csv",class="rep_link"),style="display:block"),
                               div(shiny::downloadLink(outputId = "download_bluk_data_xlsx", label = "*humet_data.xlsx",class="rep_link"),style="display:block")
                              ),
                             tags$td(uiOutput(outputId = "download_bulk_data_info")),
                             tags$td(
                               tags$p("Dataframe with data as selected in the header.")), 
                             tags$td(
                               tags$b("How to change the settings"),br(),
                               tags$img(src="img/tutorial/tutorial_headerfiltering.png",style="max-height:40px;max-width:100%"),br(),
                               tags$span("Go to modules"),
                               shiny::actionLink(inputId = "download_bulk_data_divertselection",class="rep_link",label=tags$em("Selection")),
                               tags$span(" or "),
                               shiny::actionLink(inputId = "download_bulk_data_diverttimecourse",
                                                 label=tags$em("Time course"), class="rep_link"),
                               tags$span( "and change settings in the top right header (shown in figure above)")
                             )
                           ),
                           tags$tr(
                             tags$td(
                               style="padding:5px",
                               div(shiny::downloadLink(outputId = "download_bluk_info_csv", label = "humet_info.csv",class="rep_link"),style="display:block"),
                               div(shiny::downloadLink(outputId = "download_bluk_info_xlsx", label = "*humet_info.xlsx",class="rep_link"),style="display:block"),
                             ),
                             tags$td(uiOutput(outputId = "download_bulk_info_info")),
                             tags$td(
                               tags$p("Dataframe with column information as selected in the header.")
                                     ),
                             tags$td()
                           )
                           ),
                         column(12, "*xlsx download only available for Chrome")
                         #shiny::downloadButton(outputId = "download_download_bluk", label = "download ")
                       )
              ),
              fluidRow(style="margin:0;",
                       shinydashboardPlus::box(
                         solidHeader = T,
                         width=12,
                         closable = F,
                         title = "Dataset download",
                         tags$table(
                           style = "width:100%; border:1px solid black;",
                           tags$tr(
                             style="background: #2c323a; color:white",
                             tags$th("Download", style="padding:5px;"),
                             tags$th("Last Published", style="padding:5px;border-left:1px solid grey;"),
                             tags$th("Publication", style="padding:5px;border-left:1px solid grey;"),
                             tags$th("Description", style="padding:5px;border-left:1px solid grey;")
                           ),
                           tags$tr(
                             tags$td(
                               style="padding:5px",
                               shiny::downloadLink(outputId = "postprandial_non_imputed_csv", label = "postprandial_non_imputed.csv",class="rep_link"),
                               br(),
                               shiny::downloadLink(outputId = "postprandial_non_imputed_excel", label = "*postprandial_non_imputed.xlsx",class="rep_link")),
                             tags$td("2022-07-2 10:44:03 CEST"),
                             tags$td(rep_href(href="https://www.frontiersin.org/articles/10.3389/fnut.2022.933526/full",label="Weinisch et al. 2022, Frontiers in Nutrition")),
                             tags$td("Dataframe containing all challenge time points used for the comparison of dietary intake challenges. Data was log2 transformed.")
                           ),
                           tags$tr(
                             tags$td(
                               style="padding:5px",
                               shiny::downloadLink(outputId = "postprandial_imputed_csv", label = "postprandial_imputed.csv",class="rep_link"),
                               br(),
                               shiny::downloadLink(outputId = "postprandial_imputed_excel", label = "*postprandial_imputed.xlsx",class="rep_link")),
                             tags$td("2022-07-2 10:44:03 CEST"),
                             tags$td(rep_href(href="https://www.frontiersin.org/articles/10.3389/fnut.2022.933526/full",label="Weinisch et al. 2022, Frontiers in Nutrition")),
                             tags$td("Dataframe containing all challenge time points used for the comparison of dietary intake challenges. Data was log2 transformed and missing values were imputed using RF-based imputation (missForest version 1.4)")
                           ),
                           tags$tr(
                             tags$td(
                               style="padding:5px",
                               shiny::downloadLink(outputId = "postprandial_info_csv", label = "postprandial_info.csv",class="rep_link"),
                               br(),
                               shiny::downloadLink(outputId = "postprandial_info_excel", label = "postprandial_info.xlsx",class="rep_link")),
                             tags$td("2022-07-2 10:44:03 CEST"),
                             tags$td(rep_href(href="https://www.frontiersin.org/articles/10.3389/fnut.2022.933526/full",label="Weinisch et al. 2022, Frontiers in Nutrition")),
                             tags$td("Annotation of metabolites.")
                           ),
                           
                           tags$tr(style="border-top:1px solid ",
                                   tags$td(
                                     style="padding:5px",
                                     shiny::downloadLink(outputId = "bile_acid_csv", label = "bile_acid.csv",class="rep_link"),
                                     br(),
                                     shiny::downloadLink(outputId = "bile_acid_excel", label = "*bile_acid.xlsx",class="rep_link")),
                                   tags$td("2022-26-10 10:32:03 CEST"),
                                   tags$td(rep_href(href="https://www.frontiersin.org/articles/10.3389/fnut.2022.932937/full",label="Fiamoncini et al. 2022, Frontiers in Nutrition")),
                                   tags$td("Protein precipitation using Phenomenex ImpactTM protein precipitation plates. Details can be found here: 
Fiamoncini J, Rist MJ, Frommherz L, Giesbertz P, Pfrang B, Kremer W, Huber F, Kastenmüller G, Skurk T, Hauner H, Suhre K, Daniel H and Kulling SE (2022) Dynamics and determinants of human plasma bile acid profiles during dietary challenges. Front. Nutr. 9:932937. doi: 10.3389/fnut.2022.932937",br(),"For the quantitative determination of BA a Nexera LC system (Shimadzu Europa GmbH, Duisburg, Germany) coupled to a 5500 Q-Trap mass spectrometer (Sciex, Darmstadt, Germany) was used with electrospray ionization in the negative mode . Source parameters were: 40 psi (curtain gas), 600°C (Source Temperature), -4500 V (Ion Spray Voltage), 50 psi/60 psi (Ion Gas 1 and 2, respectively). Data were recorded in the multiple reaction monitoring mode (MRM) with nitrogen as a collision gas. System operation and data acquisition were done using Analyst 1.5.2. software (AB Sciex). 
                                           ")
                           ),
                           tags$tr(
                             tags$td(
                               style="padding:5px",
                               shiny::downloadLink(outputId = "bile_acid_info_csv", label = "bile_acid_info.csv",class="rep_link"),
                               br(),
                               shiny::downloadLink(outputId = "bile_acid_info_excel", label = "*bile_acid_info.xlsx",class="rep_link")),
                             tags$td("2022-26-10 10:32:03 CEST"),
                             tags$td(rep_href(href="https://www.frontiersin.org/articles/10.3389/fnut.2022.932937/full",label="Fiamoncini et al. 2022, Frontiers in Nutrition")),
                             tags$td("Annotation of bile acids.")
                           )
                           #,
                           # tags$tr(
                           #   tags$td(
                           #     style="padding:5px",
                           #     shiny::downloadLink(outputId = "complete_metabolite_data_log2", label = "complete_metabolite_data_log2.csv")),
                           #   tags$td("2022-06-30 22:44:03 CEST"),
                           #   tags$td("Manuscript in preparation"),
                           #   tags$td("Dataframe containing all measured metabolites. Data was log2 transformed.")
                           # ),
                           # tags$tr(
                           #   tags$td(
                           #     style="padding:5px",
                           #     shiny::downloadLink(outputId = "complete_metabolite_data_log2_imputed", label = "complete_metabolite_data_log2_imputed.csv")),
                           #   tags$td("2022-06-30 22:44:03 CEST"),
                           #   tags$td("Manuscript in preparation"),
                           #   tags$td("Dataframe containing all measured metabolites. Data was log2 transformed and missing values were imputed using RF-based imputation (missForest version 1.4)")
                           # )
                         ),
                         column(12,"*xlsx download only available for Chrome")
                       )
                       
                       
              ),
              fluidRow(
                div(
                  #shiny::selectInput(inputId = "download_paper_postprandial", label="Download full dataset", choices=c("curated_imputed.csv"="imp", "curated_unimputed.csv"="non_imp")
                )
              )
      ),
      
      ### about_team----
      tabItem(tabName="about_team",
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_header(title="About us",
                                            question="The development team",
                                            buttons=column(12,style="padding-bottom:10px",
                                                           tags$span("Jump to:", style="color:white"),
                                                           rep_jump_to(label="Developers", id="box_about_team_developers"),
                                                           rep_jump_to(label="Contact", id="box_about_team_contact"),
                                                           rep_jump_to(label="Acknowledgements", id="box_about_profiling_acknowledgements")
                                            )
                        )
              ),
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_panel(width=12,title = "Developers",id="box_about_team_developers",
                                           column(12,
                                                  tags$span("The HuMet Repository is a project of the ", 
                                                            a(href="https://www.helmholtz-muenchen.de/icb/research/groups/kastenmueller-lab/metabolomics/about-us/index.html",target="_blank","Kastenmüller lab @ ICB Munich", style="color:black"),
                                                            "at the",
                                                            a(href="https://www.helmholtz-muenchen.de/",target="_blank","Helmholtz Zentrum München", style="color:black"),
                                                            ". It was mainly developed and maintained by all authors mentioned as followed.", style="text-align:center")),
                                           column(width=2,
                                                  tags$img(src = "img/about/developer_patrick.jpg", style="margin-left:auto; margin-right:auto;border-radius:50%;display:block;"),
                                                  #tags$span("PhD Student",br(),"Longitudinal metabolomics",style="color:black;display:block;text-align:center;"),
                                                  tags$b(a(href="https://www.helmholtz-munich.de/en/icb/research-groups/kastenmueller-lab",target="_blank", font="black","Patrick Weinisch", style="color:black;display:block;text-align:center;")),
                                                  div(style="font-size:20px; text-align:center",
                                                      rep_social_button(url = "https://twitter.com/WeinischPatrick",type = "twitter"),
                                                      rep_social_button(url = "https://www.linkedin.com/in/patrick-weinisch-1b900a148/", type="linkedin"),
                                                      rep_social_button(url="patrick@weinisch.com",  type="mail"))
                                           ),
                                           column(width=2,
                                                  tags$img(src = "img/about/developer_johannes.jpg", style="margin-left:auto; margin-right:auto;border-radius:50%;display:block;"),
                                                  #tags$span("Team Leader",br(),"Data Visualization and Integration Tools",style="color:black;display:block;text-align:center;"),
                                                  tags$b("Dr. Johannes Raffler", style="color:black;display:block;text-align:center;"),
                                                  div(style="font-size:20px; text-align:center",
                                                      rep_social_button(url = "https://twitter.com/JohannesRaffler",type = "twitter")
                                                  )
                                           ),
                                           column(width=2,
                                                  tags$img(src = "img/about/developer_gabi.jpg", style="margin-left:auto; margin-right:auto;border-radius:50%;display:block;"),
                                                  #tags$span("Group Leader @ICB",br(),"Systems Metabolomics",style="color:black;display:block;text-align:center;"),
                                                  tags$b(a(href="https://www.helmholtz-munich.de/en/icb/pi/gabi-kastenmueller",target="_blank", font="black", "Dr. Gabi Kastenmüller", style="color:black;display:block;text-align:center;")),
                                                  div(style="font-size:20px; text-align:center",
                                                      rep_social_button(url = "https://twitter.com/KastenmullerLab",type = "twitter"),
                                                      rep_social_button(url = "https://www.linkedin.com/in/gabi-kastenm%C3%BCller-8b4b5576/", type="linkedin"),
                                                      rep_social_button(url="g.kastenmueller@helmholtz-munich.de",  type="mail"))
                                           ),
                                           column(width=3,
                                                  tags$img(src = "img/about/developer_werner.jpg", style="margin-left:auto; margin-right:auto;border-radius:50%;display:block;max-height:133px;"),
                                                  tags$b(a(href="https://www.helmholtz-munich.de/en/icb/research-groups/kastenmueller-lab/staff/werner-roemisch-margl",target="_blank", font="black","Dr. Werner Römisch-Margl", style="color:black;display:block;text-align:center;")),
                                                  div(style="font-size:20px; text-align:center",
                                                      rep_social_button(url="werner.roemischmargl@helmholtz-munich.de",  type="mail"))
                                           ),
                                           column(width=2,
                                                  div(
                                                    div(tags$b("Former developers:")), 
                                                    div("Maria Littmann")
                                                  )
                                           )
                                           
                        ),
                        rep_showcase_panel(width=12,title = "Contact",id="box_about_team_contact",
                                           p("If you have any questions regarding the HuMet repository feel free to contact us:",
                                             a(href="mailto:patrick@weinisch.com",target="_blank","patrick@weinisch.com", style="color:#d2310b"),
                                             ".")
                        ),
                        rep_showcase_panel(width=12,title = "Acknowledgements",id="box_about_team_acknowledgements",
                                           p("We would also like to thank all research groups that generated the data integrated into the HuMet repository:"),
                                           p("Krug S, Kastenmüller G, Stückler F, Rist MJ, Skurk T, Sailer M, Raffler J, Römisch-Margl W, Adamski J, 
                      Prehn C, Frank T, Engel KH, Hofmann T, Luy B, Zimmermann R, Moritz F, Schmitt-Kopplin P, Krumsiek J, Kremer W, 
                      Huber F, Oeh U, Theis FJ, Szymczak W, Hauner H, Suhre K, Daniel H. ",a(href="https://www.ncbi.nlm.nih.gov/pubmed/22426117",target="_blank",
                                                                                             tags$b("The dynamic range of the human metabolome revealed by challenges.", style="color:#274672"))," FASEB J. 2012 Jun;26(6):2607-19."))
              )
      ),
      ### about_profiling ----
      tabItem(tabName="about_profiling",
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_header(title="Metabolite profling",
                                            question='Measurement and identification of metabolites',
                                            buttons=column(12,style="padding-bottom:10px",
                                                           tags$span("Jump to:", style="color:white"),
                                                           rep_jump_to(label="Platforms", id="box_about_profiling_profiling"),
                                                           rep_jump_to(label="Annotation", id="box_about_profiling_annotation"),
                                                           rep_jump_to(label="Metabolon HD4", id="box_about_profiling_metabolon"),
                                                           rep_jump_to(label="Lipidyzer", id="box_about_profiling_lipidyzer")
                                            )
                        )
              ),
              
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_panel(width=12,
                                           title = "Platforms",
                                           id="box_about_profiling_profiling",
                                           tags$div(
                                             style="margin-left:auto;margin-right:auto;",
                                             tableHTML::tableHTML(about[["profiling_table"]],
                                                                  escape=F,
                                                                  caption="Table 1. Overview of data provided within the resource.",
                                                                  rownames=F,border=0, spacing="3px", round=1, collapse="separate_shiny")
                                           )
                        ),
                        
                        
                        rep_showcase_panel(width=12,title = "Metabolite annotation",id="box_about_profiling_annotation",
                                           "To provide an overview of measured metabolites, we implemented a super-pathway and sub-pathway structure. 
                                           Metabolites identified from our in-house platforms were manually assigned to this structure and matched to knowledge-based platforms such as KEGG, PubChem, and HMDB.
                                           For identified metabolites from vendor-based kits, we used annotation provided by the Metabolon HD4 and Biocrates p150 platforms to classify them into our super- / sub-pathway structure."
                        ),
                        rep_showcase_panel(width=6,title = "Methods Metabolon HD4",id="box_about_profiling_metabolon",
                                           tags$p("In addition to the published metabolomics data, we acquired new data by profiling plasma and urine samples on the non-targeted LC-MS based platform Metabolon HD4 at Metabolon, Inc. (Durham, NC, USA).
                                                  First recovery standards were added to the samples for quality control purposes. 
                                                  Thereafter the samples were prepared using the automated MicroLab STAR® system from Hamilton Company (Reno, NV, USA), to remove proteins. ",
                                                  "The resulting sample was split into five portions, stored overnight under nitrogen and reconstituted in solvents compatible for the methods before each analysis."),
                                                  br(),
                                           tags$p("Metabolite detection and quantification were performed on four different chromatography platforms:"),
                                           tags$ul(
                                             tags$li("Two separate reverse Phase (RP)/ ultra-high-performance liquid-phase chromatography (UPLC)-MS/MS methods with positive ion mode electrospray ionization (ESI)"),
                                             tags$li("HILIC/UPLC-MS/MS with negative mode ESI"),
                                             tags$li("(RP)/UPLC-MS/MS with positive ion mode ESI"),
                                             tags$li("RP/UPLC-MS/MS with negative ion mode ESI")),
                                             "Thereafter, organic solvent was removed by placing the samples on a TurboVap® (Zymark).",
                                           tags$p("All four chromatography methods were coupled with a Thermo Scientific Q-Exactive high resolution/accuracy mass spectrometer (MS/MS) applying standard protocols developed by Metabolon(",
                                                  rep_href(href="https://doi.org/10.4172/2153-0769.1000132",label="Evans et al. 2014"),")"),br(),
                                           tags$p("Two types of controls were measured to assess the mean relative standard deviation (RSD) per metabolites: "),
                                           tags$ul(
                                             tags$li("Pooled matrix samples (CMTRX) generated from all HuMet samples to assess biological variability versus process variability;"),
                                             tags$li("Vendor maintained human plasma samples (MTRX) to assess across Metabolon HD4 measured studies.")
                                           ),
                                           tags$p("The area-under-the-curve was used to quantify peaks, giving rise to relative levels of a total of 595 known metabolites in the plasma and 620 in urine.
                                                  Metabolites of the Metabolon HD4 platform were assigned to 8 super-pathway classes (amino acids, carbohydrates, cofactors and vitamins, energy, lipids, nucleotides, peptides, xenobiotics), each being divided into two or more sub-pathways), resulting in a total of 78 and 68  sub-pathways for the plasma and urine metabolites, respectively. ")
                        ),
                        rep_showcase_panel(width=6,title = "Methods Lipidyzer", id="box_about_profiling_lipidyzer",
                                           tags$p("Lipid concentrations in HuMet plasma samples of four participants were analyzed on the LipidyzerTM platform of AB Sciex Pte. Ltd. (Framingham, MA, USA) by Metabolon Inc., Durham, NC, USA. A detailed protocol for lipid quantification of the HuMet study samples is published in (",
                                           rep_href(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6631474/",label="Quell et al., 2019",),")."),
                                           tags$p( "In brief, dichloromethane and methanol were used to extract lipids within a Bligh-Dyer extraction. For analysis, the lower, organic phase which includes internal standards was used and concentrated under nitrogen. 0.25 mL of dichloromethane:methanol (50:50) containing 10 mM ammonium acetate was used for reconstitution. The result was placed in vials for infusion-MS analysis which was performed on a Sciex 5500 QTRAP equipted with SelexIONTM differential ion mobility spectrometry. Phosphatidylcholines were detected in the negative MRM mode and quantified using 10 stable isotope labeled compounds."),
                                           tags$p("The Lipidyzer platform allowed for absolute quantification of 965 lipids distributed over 14 lipid classes: (CE, TAG, DAG, FFA, PC, PE, PI, LPC, LPE, SM, CER, HCER, LCER, DCER). ")
                        )
              )
      ),
      ### about_processing -----
      tabItem(tabName="about_processing",
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_header(title="Metabolite profling",
                                            question='Measurement and identification of metabolites',
                                            buttons=column(12,style="padding-bottom:10px",
                                                           tags$span("Jump to:", style="color:white"),
                                                           rep_jump_to(label="Batch correction", id="box_about_processing_batch"),
                                                           rep_jump_to(label="Manual curation", id="box_about_processing_curation"),
                                                           rep_jump_to(label="Imputation", id="box_about_processing_imputation"),
                                                           rep_jump_to(label="Data transformation", id="box_about_processing_transformation")
                                            )
                        )
              ),
              
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_panel(width=12,title = "Batch correction", id="box_about_processing_batch",
                                           "The metabolic raw area counts from the non-targeted LCMS platform were corrected for instrument inter-day tuning differences by setting the run-day medians equal to one. Additionally, data derived from urine samples were normalized by osmolality before run-day correction. The resulting transformation of these normalized datasets is termed “Concentrations and relative abundances” within the repository and can be downloaded in bulk."
                        ),
                        rep_showcase_panel(width=12,title = "Manual curation", id="box_about_processing_curation",
                                           "Due to large fluctuations in metabolic concentrations, we applied manual data curation by systematically filtering single data points according to the following two criteria:",
                                           tags$li("The value of the single data point was outside 4 times the standard deviation (SD) of this time point."),
                                           tags$li("The data point was not measured within the first 30 minutes of a study challenge"),
                                           "A total of 163 data points fit both criteria, of which 92 were excluded after manual inspection. 
                                           The resulting data has been deposited as 'Concentrations and relative abundances' within the HuMet repository. 
                                           Data deposited in the repository does not contain manually excluded data points."
                                           
                        ),
                        rep_showcase_panel(width=12,title = "Imputation",id="box_about_processing_imputation",
                                           fluidRow(
                                             column(6,
                                                    tags$span("Some of our statistical models require a complete dataset without missing values. 
                                                              This includes network inference with gaussian graphical models and principle component analysis. To address missing values, we used the manually curated dataset for imputation."),
                                                    tags$b("Imputation methods used for metabolites < 30% missingness:"),br(),
                                                    tags$ul(
                                                      tags$li(rep_href(href="https://cran.r-project.org/web/packages/missForest/", label="missForest")," missForest with parameters set to: ntree=1500 and mtry=22. R package version: missForest 1.4"),
                                                      tags$li(rep_href(href="https://github.com/krumsieklab/maplet/tree/main", label="KNN"), " mt_pre_impute_knn with parameters set to: k=10, method ='knn.obs.euc.sel'. R package version: maplet 1.0. Missingness of a complete sample can't be imputed using this method."),
                                                      tags$li(rep_href(href="https://cran.r-project.org/web/packages/mice/index.html", label="PMM"), " futuremice with parameters set to method = '2lonly.pmm'",". R package version: mice 3.15.0"),
                                                      tags$li("Linear imputation", " using the approx function with parameters set to method='linear'. Values were imputed per subject per block and only if the previous and subsequent time point are available. R Package version: stats 4.2.3. Missingness of a complete sample can't be imputed using this method.")
                                                      )
                                                    ),
                                             column(6,
                                                    tags$div(
                                                      style="margin-left:auto;margin-right:auto;",
                                                      tableHTML::tableHTML(about[["imputation_table"]],
                                                                           caption="Table 1. Overview of measured tissues, platforms and number of metabolites that were not imputed.",
                                                                           rownames=F,border=0, spacing="3px", round=1, collapse="separate_shiny")
                                                    ))
                                             
                                                       
                                           )
                      

                        ),
                        rep_showcase_panel(width=12,title = "Data transformation",id="box_about_processing_transformation",
                                           "Users can select between two different types of data transformation based on the dataset with or without imputation. The following options are available:",
                                           tags$li("Use z-scores to better compare across platforms"),
                                           tags$li("Choose fold changes calculated between user-defined time points. 
                                                   In the ",tags$em("Time Course module")," the default baseline time points are set to the first time point per block. In the",tags$em("Network module")," the default baseline is set to the baseline of each challenge. In the",tags$em("Time Course module"), "the default is set to the first time point of the user-chosen time range.")
                                           
                        )
              )
      ),
      ### about_statistics -----
      tabItem(tabName="about_analysis",
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_header(title="Statistical analysis",
                                            question='Methods used for data exploration',
                                            buttons=column(12,style="padding-bottom:10px",
                                                           tags$span("Jump to:", style="color:white"),
                                                           rep_jump_to(label="Similarity", id="box_about_analysis_similarity"),
                                                           rep_jump_to(label="Paired t-test", id="box_about_analysis_ttest"),
                                                           rep_jump_to(label="Networks", id="box_about_analysis_networks")
                                            )
                        )
              ),
              
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_panel(width=12,title = "Metabolite time course similarity ",id="box_about_analysis_similarity",
                                           tags$p("We offer several distance measures for ranking metabolites based on their similarity over time. Statistical calculation of these distance measures depends on the selected time range, included subjects, and imputation, which the user can choose individually."),                                 
                                           tags$ul(
                                             tags$li("Users can compare paired metabolite trajectories. Here, we use the z-scored dataset to calculate the distance (Euclidean, Manhattan) or correlation (Pearson) between two metabolite curves within one subject, and then sum up the distance matrices across all selected subjects. We use the dist function implemented within the " , 
                                                     rep_href(href="https://cran.r-project.org/web/packages/proxy/index.html",label="proxy")," R package to calculate the Euclidean and Manhattan distance.", 
                                                     "the",rep_href(href="https://stat.ethz.ch/R-manual/R-devel/library/stats/html/00Index.html",label="stats")," R package is used to calculate the paired Pearson correlation."),
                                             tags$li("Users can use distance measures capable of measuring the similarity between curves by taking the metabolite concentration and order of time points along the metabolite curves into account. 
                                                     The Fréchet distance is set as the default within the similarity tool, as it is more suitable for our data type. Here, we use a window approach that allows the Fréchet algorithm to search for the smallest distance between curves in a defined timeframe. This timeframe is defined as follows: a maximum of +/- 30 minutes within all challenges except extended fasting. Within extended fasting, we allow for comparison of time points within a range of +/- 120 minutes. Metabolite curves are compared by subject within the user-defined time range, and subsequently, the mean is calculated across all subjects. To calculate the Fréchet distance, we use the",
                                                     rep_href(href="https://rdrr.io/cran/kmlShape/man/distFrechet.html",label="distFrechet"), " function implemented within the",
                                                     rep_href(href="https://rdrr.io/cran/kmlShape/",label="kmlShape"), " R package.")
                                           )      
                        ),
                        rep_showcase_panel(width=12,title="Paired t-test",id="box_about_analysis_ttest",
                                           tags$p("Metabolic changes were assessed by paired t-tests of non-imputed samples using the ",rep_href(href="https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/t.test",label="t.test")," function implemented in R statistical software (version 3.6.1). 
                                           The user can select either a group of metabolites to be analyzed or analyze all measured metabolites. 
                                           To adjust for multiple testing, the user can choose between FDR (q <= 0.05) or Bonferroni correction (nMetabolites / nTime points). 
                                           The levels of adjustment depend on the chosen time range and number of metabolites submitted for analysis.
                                           Results are visualized in a volcano plot using the",rep_href(href="https://cran.r-project.org/web/packages/plotly/index.html", label="plotly")," R package. 
                                                  Each data point in the volcano plot can be colored by corresponding super-pathways or measured platform.")
                        ),
                        # rep_showcase_panel(width=12,title="PCA",id="box_about_analysis_pca",
                        #                    tags$p("The analysis is based on the imputed and scaled HuMet dataset without missing values. Here, we use the function prcomp implemented within the Rpackage stats (version 3.6.2.). The user can tailor the analysis towards their own research question by "),
                        #                    tags$ul(
                        #                      tags$li("deselecting subjects (n=15) or time points (n=56) to exclude samples excluding samples (15 subjects x 56 time points) "),
                        #                      tags$li("exclude metabolic traits (2656) by deselecting platforms. PCA results are visualized in scores and loadings plots, which can be colored user defined settings. ")
                        #                    ),
                        #                    tags$p("For visualization, we use the function plot_ly of the RPackage plotly (version 4.9.1). ")
                        # ),
                        rep_showcase_panel(width=12,title="Network generation",id="box_about_analysis_networks",
                                           tags$p("We constructed knowledge-based networks based on the super- and sub-pathway structure that we implemented. 
                                           This structure provides a quick overview of available metabolites from different platforms. The network inference of single fluid networks is based on estimating GGMs from the metabolite concentrations. 
                                                  These data-driven networks have previously been shown to reconstruct biological pathways from metabolomics data (", rep_href(href="https://bmcsystbiol.biomedcentral.com/articles/10.1186/1752-0509-5-21",label="Krumsiek et al. 2011"),")."),br(), 
                                           tags$p("Single-fluid / Multi-fluid gaussian graphical models (GGMs) are based on partial correlations. Network inference closely follows procedures established by ", 
                                           rep_href(href="https://www.nature.com/articles/s41540-017-0029-9",label="Do et al. 2017"),
                                           " but applying the dynamic partial correlation approach described by ",
                                           rep_href(href="https://www.ine.pt/revstat/pdf/rs060103.pdf",label="Opgenrhein & Strimmer 2006"),
                                           " which takes the factor ‘time’ into account. The shrinkage estimator approach ‘GeneNet’ was used as the dataset includes fewer samples (840) than variables (2656). 
                                           Network inference was based on the log2 transformed metabolomics data to better meet the assumption of a multivariate normal distribution ",
                                           "(",rep_href(href="https://pubmed.ncbi.nlm.nih.gov/31639475/",label="Altenbuchinger et al., 2020"),").",
                                           "Furthermore, we assume that relationships between variables are linear and introduce sparsity (implemented within the", rep_href(href="https://cran.rstudio.com/web/packages/GeneNet/index.html",label="GeneNet") ,"approach). ",
                                           "The user can choose to apply Bonferroni or FDR correction and cut metabolite links at specific dynamic partial correlations.")
                                          
                                           
                        )
                        
              )),
      ### about_repository -----
      tabItem(tabName="about_repository",
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_header(title="The repository",
                                            question='Generation of the HuMet repository',
                                            buttons=column(12,style="padding-bottom:10px"
                                            )
                        )
              ),
              
              fluidRow( style="margin:0;background:#ddd;",

                        rep_showcase_panel(width=12, title="Overview",id="box_about_repository_overview",
                                           tags$p("The HuMet repository was built using the ",
                                                  rep_href(href="https://shiny.rstudio.com",label="Shiny R package"),", which provides a basic structure for the interactive web application. All additionally used R packages are listed below. We wrote the majority of the code in R with custom tailoring using JS and CSS for data-specific individualization."),
                                           column(3,tags$b("Web application"),
                                                  tags$ul(
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/shiny/index.html",label="shiny - basic shiny structure")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/shinydashboard/index.html",label="shinydashboard - shiny dashboard design")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/shinydashboardPlus/index.html",label="shinydashboardPlus - shiny dashboard design")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/shinycssloaders/index.html",label="shinycssloaders - css loading screen")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/shinyjs/index.html",label="shinyjs - JS improvements")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/shinyWidgets/index.html",label="shinyWidgets - additional ui elements")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/shinyBS/index.html",label="shinyBS - additional ui elements"))
                                                  )
                                           ),
                                           column(3,tags$b("Statistical analysis"),
                                                  tags$ul(
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/longitudinalData/index.html",label="Stats - basic statistics")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/longitudinalData/index.html",label="longitudinalData - longitudinal analysis")),
                                                    tags$li(rep_href(href="https://rdrr.io/cran/kmlShape/",label="kmlShape - Frechet distance calculation")))
                                           ),
                                           column(3,tags$b("Data visualization"),
                                                  tags$ul(
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/DT/index.html",label="DT - interactive tables")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/tableHTML/index.html",label="tableHTML - basic tables")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/highcharter/index.html",label="highcharter - interactive plots")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/visNetwork/index.html",label="visNetwork - interactive networks")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/igraph/index.html",label="igraph - network layout")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/plotly/index.html",label="plotly - interactive plots")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/colorRamps/index.html",label="colorRamps - dynamic colering")))
                                           ),
                                           column(3,tags$b("Data processing"),
                                                  tags$ul(
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/doParallel/index.html",label="doParralel - parallel computing")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/readxl/index.html",label="readxl - read excel sheets")),
                                                    tags$li(rep_href(href="https://cran.r-project.org/web/packages/writexl/index.html",label="writexl - write excel sheets"))
                                                    )
                                           ),
                                           column(12,
                                                  tags$p("Upon session start, the repository loads preprocessed data, metabolite information, and sample information. The repository is then responsive to the user's options, allowing them to exclude data points based on time points, subjects, and platforms. The chosen data is then visualized in interactive plots, with customizable color options for the user."))
                        )
              )),
      ### about_study -----
      tabItem(tabName="about_study",
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_header(title="Study design",
                                            question='Overview over the HuMet (Human metabolome) study',
                                            buttons=column(12,style="padding-bottom:10px",
                                                           tags$span("Jump to:", style="color:white"),
                                                           rep_jump_to(label="Setup", id="box_about_study_setup"),
                                                           rep_jump_to(label="Challenges", id="box_about_study_challenges"),
                                                           rep_jump_to(label="Participants", id="box_about_study_participants"),
                                                           rep_jump_to(label="Samples", id="box_about_study_samples")
                                            )
                        )
              ),
              
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_panel(width=12,title = "Setup", id="box_about_study_setup",
                                           about_study_complete(tag="all_study"),
                                           footer=tags$p("The Human Metabolome (HuMet) Study was conducted at the Human Study Center of the Else-Kröner-Fresenius Center for Nutritional Medicine at the Technical University Munich.",
                                           "A detailed study design is described in",
                                           rep_href(href="https://pubmed.ncbi.nlm.nih.gov/22426117/",label="Krug et al., 2012."))
                        ),
                        rep_showcase_panel(width=12,title = "Study design", id="box_about_study_challenges",
                                           fluidRow(
                                             column(3,
                                                    img(src="img/logo/Fasting_small.png",class="showcase-box_img", style="height:50px;"),
                                                    tags$b("Extended fasting", style="text-align:center;display:block"),br(),
                                                    tags$p("The fasting challenge consisted of a 36-hour period of fasting, during which ten sampling time points were taken.")
                                             ),
                                             column(width=3,
                                                    img(src="img/logo/sld_small.png",class="showcase-box_img", style="height:50px;"),
                                                    tags$b("Standard liquid diet", style="text-align:center;display:block"),br(),
                                                    tags$p("The two standard liquid diets (SLD) on day two and three consisted of a defined fiber-free formula drink (Fresubin® Energy Drink Chocolate, Fresenius Kabi, Bad Homburg, Germany), providing one-third of the daily energy requirement. Within the SLD challenges in block one, participants were sampled at 13 time points. In block two, participants were sampled four times.")
                                             ),
                                             column(width=3,
                                                    img(src="img/logo/ogtt_small.png",class="showcase-box_img", style="height:50px;"),
                                                    tags$b("Oral glucose tolerance test", style="text-align:center;display:block"),br(),
                                                    tags$p("The oral glucose tolerance test (OGTT) on day three consisted of a 300 ml solution with mono- and oligosaccharides, equivalent to 75g of glucose after enzymatic cleavage (Dextro O.G.T., Roche Diagnostics, Mannheim, Germany). Samples were taken at nine different time points within the OGTT.")
                                             ),
                                             column(width=3,
                                                    
                                                    img(src="img/logo/physical_small.png",class="showcase-box_img", style="height:50px;"),
                                                    tags$b("Physical activity test", style="text-align:center;display:block"),br(),
                                                    tags$p("Physical activity tested by a 30 min bicycle ergometer training (PAT).")
                                             )),
                                           rep_ui_vline(),
                                           fluidRow(
                                             column(width=3,
                                                    img(src="img/logo/oltt_small.png",class="showcase-box_img", style="height:50px;"),
                                                    tags$b("Oral lipid tolerance test", style="text-align:center;display:block"),br(),
                                                    tags$p("The oral lipid tolerance test (OLTT) on day 4 combined two parts of the SLD and one part of a fat emulsion containing predefined long-chain triglycerides (Calogen®, Nutricia, Zoetemeer, Netherlands). Here, samples were taken at 11 time points within the 8-hour challenge. All challenge drinks were served at room temperature for ingestion within 5 minutes.")
                                             ),
                                             column(width=3,
                                                    img(src="img/logo/stress_small.png",class="showcase-box_img", style="height:50px;"),
                                                    tags$b("Cold stress", style="text-align:center;display:block"),br(),
                                                    tags$p("Participants were triggered by immersing one hand, up to wrist level for a maximum of 3 min in ice water.")
                                             ),
                                             column(width=3,
                                                    title = "Standardized dinner",
                                                    img(src="img/logo/chicken_small.png",class="showcase-box_img", style="height:50px;"),
                                                    tags$b("Standardized chicken meal", style="text-align:center;display:block"),br(),
                                                    tags$p("Participants were provided with a standardized balanced chicken meal (FRoSTA Tiefkühlkost GmbH, Hamburg, Germany) at 7 pm one day prior to each to both blocks (before fasting and before OGTT).")
                                             )
                                           )
                        ),
                        rep_showcase_panel(width=12,title = "Participants", id="box_about_study_participants",
                                           column(12,
                                                  div(style="width:80%; margin-left:auto;margin-right:auto;text-align:center;",
                                                      tableHTML::tableHTML(about[["baseline_anthropometry"]],
                                                                           caption="Table 1. Anthropometric measures of participants. A total of 15 healthy male participants were recruited. Participants did not take any medication, did not show any metabolic abnormalities and were non-smokers.",
                                                                           rownames=F,border=0, spacing="3px", round=1, collapse="separate_shiny"),
                                                      
                                                  )
                                           )
                        ),
                        rep_showcase_panel(width=12,title = "Sample types", id="box_about_study_samples",
                                           column(12,
                                                  "A total of four different types of samples were collected during the HuMet study. Detailed sampling information is described by ",
                                                  rep_href(href="https://pubmed.ncbi.nlm.nih.gov/22426117/",label="Krug et al., 2012."),br(),
                                                  tags$span("Time points are numbered from 1 to 56, with five extra time points (10.5, 11.5, 27.5, 39.5, 57) added in between the original time points. In total, samples were taken at 61 unique time points.")),
                                           column(3,
                                                  img(src="img/logo/blood_small.png",class="showcase-box_img"),
                                                  div(
                                                    
                                                    tags$b("Plasma", style="text-align:center;display:block"),br(),
                                                    tags$span("Time points: 56"),br(),
                                                    tags$span("Time range: 15 to 120 minutes")
                                                  )
                                           ),
                                           column(3,
                                                  img(src="img/logo/urine_small.png",class="showcase-box_img"),
                                                  div(
                                                    tags$b("Spot urine", style="text-align:center;display:block"),br(),
                                                    tags$span("Time points: 23"),br(),
                                                    tags$span("Time range: 2 to 4 hours")
                                                  )
                                           ),
                                           column(3,
                                                  img(src="img/logo/breath_small.png",class="showcase-box_img"),
                                                  div(
                                                    
                                                    tags$b("Breath air", style="text-align:center;display:block"),br(),
                                                    tags$span("Time points: 56"),br(),
                                                    tags$span("Time range: 15 to 120 minutes")
                                                  )
                                           ),
                                           column(3,
                                                  img(src="img/logo/breath_small.png",class="showcase-box_img"),
                                                  div(
                                                    
                                                    tags$b("Breath condensate", style="text-align:center;display:block"),br(),
                                                    tags$span("Time points: 32"),br(),
                                                    tags$span("Time range: every hour")
                                                  )
                                           )
                        )
              )
              
      ),
      
      tabItem(tabName="about_FAQ",
              fluidRow(
                h1("Frequently asked questions",style="text-align:center")
              ),
              fluidRow(style="margin:15px;",
                       h4("Data sources and tools"),
                       shinydashboardPlus::accordion(
                         shinydashboardPlus::accordionItem(
                           id = 1,color="danger",
                           title = "Which platforms were used to analyze metabolites?",
                           collapsed = TRUE,
                           "add text"
                         ),
                         shinydashboardPlus::accordionItem(
                           id = 2,color="danger",
                           title = "What tools and software packages are used in the HuMet Repository?",
                           collapsed = TRUE,
                           "add text"
                         ))
              ),
              fluidRow(style="margin:15px;",
                       h4("How to use the HuMet repository"),
                       shinydashboardPlus::accordion(
                         shinydashboardPlus::accordionItem(
                           id = 3,color="danger",
                           title = "How can I select single metabolites and view their interactive plots?",
                           collapsed = TRUE,
                           "add text"
                         ),
                         shinydashboardPlus::accordionItem(
                           id = 4,color="danger",
                           title = "How can I load sets of multiple metabolites from KEGG or other databases?",
                           collapsed = TRUE,
                           "add text"
                         ),
                         shinydashboardPlus::accordionItem(
                           id = 5,color="danger",
                           title = "Can I download complete data of the HuMet Repository?",
                           collapsed = TRUE,
                           "add text"
                         ))
              )
      ),
      ### showcase_washout ####
      tabItem(tabName="showcase_washout",
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_header(title="Prior exposures: identify metabolites with washout-like temporal profiles",
                                            question='Objectiv: Identify metabolites that originate from exposure such as foods or drugs.',
                                            img= img(src="img/show/washout/abstract_showcase_washout.png", style="margin-left:auto; margin-right:auto"),
                                            buttons = column(12,style="padding-bottom:3px",
                                                             rep_showcase_button_back(id="washout",label="← Showcase"),
                                                             tags$span("Jump to:"),
                                                             rep_jump_to(label="Workflow", id="box_showcase_washout_workflow"),
                                                             rep_jump_to(label="Background", id="box_showcase_washout_background"),
                                                             rep_jump_to(label="Results", id="box_showcase_washout_results"),
                                                             rep_jump_to(label="References", id="box_showcase_washout_references"),
                                                             rep_jump_to(label="Interpretation", id="box_showcase_washout_interpretation")
                                                             
                                            )
                        )
              ),
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_panel(width=12,title = "Workflow",id="box_showcase_washout_workflow",
                                           column(6,
                                                  div(style="margin-top:10px;",
                                                      tags$b("1. Identify reference metabolite trajectory"),
                                                      tags$p(
                                                        "The first step involves selecting a metabolite characterized by a specific and anticipated trajectory - in our case, a gradual decline over time. We selected ",
                                                        rep_href(href="https://hmdb.ca/metabolites/HMDB0000479",label="3-methylhisidine"),
                                                        "as our reference metabolite, which is a known biomarker of meat intake [",
                                                        rep_href(href="https://pubmed.ncbi.nlm.nih.gov/28794208/",label="Yin et al. 2017, J. Nutr."), "]. This chosen metabolite allows us to find and analyzer metabolic changes associated with dietary habits."
                                                      )
                                                  ),
                                                  highchartOutput(outputId="showcase_washout_hcplot2", height="200px"),
                                                  div(actionButton(inputId="showcase_washout_divertplot2", label= tags$span("View this plot in module", tags$em("Time course"))), style="text-align:right")
                                           ),
                                           column(3,
                                                  div(style="margin-top:10px;",
                                                      tags$b("2. Ranking metabolites with similar patterns"), br(),
                                                      tags$p("To identify the most relevant metabolites, we utilize", 
                                                             rep_href(href="https://hmdb.ca/metabolites/HMDB0000479",label="3-methylhisidine"),
                                                             " as the reference metabolite in our ", tags$em("Selection Module"),". Utilizing the Fréchet distance (function: ",
                                                             rep_href(href="https://rdrr.io/cran/kmlShape/man/distFrechet.html",label="distFrechet"),
                                                             "of the ", rep_href(href="https://rdrr.io/cran/kmlShape/",label="kmlShape")," package) ",
                                                             "we calculated the similarity between the average trajectory of the reference metabolite and all other average metabolite trajectories. This allows for precise ranking based on how closely each metabolite's trajectory compares to the reference."
                                                             )
                                                  ),
                                                  div(
                                                    
                                                    img(src="img/logo/logo_selection.png",class="showcase-box_img", style="max-height:120px;margin-top:40px;")
                                                  )#,
                                                  #tags$p("We use a window approach where we allow the Fréchet algorithm to search for the smallest distance between curves in a defined timeframe. This time frame is defined as followed: Maximum of +/- 30 minutes within all challenges except the extended fasting. Within the extended fasting challenge, we allow for comparison of time-points within a range of +/- 120 minutes. Metabolite curves are compared by subject within the user-defined time range and subsequently the mean is calculated across all subjects.")
                                           ),
                                           column(3,
                                                  div(
                                                    style="margin-top:10px",
                                                    tags$b("3. Metabolite selection and visualization:"),br(),
                                                    "The last step in our Selection of metabolites with a Frechet distance < 0.6 with subsequent visualization of mean metabolite curves in the ", tags$em("Time course module"),"."),
                                                  div(
                                                    img(src="img/logo/logo_browser.png",class="showcase-box_img", style="height:120px;")
                                                  )
                                           )
                        ),
                        rep_showcase_panel(width=12,title = "Background", id="box_showcase_washout_background",
                                           column(12,
                                                  style="margin-bottom:10px",
                                                  "We aim to identify metabolites from exposures like foods or drugs that participants didn't have access to during the first two days of the study blocks. Before each study block, all participants consumed a standardized 'chicken with vegetables' meal, which contained dietary ingredients not present in the liquid meals provided during the 4-day study phase (e.g., meat or vegetables).",
                                                  br(),
                                                  "In the HuMet study, we observed a washout-like temporal pattern in plasma levels of 3-methylhistidine in our participants, showing a consistent decrease after the intake of chicken. These profiles displayed minimal interference with other stimuli during the study phase, meeting the essential criteria for a reliable dietary biomarker."
                                                  ),
                                           column(12,
                  
                                                  style="margin-bottom:10px;",
                                                  div(
                                                    style="text-align:center",
                                                    img(src="img/show/washout/abstract_study_design.png", style="width:800px;max-width:100%"),
                                                  ),
                                                  div(style="font-size:14px;",
                                                      tags$b("Figure 1. Study design with standardized chicken meal."),br(),
                                                      " Before each of the two blocks in the HuMet study, a standardized chicken meal (Frosta) was given. Thus",
                                                      rep_href(href="https://hmdb.ca/metabolites/HMDB0000479",label="3-methylhisidine"),
                                                      "was expected to demonstrate a clear excretion curve. Further intake of",
                                                      rep_href(href="https://hmdb.ca/metabolites/HMDB0000479",label="3-methylhisidine"),
                                                      "could be ruled out, as none of the highly controlled challenge meals contained chicken."
                                                  )
                                           ),
                                           column(4,
                                                  tags$b("What are putative dietary biomarker?"), br(),
                                                  "Dietary biomarkers are metabolites that should exhibit steady decreasing excretion curves after food intake, with minimal interferance by environmental stimuli.", br(),
                                           ),
                                           column(4,
                                                  tags$b("How many markers have been validated?"), br(),
                                                  "Validation of  biomarkers is one of the onging efforts in nutritional metabolomics. Several databases integrate results from various biomarker studies including",
                                                  rep_href(href="https://www.foodb.ca",label="FOODB")," and the ", 
                                                  rep_href(href="http://exposome-explorer.iarc.fr",label="Exposome Explorer"),"."
                                           ),
                                           column(4,
                                                  tags$b("Which metabolite did we use as reference biomarker?"), br(),
                                                  "We used the known chicken meat intake biomarker", 
                                                  rep_href(href="https://hmdb.ca/metabolites/HMDB0000479",label="3-methylhisidine"),
                                                  "to identify further dietary biomarkers by distance ranking."
                                           )
                        ),
                        rep_showcase_panel(width=12, title="Results (plot)",id="box_showcase_washout_results",
                                           column(8,style="font-size:14px;",
                                                  actionButton(inputId="showcase_washout_divertplot1", label= tags$span("Load plot into Module", tags$em("Time course"))),
                                                  highchartOutput(outputId="showcase_washout_hcplot1", height="300px"),
                                                  tags$b("Figure 2. Ten selected metabolites with similar kinetics to"), 
                                                  rep_href(href="https://hmdb.ca/metabolites/HMDB0000479",label="3-methylhisidine", style = "font-weight:bold"),
                                                  tags$b("ranked by Fréchet distance.")
                                           ),
                                           column(4,style="font-size:14px; margin-top:40px;",
                                                  tags$b('Metabolites depicting a "washout-like" trajectory '),br(),
                                                  tags$ul(
                                                    tags$li("A total of 21 metabolites showed a distance lower than 1.5 to the reference metabolite."),
                                                    tags$li("10 xenobiotics, 8 amino acids, 3 cofactors and vitamins, and 1 lipid in blood and urine."),
                                                    tags$li("Many have been declared as or may be putative dietary biomarkers of food intake.")
                                                  )
                                           )
                        ),
                        rep_showcase_panel(width=12, title="Results",
                                           column(12,
                                                  style="font-size:14px;overflow-x:scroll;",
                                                  tags$b("Table.1. Metabolites (Metabolon HD4, Biocrates p150) that depict similar trajectories as the reference metabolite 3-methylhisitidine.(Mean Frechet <= 0.6)"),
                                                  DT::DTOutput("showcase_washout_table", width="100%",height = "auto")
                                           )
                        ),
                        rep_showcase_panel(width=12, title="Interpretation",id="box_showcase_washout_interpretation",
                                           column(6,
                                                  img(src="img/show/washout/washout_interpretation.png", style="max-height:100%;max-width:100%;"),br(),
                                                 tags$b("Figure 3. Mapping metabolites to food sources."),
                                                 tags$p("Out of the 17 metabolites 12 are known biomarkers for various food items that have not been provided to participants during the study blocks, indicating exposures prior to the study. ")
                                           ),
                                           column(6,
                                                  "Our research identified S-allylcysteine in the plasma as the metabolite showing the highest correlation in temporal pattern with plasma 3-methylhistidine, 
                                                  exhibiting a Fréchet distance of 0.2712. Furthermore, 34 other metabolites displayed a distance less than 0.6, among which 16 were plasma metabolites. 
                                                  Of these, 12 are listed in the Food Database or associated with dietary exposures in the Exposome Explorer. 
                                                  These metabolites hint at exposure to foods such as meat, garlic, bread, coffee, milk, and soy, and were present in nearly all subjects at most collection points.",
                                                  br(),
                                                  "An exception was equol glucuronide, a derivative of the isoflavone daidzein found primarily in soy. 
                                                  This compound was identified only in two participants. The metabolization of daidzein into equol, which can subsequently be sulfated and glucuronated, is variable among the human population, with about half capable of this conversion. 
                                                  This metabolic capacity seems to depend on the individual's gut microbiome and is possibly associated with the health benefits of soy isoflavones. While at least two subjects from our study demonstrated this ability, 
                                                  equol glucuronide was consistently detected in only one, suggesting that this individual had consumed soy or other daidzein-containing food prior to each study phase."
                                                  )
                        ),
                        rep_showcase_panel(width=12, title="References", id="box_showcase_washout_references",
                                           tags$ul(
                                             tags$li(rep_href(href="https://pubmed.ncbi.nlm.nih.gov/11052553/", label="Myint et al., 2000, Am J Epidemiol")),
                                             tags$li(rep_href(href="https://pubmed.ncbi.nlm.nih.gov/17022649/", label="Stella et al., 2006, J Proteome Res")),
                                             tags$li(rep_href(href="https://pubmed.ncbi.nlm.nih.gov/21527577/", label="Cross et al., 2011, Cancer Empidemiol Biomarkers Prev")),
                                             tags$li( rep_href(href="https://pubmed.ncbi.nlm.nih.gov/24740205/", label="Guertin et al., 2014, Am J Clin Nutr"))
                                           )
                        )
              )
              
      ),
      ### showcase_networks_cross ----------------------------------------------------
      tabItem(tabName="showcase_networks_cross",
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_header(title="Systemic metabolic responses: Reveal & compare responses to challenges (example: comparison of dietary challenges)",
                                            question='"How do metabolic responses in two selected pathways compare between three different nutritional challenges?"',
                                            img=img(src="img/show/network/outcome_showcase_network2.png", style="height:100%"),
                                            button=  column(12,style="padding-bottom:3px",
                                                            rep_showcase_button_back(id="networks_cross",label="← Showcase"),
                                                            tags$span("Jump to:"),
                                                            rep_jump_to(label="Background", id="box_showcase_networks_cross_background"),
                                                            rep_jump_to(label="Results", id="box_showcase_networks_cross_results"),
                                                            rep_jump_to(label="Method", id="box_showcase_networks_cross_method"),
                                                            rep_jump_to(label="References", id="box_showcase_networks_cross_references")
                                            )
                        )
              ),
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_panel(width=12, id="box_showcase_networks_cross_background",
                                           title = "Background",
                                           column(4,
                                                  tags$b("Objective"),br(),
                                                  tags$span("One of the most well-studied challenge is the oral glucose tolerance test (OGTT), where we commonly evaluate glucose disposal, ß-cell function and insulin sensitivity for diagnosis. In the OGTT, we focus on the temporal response of glucose, insulin and insulin regulated pathways. However, recent studies have suggested an important role of bile acids in glucose regulation and energy homeostasis. Using our holistic temporal network approach, we can investigate the regulation of bile acids across all dietary challenges."
                                                  )
                                           ),
                                           column(4,
                                                  tags$b("Network reconstruction"),br(),
                                                  tags$span("Gaussian graphical models (GGMs) use significant partial correlation to establish connections between metabolites, eliminating indirect correlations. These connections form a data-driven network from metabolomics data, effectively mirroring established metabolic pathways."
                                                            )
    
                                           ),
                                           column(4,
                                                  tags$b("Our network"),br(),
                                                  "Here, we use the single fluid GGM (Metabolon HD4 Plasma, Biocrates p150 plasma and in-house biochemistry, Pcor >= 0.12). We mapped metabolic changes of the OGTT (60 min vs. baseline), SLD (60 min vs. baseline) and OLTT (60min vs baseline) and focused on two selected pathways."
                                                  
                                                  )
                        ),
                        rep_showcase_panel(width=12, id="box_showcase_networks_cross_results",
                                           title = "Results",
                                           column(8,
                                                  actionButton(inputId="showcase_networks_cross_load_plot", label= tags$span("View this plot in the module", tags$em("Networks"))),
                                                  img(src="img/show/network/outcome_showcase_network2.png",class="showcase-box_img", style="max-height:1200px;"),
                                                  tags$b("Figure 1. Comparison of metabolic responses to nutritional challenges within reconstructed metabolic networks."),br(),
                                                  tags$p("For comparison of metabolite changes within particular pathways (here: bile acids and amino acids) 60 min after ingestion of the challenge drink, we select the single fluid network generated based on plasma levels of the non-targeted (Metabolon HD4) and targeted (Biocrates p150) platform. To visualize responses, we map the log2 fold changes and p-values resulting from t-tests comparing metabolite levels after the respective challenge with the corresponding baseline levels (module Statistics). Color displays the log2 fold change between challenge baseline and chosen time point with red indicating an increase in metabolite concentration and blue indicating a decrease in metabolite concentration. Node size depicts the -log10 p-value of metabolic changes between challenge baseline and chosen time point.")
                                           ),
                                           column(4,
                                                  tags$b("Networks provide an overview of changes upon nutritional changes"),br(),
                                                  tags$p("60 min after ingestion of challenge drink versus baseline in OGTT, SLD, and OLTT"),
                                                  tags$ul(
                                                    tags$li("Coloring the network based on plasma metabolites by changes 60 min after ingestion of the challenge drinks (right) revealed a cluster of bile acids that similarly increased in SLD and OGTT and to a lesser extent also in response to OGTT. "),
                                                    tags$li("In contrast, a cluster containing most amino acids, showed decreases in OGTT and increases in SLD and OLTT, with less effect observed in the latter case (though 65% of the same protein mix was ingested with the OLTT drink as with the SLD drink). ")
                                                  ) 
                                           )
                        ),
                        rep_showcase_panel(width=12,title = "Method", id="box_showcase_networks_cross_method",
                                           column(4, style="font-size:14px;",
                                                  h4("1. Network generation", style="text-align:center"),
                                                  tags$span("Network inference of single fluid networks is based on estimating GGMs from the metabolite concentrations. 
                                           These models are based on partial correlations and have previously demonstrated to reconstruct biological pathways from metabolomics data.
                                           Our single fluid networks are based on the imputed and log2 transformed data. Network inference closely followed the procedures reported in Do et al., with a few changes due to the longitudinal nature of the dataset.")
                                           ),
                                           column(4, style="font-size:14px;",
                                                  h4("2. Statistical results", style="text-align:center"), 
                                                  tags$span(
                                                    "The bubbles within the network represent metabolites. Here, we added bubble color and size. Color displays the log2 foldchange between challenge baseline and chosen time point with red indicating an increase in metabolite concentration and blue indicating a decrease in metabolite concentration. Size depicts the -log10 p-value of metabolic changes between challenge baseline and chosen time point. Thereby, node size increases with lower p-value. These results can be displayed in the module Statistics"
                                                  ),
                                                  div(style="text-align:center",
                                                      img(src="img/logo/logo_statistics.png",class="showcase-box_img", style="max-height:120px;")
                                                      )
                                                
                                                  ),
                                           column(4, style="font-size:14px;",
                                                  h4("3. Cluster selection", style="text-align:center"), 
                                                  "Interesing clusters of metabolites were selected manually.",
                                                  div(style="text-align:center",
                                                      img(src="img/logo/logo_networks.png",class="showcase-box_img", style="max-height:120px;"))
                                           )
                        ),
                        rep_showcase_panel(width=12,title = "References", id="box_showcase_networks_cross_references",
                                           tags$ul(
                                             tags$li(rep_href(href="https://www.ine.pt/revstat/pdf/rs060103.pdf",label="Opgenrhein & Strimmer 2006")),
                                             tags$li(rep_href(href="https://bmcsystbiol.biomedcentral.com/articles/10.1186/1752-0509-5-21",label="Krumsiek et al. 2011")),
                                             tags$li(rep_href(href="https://pubs.acs.org/doi/abs/10.1021/pr501130a",label = "Do et al., 2015"))
                                           )
                        )
              )
              
      ),
      ### showcase_networks_longi ----------------------------------------------------
      tabItem(tabName="showcase_networks_longi",
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_header(title="Systemic metabolic responses: Reveal & compare responses to challenges (example extended fasting)",
                                            question='"Which areas of metabolism change after extended fasting (36 h) compared to standardized overnight fasting in the reconstructed metabolic network?"',
                                            img=img(src="img/show/network/abstract_showcase_network1.png"),
                                            buttons=column(12,style="padding-bottom:3px",
                                                           rep_showcase_button_back(id="networks_longi",label="← Showcase"),
                                                           tags$span("Jump to:"),
                                                           rep_jump_to(label="Background", id="box_showcase_networks_longi_background"),
                                                           rep_jump_to(label="Results", id="box_showcase_networks_longi_results"),
                                                           rep_jump_to(label="Method", id="box_showcase_networks_longi_method"),
                                                           rep_jump_to(label="References", id="box_showcase_networks_longi_references"),
                                                           rep_jump_to(label="Implementation", id="box_showcase_networks_longi_implementation")
                                            ))
              ),
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_panel(width=12,title = "Background", id="box_showcase_networks_longi_background",
                                           column(4,
                                                  tags$b("Objective"),br(),
                                                  tags$span("Extended fasting is of special interest as it depicts part of the catabolic state which the body undertakes several times per day. Here, we can find known and novel patterns within the dynamic adaption."
                                                  )
                                                  
                                           ),
                                           column(4,
                                                  tags$b("Network reconstruction"),br(),
                                                  tags$span("Gaussian graphical models (GGMs) use significant partial correlation to establish connections between metabolites, eliminating indirect correlations. These connections form a data-driven network from metabolomics data, effectively mirroring established metabolic pathways."
                                                  )
                                                  
                                           ),
                                           column(4,
                                                  tags$b("Our network"),br(),
                                                  tags$span("We use the multi-fluid network (Metabolon HD4 plasma pcor >= 0.12, Metabolon HD4 urine pcor >= 0.09). On this backbone, we mapped statistical results comparing extended fasting (36h postprandial, TP 10) versus standardized overnight fasting (12h postprandial, TP 1).")
                                           )
                        ),
                        rep_showcase_panel(width=12,title = "Results",id="box_showcase_networks_longi_results",
                                           column(8, style="font-size:12px;",
                                                  actionButton(inputId="showcase_networks_longi_load_plot", label= tags$span("View this plot in module", tags$em("Networks"))),
                                                  img(src="img/show/network/outcome_showcase_network1.png",class="showcase-box_img", style="max-height:500px;max-width:100%"),
                                                  tags$b("Fig. 1. Temporal multi fluid networks based on the plasma and urine Metabolon HD4 platform. "), br(),
                                                  tags$span("Composite network derived from two single-fluid Gaussian graphical models (GGMs) including (i) plasma Metabolon HD4 GGM (dynamic partial correlation>=0.12), and (ii) a urine Metabolon HD4 GGM (dynamic partial correlation <= 0.08). The network compares metabolic alterations within biological pathways between fasting at baseline (overnight, 12h fasting) and extended fasting of 36 hours. "),
                                                  
                                                  tags$span("The color gradient represents the log2 fold change in metabolite concentrations between the baseline and the selected time point, with red indicating an increase and blue indicating a decrease. The size of each node is proportional to -log10(p-value). As such, smaller p-values, indicating more significant changes, are represented by larger node sizes. Shapes indicate the platform on which the metabolite was measured with circles representing plasma and squares representing urine Metabolon HD4. This visual representation enables the understanding of the metabolic dynamics influenced by prolonged fasting.")
                                                  
                                           ),
                                           column(4,
                                                  tags$b("Inferrend networks reconstruct biological pathways"),br(),
                                                  tags$ul(
                                                    tags$li("Network view provides a holistic overview of typical metabolic responses upon perturbation"),
                                                    tags$li("Multiple biological processes are involved in adaption to 36h extreme fasting ")
                                                  ),
                                                  tags$b("Multiple biological pathways affected by extended fasting"),br(),
                                                  tags$ul(
                                                    tags$li("(A) Upregulated bile acid metabolism"),
                                                    tags$li("(B) downregulated benzoate metabolism"),
                                                    tags$li("(C) downregulated xanthine metabolism"),
                                                    tags$li("(D) upregulated ketone bodies"),
                                                    tags$li("(F) upregulated levels of free fatty acids")
                                                  )
                                           )
                        ),
                        rep_showcase_panel(width=12,title = "Method", id="box_showcase_networks_longi_method",
                                           column(4, style="font-size:14px;",
                                                  h4("1. Network reconstruction", style="text-align:center"),
                                                  tags$span("Network inference of single fluid networks is based on estimating GGMs from the metabolite concentrations. 
                                             These models are based on partial correlations and have previously demonstrated to reconstruct biological pathways from metabolomics data.
                                             Our single fluid networks are based on the imputed and log2 transformed data. Network inference closely followed the procedures reported in (Do et al. 2015), 
                                             termed “fused networks,” with a few changes due to the longitudinal nature of the dataset.")
                                           ),
                                           column(4, style="font-size:14px;",
                                                  h4("2. Statistical results", style="text-align:center"), 
                                                  tags$span(
                                                    "The bubbles within the network represent metabolites. Here, we added bubble color and size. Color displays the log2 foldchange between challenge baseline and chosen time point with red indicating an increase in metabolite concentration and blue indicating a decrease in metabolite concentration. Size depicts the -log10 p-value of metabolic changes between challenge baseline and chosen time point. Thereby, node size increases with lower p-value."
                                                  )),
                                           column(4, style="font-size:14px;",
                                                  h4("3. Cluster selection", style="text-align:center"), 
                                                  "Interesing clusters of metabolites were selected manually. Each of cluster represents a unique biological pathway that is part of the metabolic responses to extended fasting."
                                           )
                        ),
                        rep_showcase_panel(width=6,title = "References", id="box_showcase_networks_longi_references",
                                           tags$ul(
                                             tags$b("Gaussian Graphical Model inferrence"),
                                             tags$li(rep_href(href="https://www.ine.pt/revstat/pdf/rs060103.pdf",label="Opgenrhein & Strimmer 2006")),
                                             tags$li(rep_href(href="https://bmcsystbiol.biomedcentral.com/articles/10.1186/1752-0509-5-21",label="Krumsiek et al. 2011")),
                                             tags$li(rep_href(href="https://pubs.acs.org/doi/abs/10.1021/pr501130a",label = "Do et al., 2015"))
                                           )
                        ),
                        rep_showcase_panel(width=6,title = "Repository implementation", id="box_showcase_networks_longi_implementation",
                                           column(6,
                                                  img(src="img/logo/logo_statistics.png",class="showcase-box_img", style="max-height:120px;"),
                                                  "Individualized results of the pairwise t tests can be viewed in the", tags$em("Statistics Module."),"T test results are used within the time-resolved networks for animation."),
                                           column(6,
                                                  img(src="img/logo/logo_networks.png",class="showcase-box_img", style="max-height:120px;"),
                                                  "Generated networks can be viewed within our", tags$em("Networks Module."))
                        )
              )
              
      ),
      ### showcase_platforms####
      tabItem(tabName="showcase_platforms",
              fluidRow( style="margin:0;background:#ddd;",
                        
                        rep_showcase_header(title="Platform comparison: Compare metabolites across platforms",
                                            question='Objective: Comparison of metabolite curves across a targeted and non-targeted platform',
                                            img=img(src="img/logo/logo_showcase_platform.png", style="max-height:200px"),
                                            buttons=column(12,style="padding-bottom:3px",
                                                           rep_showcase_button_back(id="platforms",label="← Showcase"),
                                                           tags$span("Jump to:", style="color:white"),
                            
                                                           rep_jump_to(label="Background", id="box_showcase_platforms_background"),
                                                           rep_jump_to(label="Workflow", id="box_showcase_platforms_method"),
                                                           rep_jump_to(label="Results", id="box_showcase_platforms_results"),
                                                           rep_jump_to(label="References", id="box_showcase_platforms_references")
                                            )
                        )
              ),
              fluidRow( style="margin:0;background:#ddd;",
                        rep_showcase_panel(width=12,title = "Background",id="box_showcase_platforms_background",
                                           column(6,
                                                  tags$b("Why compare overlapping metabolite signals across profiling platforms?"),br(),
                                                  tags$p(
                                                    style="margin-left:10px",
                                                    "There are multiple solutions to measuring and identifying metabolites. Therefore, metabolites reported across different studies are measured on different profiling platforms. It is thereby essential to compare a unique metabolite that is measured on different platforms, to estimate the precision of each platform.")
                                           ),
                                           column(6,
                                                  tags$b("Which overlapping signals were measured in the HuMet study?"),br(),
                                                  tags$p("A total of 38 metabolite pairs were available from non-targeted LCMS (Metabolon HD4) and targeted LCMS (Biocrates p150). A list of metabolites were previosuly matched by ",
                                                         rep_href(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4830611/",label="Yet et al., 2016"),
                                                         "comprising 43 metabolites that were measured both using the Biocrates p150 kit and using an older version of the Metabolon HD4 platform in a british cohort.",
                                                         "Overall, we observed a high correlation of measurements for the investigated metabolite pairs across the two platforms in HuMet, with a median correlation of 0.75."
                                                  )
                                           )
                                           
                        ),
                        rep_showcase_panel(width=12,title = "Workflow",id="box_showcase_platforms_method",
                                           column(6, 
                                                  tags$b("1. Calculation of similarity", style="text-align:center"),
                                                  tags$p("The curated, imputated and z-scored Metabolon HD4 plasma and Biocrates p150 plasma dataset is used to calculate the paired Pearson correlation between two metabolite curves within one subject and the resulting distance matrices are summed up across all subjects."),
                                                  img(src="img/logo/logo_selection.png",class="showcase-box_img", style="max-height:120px;")
                                           ),
                                           column(4, 
                                                  tags$b("2. Visualization of similarities", style="text-align:center"),
                                                  tags$p("Metabolite pairs can be visually inspected in the ", tags$em("Module Time course"),  " by grouping in the metabolite bags (header)."),
                                                  img(src="img/logo/logo_timecourse.png",class="showcase-box_img", style="max-height:120px;")
                                           )
                        ),
                        rep_showcase_panel(width=12,title = "Results",id="box_showcase_platforms_results",
                                           column(6,
                                                  img(src="img/show/platform/outcome_showcase_platform3.png",class="showcase-box_img", style="max-height:800px;"),
                                                  tags$b("Figure 1. Comparison of 38 overlapping metabolites measured on both non-targeted (Metabolon HD4) and targeted (Biocrates p150) platform matched by expert knowledge."),
                                                  tags$p("Pearson correlations were calculated on the individuals metabolite trajectories and subsequently averaged across all subjects to assess the synchronicity. Metabolites were ranked according to the super-pathway and Pearson correlation.")
                                           ),
                                           column(6,
                                                  img(src="img/show/platform/showcase_platform_correlation.png",class="showcase-box_img", style="max-height:300px;"),
                                                  tags$b("Figure 2. Concordance of measurements from the two platforms."),
                                                  tags$p("High concordance with a median correlation of measurements of r = 0.75 and only four out of 38 pairs with correlations below 0.5."),
                                                  tags$br(),
                                                  tags$b("Findings"),tags$br(),
                                                  tags$ul(
                                                    tags$li("Strong correlations between most overlapping metabolites sums (Biocrates p150 [t-ms]) and single metabolite signals (Metabolon HD4 [nt-ms])"),
                                                    tags$li("Also, glucose (non-targeted) and H1 (hexose, targeted) measurements were highly correlated (r = 0.87), which is in line with measured plasma hexose consisting mostly of glucose in humans.")
                                                    ),
                                                  tags$b("Only four pairs showed comparably weak correlations (r < 0.5)"),tags$br(),
                                                  tags$ul(
                                                      tags$li("butyrylcarnitine (non-targeted)/C4 (butyrylcarnitine) (targeted) (r = 0.18) and glutarylcarnitine (nt-ms)/C6-OH (C5-DC) (t-ms) (r = 0.18) showed differences between the platforms. In the first case, the reason for this difference could be that the Biocrates p150 measure labeled as C4 (butyrylcarnitine) includes the isobaric isobutyrylcarnitine, while these two metabolites are measured as two separate analytes on the Metabolon HD4 platform."),
                                                      tags$li("Correlation analysis of metabolite isobutyrylcarnitine (nt-ms)/C4 (butyrylcarnitine) (t-ms) (r = 0.82) indicates that C4 (butyrylcarnitine) (t-ms) and/or its dynamic changes might indeed be dominated by isobutyrylcarnitine. This is of particular interest as butyrylcarnitine and isobutyrylcarnitine derive from two fundamentally different pathways linked to the degradation of fatty acids and to the degradation of branched-chain amino acid, respectively."),
                                                      tags$li("A similar scenario can be assumed to underly the low correlations between glutarylcarnitine measured on Metabolon HD4 and the analyte labeled as C6-OH (C5-DC) measuring glutarylcarnitine and hydroxyhexanoylcarnitine together using the Biocrates p150 kit.")
                                                    )
                                                  

                                           )
                        ),
                        rep_showcase_panel(width=12,title = "Results",
                                           column(12,
                                                  actionButton(inputId="showcase_platform_divertplot2", label= tags$span("View this plot in module", tags$em("Time course"))),
                                                  highchartOutput(outputId = "showcase_platform_plot1", height = "300px"),
                                                  tags$b("Fig. 2. Depiction of Laurylcarnitine(targeted) / C12(Laurylcarnitine, non-targeted)."),
                                                  tags$span("Laurylcarnitine/C12 (Laurylcarnitine) depict the strongest correlation (r = 0.95).")
                                           ),
                                           column(12,
                                                  actionButton(inputId="showcase_platform_divertplot1", label= tags$span("View this plot in module", tags$em("Time course"))),
                                                  highchartOutput(outputId = "showcase_platform_plot2", height = "300px"),
                                                  tags$b("Fig. 3. Depiction of Butyrylcarnitine(targeted)/C4 (butyrylcarnitine, non-targeted)."),
                                                  tags$span("Butyrylcarnitine/C4 (butyrylcarnitine) show the weakest correlation (r = 0.18) and the isoform isobutyrylcarnitine/C4 (butyrylcarnitine) demonstrating a high correlation (r = 0.82). Therefore, repository exploration reveals high contribution of isobutyrylcarnitine [nt-ms] to C4 (butyrylcarnitine) sum [t-ms].")
                                           )
                        ),
                        rep_showcase_panel(width=6,title = "Outlook",id="box_showcase_platforms_outlook",
                                           column(12,
                                                  " Having time-series metabolomics data from the same subjects across multiple platforms, coupled with the repository's exploration tools like the similarity search (Selection module) and time-course plots (Time Course module), enables a comprehensive comparison, illuminating how different platform measurements can complement each other.")
                                           ),
                        rep_showcase_panel(width=6,title = "References",id="box_showcase_platforms_refereces",
                                           tags$ul(
                                             tags$li(
                                               rep_href(href="https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4830611/",label="Yet et al., 2016"))
                                           ))
              )
              
      ),
      
      #showcase_krug ####
      tabItem(tabName="showcase_krug",
              fluidRow(
                h2("Paper figures",style="text-align:center"),
                h6("currently under development",style="text-align:center")
              )
      ),
      
      
      
      
      #showcase_fasting ####
      
      tabItem(tabName="showcase_fasting",
              fluidRow(
                column(12,
                       div(style="display:-webkit-box",
                           div(),
                           div(
                             style="width:40%;padding:20px;",
                             h2(tags$b("Fasting vs. Non-Fasting"), style="color:#284672"),
                             hr(),
                             tags$em("Explore metabolites that are informative of the fasting status, which were identified via an iterative random forest model using backward variable selection.")
                           )))
              ),
              fluidRow(
                h3("this site is currently under developement")
              )
      ),
      # showcase kinetic patterns ####
      
      
      tabItem(tabName="showcase_kinetic",
              fluidRow( # Title 
                column(8,style="max-height:300px;overflow:hidden"
                       #img(src ="img/show/showcase_network_header.png", style="width:100%")
                ),
                column(4,style="text-align:center",
                       h2("Kinetic Patterns", style="color:#284672"))
                
              ),
              fluidRow( # first example
                style="background-color:ghostwhite;margin-top:10px;padding:5px;",
                column(4,
                       tags$div(style="display:flex",
                                h4("Early peaks in metabolic levels", style="color:#284672")
                       ),
                       tags$div(uiOutput("kin_early_UI"), style="border:1px solid black;padding:1px;background-color:white;text-align:center")
                ),
                column(8,
                       tags$div(
                         tags$div(tags$b("Find pattern in challenge:")),
                         radioGroupButtons(inputId = "kin_early_chal", status = "metVradio", selected="OGTT",
                                           choices=list("Fasting"="FASTING", "SLD1"="SLD1", "OGTT"="OGTT", "SLD2"="SLD2", "PAT"="PAT", "OLTT"="OLTT", "Stress"="STRESS"))
                         
                       ),
                       tags$div(tags$b("Table.1: Ranking of metabolites with early peaks by euclidean distance")),
                       tags$div(style="border:1px solid black;padding:5px", DT::DTOutput("kin_early_table"))
                )
              ),
              fluidRow( # second example
                style="margin-top:10px;padding:5px;",
                column(4,
                       tags$div(style="display:flex",
                                h4("Washout metabolites", style="color:#284672")
                       ),
                       tags$div(uiOutput("kin_washout_UI"), style="border:1px solid black;padding:1px;background-color:white;text-align:center")
                ),
                column(8,
                       tags$div(
                         tags$div(tags$b("Find pattern in challenge:")),
                         radioGroupButtons(inputId = "kin_washout_chal", status = "metVradio", selected="OGTT",
                                           choices=list("Fasting"="FASTING", "SLD1"="SLD1", "OGTT"="OGTT", "SLD2"="SLD2", "PAT"="PAT", "OLTT"="OLTT", "Stress"="STRESS"))
                         
                       ),
                       tags$div(tags$b("Table.1: Ranking of metabolites with early peaks by euclidean distance")),
                       tags$div(style="border:1px solid black;padding:5px", DT::DTOutput("kin_washout_table"))
                )
              ), #second end
              fluidRow( # third example
                style="margin-top:10px;padding:5px;",
                column(4,
                       tags$div(style="display:flex",
                                h4("Late peaks in metabolic levels", style="color:#284672")
                       ),
                       tags$div(uiOutput("kin_late_UI"), style="border:1px solid black;padding:1px;background-color:white;text-align:center")
                ),
                column(8,
                       tags$div(
                         tags$div(tags$b("Find pattern in challenge:")),
                         radioGroupButtons(inputId = "kin_late_chal", status = "metVradio", selected="OGTT",
                                           choices=list("Fasting"="FASTING", "SLD1"="SLD1", "OGTT"="OGTT", "SLD2"="SLD2", "PAT"="PAT", "OLTT"="OLTT", "Stress"="STRESS"))
                         
                       ),
                       tags$div(tags$b("Table.1: Ranking of metabolites with early peaks by euclidean distance")),
                       tags$div(style="border:1px solid black;padding:5px", DT::DTOutput("kin_late_table"))
                )
              ) #third end
              
      )
      
    )
  )
)

body$attribs$style <- paste0(body$attribs$style,";font-family:Arial,Helvetica,sans-serif;")
