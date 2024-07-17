
sidebar<-shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    id="menu",
    br(),
    shinydashboard::menuItem("Home", icon=rep_ui_icon("home"), tabName = "home"),
    br(),
    rep_ui_sidebar_dropdown_menu_hidden(tabName="showcase",
                                        shinydashboard::menuItem("Showcases",icon=rep_ui_icon("tv"), tabName = "showcase",startExpanded=F,
                                                                 shinydashboard::menuSubItem("Prior exposure", tabName = "showcase_washout"),
                                                                 shinydashboard::menuSubItem("Systemic response across challenge", tabName = "showcase_networks_cross"),
                                                                 shinydashboard::menuSubItem("Systemic response longitudinal", tabName = "showcase_networks_longi"),
                                                                 shinydashboard::menuSubItem("Platform comparison", tabName = "showcase_platforms")
                                        )
    ),
    shinydashboard::menuItem("Tutorial", icon=icon("question",lib = "font-awesome"), tabName = "tutorial"),
    br(), 
    shinydashboard::menuItem("Selection", icon=icon("magnifying-glass",lib = "font-awesome"), tabName = "metabselect"),
    shinydashboard::menuItem("Time course",
                             icon=icon("chart-line",lib = "font-awesome"),#rep_ui_icon("chart-area"), 
                             tabName = "metabdetail"),
    shinydashboard::menuItem("Statistics", icon=icon("chart-column",lib = "font-awesome"),#icon=rep_ui_icon("braille"), 
                             tabName = "stats_ttest"),
    shinydashboard::menuItem("Networks",icon=icon("diagram-project",lib = "font-awesome"), #icon=rep_ui_icon("share-alt"), 
                             tabName = "network"),
    # rep_ui_sidebar_dropdown_menu(tabName = "statistics",
    #                              shinydashboard::menuItem("Statistics",icon=rep_ui_icon("braille"), tabName = "statistics",startExpanded=F,
    #                                                       shinydashboard::menuSubItem("t Test", tabName = "stats_ttest")#,
    #                                                       #shinydashboard::menuSubItem("PCA", tabName = "stats_pca")
    #                              )
    # ),
    br(),
    shinydashboard::menuItem("About", icon=rep_ui_icon("book"),
                             shinydashboard::menuSubItem("Study design", tabName = "about_study"),
                             shinydashboard::menuSubItem("Metabolite profling", tabName = "about_profiling"),
                             shinydashboard::menuSubItem("Data preprocessing", tabName = "about_processing"),
                             shinydashboard::menuSubItem("Statistical analysis", tabName = "about_analysis"),
                             shinydashboard::menuSubItem("Development team", tabName = "about_team"),
                             shinydashboard::menuSubItem("Repository", tabName = "about_repository")
    ),
    shinydashboard::menuItem("Download", icon=rep_ui_icon("download"),tabName="download"),
    
    div(style="display:flex;flex:1;flex-direction:column; margin:5px 5px 10px 10px; position:absolute; bottom:0;",
        shinydashboard::menuItem(tags$span("Imprint",style="color:white"),href="https://www.helmholtz-muenchen.de/en/legal-notice/index.html"),
        shinydashboard::menuItem(tags$span("Data Protection Statement",style="color:white"),href="https://www.helmholtz-muenchen.de/en/privacy-policy/index.html")
    )
  )
) %>%
  rep_ui_sidebar(class="mtd_text", style=";background-color:#2c323a; color:white")
