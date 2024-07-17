# Functions to generate ui elements

# General ---------------
rep_href<-function(href,label, color="#d2310b", style=NULL){
  out=tags$a(href=href,target="_blank",tags$span(label), style=paste0("color:",color,";", style))
  return(out)
}

# Sidebar ----------------------------------------------------------------------
rep_ui_sidebar<-function(.sidebar,class=NULL, style=NULL){
  #add mtd_design to sidebar
  .sidebar[["attribs"]]$class<-paste("main-sidebar",class)
  .sidebar[["attribs"]]$style<-paste(.sidebar[["attribs"]]$style, style)
  return(.sidebar)
}

rep_ui_sidebar_dropdown_menu <- function(list,tabName) {
  list$children[[1]]$attribs['data-toggle']="tab"
  list$children[[1]]$attribs['data-value'] = tabName
  if(length(list)>0 && list$attribs$class=="treeview"){
    list$attribs$class=NULL
  }
  list
}

rep_ui_sidebar_dropdown_menu_hidden <- function(list,tabName) {
  list$children[[1]]$attribs['data-toggle']="tab"
  list$children[[2]]$attribs$style="visibility:hidden;height:0px;"
  list$children[[1]]$attribs['data-value'] = tabName
  list$children[[1]]$children[[3]]=NULL
  if(length(list)>0 && list$attribs$class=="treeview"){
    list$attribs$class=NULL
  }
  list
}

# Header -----


# Home ---
rep_header_stats <- function(id,title, subtitle=NULL, color="white",background="#2c323a", width="150px", font_size="12px"){
  backgroundTag=NULL
  if(!is.null(background)){
    backgroundTag=paste0(";background:",background)
  }
  

  actionButton(inputId = id, label=div(div(title), div(subtitle)),class="rep_header_stats",style=paste0(backgroundTag, ";width:", validateCssUnit(width), ";font-size:",font_size))
  
}
rep_home_stats <- function(id,title, subtitle=NULL, color="white",background="#2c323a", width="150px", font_size="12px"){
  backgroundTag=NULL
  if(!is.null(background)){
    backgroundTag=paste0(";background:",background)
  }
  
  div(
    style=paste0(backgroundTag, ";width:", validateCssUnit(width), ";font-size:",font_size, "; color:",color, ";display:inline-block; border-radius:5px; padding:5px; margin:5px"),
    div(title, style="display:block"),
    div(subtitle, style="display:block")
  )
}

# Misc -----
## add a vertical line
rep_ui_vline <- function(style=NULL){
  styleTag=NULL
  if(!is.null(style)) styleTag=style
  # div(style=paste0("width:100%;display:block;",styleTag),
  #     div(style="border-bottom:3px solid lightgrey; margin-right:30px; margin-left:30px")) ##2c323a
  div()
}

## icon 
rep_ui_icon <- function(name, style=NULL){
  suppressWarnings({icon(name=name)})
}

rep_sidebar_icon <- function(url){
  out <- paste0("<i>",tags$img(url=url),"</i>")
  out <- tags$img(url=url)
  return(out)
}


# showcase button
rep_showcase_button<-function(id, width=4, title=NULL, subtitle=NULL, content=NULL, img_src=NULL, title_color=NULL,title_size="18px",subtitle_color="grey",subtitle_size="12px",border_radius=NULL,background="white"){
  titleTag=NULL
  if(!is.null(title)){
    if(is.null(title_color) & "link_highlight" %in% names(options$general$colors)) title_color=options$general$colors$link_highlight
    titleTag=div(tags$span(title),
                 style=paste0("width:100%;color:",title_color,";","font-size:",title_size,";font-weight:600; border-top:2px solid #ddd;")
    )
  }
  subTag=NULL
  if(!is.null(subtitle)){
    subTag=div(tags$p(subtitle),
               style=paste0("width:100%;color:",subtitle_color,";","font-size:",subtitle_size)
    )
  }
  
  
  contentTag=NULL
  if(!is.null(content)){
    contentTag=content
  }
  
  content_imgTag=NULL  
  if(!is.null(img_src)){
    content_imgTag=img(src=img_src,
                       style = "max-height:70%; max-width:100%;margin-left:auto;margin-right:auto;display:block;")
    
  }
  
  
  out=actionButton(inputId = id,
                   label=div(class="rep_showcase_button-body",
                     style=paste0("background:",background),
                     #div(style=paste0("font-size:12px;text-align:center;width:100%; max-height:70%;min-height:fit-content"),contentTag,
                         content_imgTag,#),
                     div(style=paste0("min-height:20%;overflow:hidden;padding:5px;"),
                       titleTag,
                         subTag))
  )
  out$attribs$class="rep_showcase_button btn btn-default action-button"
  column(width,out, style=paste0("display:block;float:left;", "height:300px;"))
}

rep_showcase_button_back <- function(id, label){
  actionButton(inputId = paste0("showcase_divert_",id), label=label, class="rep_button")
}

rep_jump_to <- function(id, label){
  a(
    label,
    type = "button",
    class = "rep_button",
    href = paste0("#",id)
  )
}
rep_showcase_header <- function(title, question, img=NULL, img_text=NULL, buttons=NULL){
  img_tag=NULL
  if(!is.null(img)){
    img_tag=#div(style="float:left;width:max-content;max-height:150px;max-width:400px; overflow:hidden; margin:20px;",
            column(3,
                   style="height:150px; width:max-content; overflow:hidden;border-radius:5px;",
                
                   class="rep_showcase_header-img",
                   img,
                   tags$span(img_text))
  }
  column(12,class="rep_showcase_header-container",
      column(width=ifelse(is.null(img),12,8),style="margin-top:10px;",
             tags$span(title,
                       class="rep_showcase_header-title"),
             tags$span(question,
                       class="rep_showcase_header-question"),
             buttons
             ),
      img_tag
  )
}

rep_showcase_panel <- function(..., title = NULL, id=NULL,footer = NULL, status = NULL, solidHeader = TRUE, 
                            background = NULL, width = 6, height = NULL, collapsible = FALSE, 
                            collapsed = FALSE, closable = FALSE, enable_label = FALSE, 
                            label_text = NULL, label_status = "primary", enable_dropdown = FALSE, 
                            dropdown_icon = "wrench", dropdown_menu = NULL, enable_sidebar = FALSE, 
                            sidebar_content = NULL, sidebar_width = 25, sidebar_background = "#222d32", 
                            title_background="transparent",title_color="black",
                            sidebar_start_open = FALSE, footer_padding = TRUE){
  if (sidebar_width < 0 || sidebar_width > 100) 
    stop("The box sidebar should be between 0 and 100")
  boxClass <- "box"
  if (solidHeader || !is.null(background)) {
    boxClass <- paste(boxClass, "box-solid")
  }
  if (!is.null(status)) {
    validateStatusPlus(status)
    boxClass <- paste0(boxClass, " box-", status)
  }
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
  }
  if (!is.null(background)) {
    validateColor(background)
    boxClass <- paste0(boxClass, " bg-", background)
  }
  if (enable_sidebar) {
    if (sidebar_start_open) {
      boxClass <- paste0(boxClass, " direct-chat direct-chat-contacts-open")
    }
    else {
      boxClass <- paste0(boxClass, " direct-chat")
    }
  }
  style <- NULL
  if (!is.null(height)) {
    style <- paste0("height: ", shiny::validateCssUnit(height))
  }
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- shiny::div(class = "box-title",
                           title, 
                           style=paste0("background-color:",title_background,";color:",title_color,"; width:100%; font-size:20px; font-weight:bold; padding:10px; border-bottom:1px solid #ddd")
    )
  }
  boxToolTag <- NULL
  if (collapsible || closable) {
    boxToolTag <- shiny::tags$div(class = "box-tools pull-right")
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- status %OR% "default"
    collapseIcon <- if (collapsed) 
      "plus"
    else "minus"
    collapseTag <- shiny::tags$button(class = paste0("btn btn-box-tool"), 
                                      `data-widget` = "collapse", shiny::icon(collapseIcon))
  }
  closableTag <- NULL
  if (closable) {
    closableTag <- shiny::tags$button(class = "btn btn-box-tool", 
                                      `data-widget` = "remove", type = "button", shiny::tags$i(shiny::icon("times")))
  }
  labelTag <- NULL
  if (enable_label) {
    labelTag <- dashboardLabel(label_text, status = label_status)
  }
  dropdownTag <- NULL
  if (enable_dropdown) {
    dropdownTag <- shiny::tags$div(class = "btn-group", 
                                   shiny::tags$button(type = "button", class = "btn btn-box-tool dropdown-toggle", 
                                                      `data-toggle` = "dropdown", shiny::icon(dropdown_icon)), 
                                   shiny::tagList(dropdown_menu))
  }
  sidebarTag <- NULL
  if (enable_sidebar) {
    sidebarTag <- shiny::tags$button(class = "btn btn-box-tool", 
                                     `data-widget` = "chat-pane-toggle", `data-toggle` = "tooltip", 
                                     `data-original-title` = "More", title = NA, type = "button", 
                                     shiny::icon("info"))
  }
  boxToolTag <- shiny::tagAppendChildren(boxToolTag, labelTag, 
                                         dropdownTag, sidebarTag, collapseTag, closableTag)
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- shiny::tags$div(class = "box-header", titleTag, style="padding:0; background:white; border-radius: 5px 5px 0 0;",
                                 boxToolTag)
  }
  boxPlusTag <- shiny::tags$div(class = if (!is.null(width)) 
    paste0("col-sm-", width), shiny::tags$div(class = boxClass, id = id,
                                              style = paste0("border:1px solid #efefef;",style), 
                                                style, headerTag, shiny::tags$div(class = "box-body", 
                                                                                  ..., if (enable_sidebar) {
                                                                                    shiny::tags$div(style = "z-index: 10000;", class = "direct-chat-contacts", 
                                                                                                    shiny::tags$ul(class = "contacts-list", shiny::tags$li(style = paste0("width: ", 
                                                                                                                                                                          sidebar_width, "%;"), sidebar_content)))
                                                                                  }), if (!is.null(footer)) 
                                                                                    shiny::tags$div(class = if (isTRUE(footer_padding)) 
                                                                                      "box-footer"
                                                                                      else "box-footer no-padding", footer)))
  translation_rate <- paste0(100 - sidebar_width, "%")
  shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML(paste0(".direct-chat-contacts {\n                 -webkit-transform: translate(100%, 0);\n                 -ms-transform: translate(100%, 0);\n                 -o-transform: translate(100%, 0);\n                 transform: translate(100%, 0);\n                 position: absolute;\n                 top: 0;\n                 bottom: 0;\n                 height: 100%;\n                 width: 100%;\n                 background: ", 
                                                                                        sidebar_background, ";\n                 color: #fff;\n                 overflow: auto;\n              }\n              .direct-chat-contacts-open .direct-chat-contacts {\n                -webkit-transform: translate(", 
                                                                                        translation_rate, ", 0);\n                -ms-transform: translate(", 
                                                                                        translation_rate, ", 0);\n                -o-transform: translate(", 
                                                                                        translation_rate, ", 0);\n                transform: translate(", 
                                                                                        translation_rate, ", 0);\n              }\n              "))))), 
                 boxPlusTag)
}
# showcase social
rep_social_button<-function(url, type=c("twitter", "linkedin","mail")){
  if(type %in% c("linkedin", "twitter")){
    a(href=url,target="_blank", font="black", icon(type))
  }
  else if(type =="mail"){
    tags$a(icon("envelope"), target="_blank", style="color:grey",
           href=paste0("mailto:",url))
  }
  else{
    
  }
}


# tooltip
rep_tooltip<-function(title, tooltip, style=NULL){
    tags$span(
      class="rep_tooltip",style=style,
      title,
      tags$span(class="rep_tooltiptext", tooltip)
    )
  }
