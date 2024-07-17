mt_box<-function (..., id=NULL, title = NULL, footer = NULL, status = NULL, solidHeader = TRUE,
                  background = NULL, width = 6, height = NULL, collapsible = FALSE, maximizable=F, refresh=T,
                  collapsed = FALSE, closable = FALSE, enable_label = FALSE, title_color=NULL,
                  header_button=NULL, label_text = NULL, label_status = "primary", enable_dropdown = FALSE, 
                  dropdown_icon = "gear", dropdown_menu = NULL, enable_sidebar = FALSE, 
                  sidebar_content = NULL, sidebar_width = 25, sidebar_background = "#ddd", sidebar_icon="table",
                  sidebar_start_open = FALSE, footer_padding = TRUE, footer_style=NULL, display_if=NULL) 
{
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
  bodyStyle<-"flex:1;"
  #if(!is.null(min_height)){
  #  bodyStyle <- paste0(bodyStyle,"min-height: ", shiny::validateCssUnit(min_height),";")
 # }
  
  titleTagStyle<-paste0("display:flex;border-radius:5px 5px 0 0;padding:0; border-bottom:2px solid grey;")
  if(!is.null(title_color)){
    titleTagStyle=    paste0(titleTagStyle,"background-color:",title_color)
  }
  
  titleTag <- NULL
  if (!is.null(title)) {
    titleTag <- shiny::tags$h3(class = "box-title", title,style="margin-top:auto; margin-bottom:auto;margin-left:20px; flex:1;")
  }
  titleBtnTag<-NULL
  if(!is.null(header_button)){
    titleBtnTag<-shiny::tags$div(style="height:inherit",
                                 shiny::tags$div(class="mt_boxopt-content",
                                                 tags$span(icon("gears"), style="margin: auto 0;height:100%;",class="mt_boxopt-btn"),
                                                 shiny::tags$div(header_button, class="mt_boxopt-body"))
    )
    
  }
  
  boxToolTag <- NULL
  if (collapsible || closable) {
    boxToolTag <- shiny::tags$div(class = "box-tools pull-right")
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- "default"
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
  maximizableTag<- NULL
  if (maximizable) {
    maximizableTag<-actionButton(type = "button", class = "btn btn-box-tool shiny-bound-input", onclick=paste0('maximizeBox("',id,'")'),
                                 inputId=paste0("max_",id),style="border:none;height:100%",
                                 label=NULL,icon=icon("expand"),`data-widget` = "maximize")
  }
  refreshTag<- NULL
  if (refresh) {
    refreshTag<-actionButton(type = "button", class = "btn btn-box-tool shiny-bound-input", inputId=paste0("refresh_",id),style="border:none;height:100%;",label=NULL,icon=icon("refresh"))
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
                                     `data-original-title` = "Plot data", title = NA, type = "button", 
                                     shiny::icon(sidebar_icon))
  }
  boxToolTag <- shiny::tagAppendChildren(titleBtnTag,boxToolTag, labelTag, dropdownTag, sidebarTag,refreshTag,maximizableTag, collapseTag, closableTag)
  headerTag <- NULL
  
  header_id <-NULL
  body_id<-NULL
  footer_id<-NULL
  
  if(!is.null(id)){
    header_id=paste0(id, "_header")
    body_id=paste0(id, "_body")
    footer_id=paste0(id, "_footer")
  }
  
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- shiny::tags$div(class = "box-header", titleTag, style=titleTagStyle, id=header_id,
                                 tags$script(
                                   'function maximizeBox(id) {
                                   var element = document.getElementById(id);
                                   element.classList.toggle("maximize");
                                   
                                   this.classList.toggle("fa-thumbs-down");
  }
                                   
                                   '),
                                 boxToolTag)
}
  style=append(style, ";display:flex;flex-direction:column;box-shadow:5px 5px 10px grey")
  boxPlusTag <- shiny::tags$div(class = if (!is.null(width))
    paste0("col-sm-", width), shiny::tags$div(class = boxClass, id=id,
                                              `data-display-if` = display_if,
                                              style = if (!is.null(style)) 
                                                style, headerTag, shiny::tags$div(class = "box-body",style=bodyStyle, id=body_id,
                                                                                  ..., if (enable_sidebar) {
                                                                                    shiny::tags$div(style = "z-index: 10000;", class = "direct-chat-contacts", 
                                                                                                    shiny::tags$ul(class = "contacts-list", shiny::tags$li(style = paste0("width: ", 
                                                                                                                                                                          sidebar_width, "%;"), sidebar_content)))
                                                                                  }), if (!is.null(footer)) 
                                                                                    shiny::tags$div(style=footer_style, id=footer_id,class = if (isTRUE(footer_padding)) 
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

mt_box_right_item<-function(...,title=NULL,title_inline=T, max_width="100px"){
  titleTag=NULL
  if(!is.null(title)){
    titleTag=tags$span(title, style="font-weight:700;font-size:12px;margin:auto 0;")
  }
  div(class="mt_boxopt-item",
      style=paste0(
        ifelse(title_inline,"display:flex;","display:block;"),
        "min-width:", validateCssUnit(max_width), ";"),
      titleTag,
      div(..., style="margin:auto 0")
  )
}


mt_box_showcase <- function(..., title = NULL, footer = NULL, status = NULL, solidHeader = TRUE, 
                            background = NULL, width = 6, height = NULL, collapsible = FALSE, 
                            collapsed = FALSE, closable = FALSE, enable_label = FALSE, 
                            label_text = NULL, label_status = "primary", enable_dropdown = FALSE, 
                            dropdown_icon = "wrench", dropdown_menu = NULL, enable_sidebar = FALSE, 
                            sidebar_content = NULL, sidebar_width = 25, sidebar_background = "#222d32", 
                            title_background="#3d485d",title_color="white",
                            sidebar_start_open = FALSE, footer_padding = TRUE){
  if (sidebar_width < 0 || sidebar_width > 100) 
    stop("The box sidebar should be between 0 and 100")
  boxClass <- "box mt_box-sc_body"
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
    titleTag <- shiny::div(class = "box-title showcase-box_title",
                           title, 
                           style=paste0("background-color:",title_background,";color:",title_color,";")
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
    headerTag <- shiny::tags$div(class = "box-header", titleTag, style="padding:0; text-align:center",
                                 boxToolTag)
  }
  boxPlusTag <- shiny::tags$div(class = if (!is.null(width)) 
    paste0("col-sm-", width), shiny::tags$div(class = boxClass, 
                                              style = if (!is.null(style)) 
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


#### create Tab box for Panels ####
mV_panelBox<-function (..., id = NULL, selected = NULL, title = NULL, width = 6, height = NULL, side = c("left", "right"), footer=NULL, subMenu=NULL) 
{
  side <- match.arg(side)
  content <- shiny::tabsetPanel(..., id = id, selected = selected)
  content[["children"]][[1]][["attribs"]][["style"]]="background:#f6f6f6"
  #content$attribs$class <- "nav-tabs-custom"
  if (!is.null(height)) {
    content <- tagAppendAttributes(content, style = paste0("height: ", 
                                                           validateCssUnit(height)))
  }
  if (side == "right") {
    content$children[[1]] <- tagAppendAttributes(content$children[[1]], 
                                                 class = "pull-right")
  }
  if (!is.null(title)) {
    if (side == "left") 
      titleClass <- "pull-right"
    else titleClass <- "pull-left"
    content$children[[1]] <- htmltools::tagAppendChild(content$children[[1]], 
                                                       tags$div(class = paste("header", titleClass), title))
  }
  content[["children"]][[1]][["attribs"]][["class"]]<-"metV-tabs nav shiny-tab-input"
  Names=c("header","boxContent")
  
  sortNames=c("header", "boxContent")
  if(!is.null(subMenu)){
    subMenuContent=div(class="metV-subItem",subMenu)
    content<-tagAppendChild(content,subMenuContent)
    sortNames=c("header", "SubMenu","boxContent")
    Names=c("header","boxContent","SubMenu")
  }
  if(!is.null(footer)){
    footerContent=div(class="metV-footerItem", footer)
    content<-tagAppendChild(content,footerContent)
    sortNames=append(sortNames, "footer")
    Names=append(Names, "footer")
  }
  names(content$children)=Names
  content$children=content$children[sortNames]
  div(class = paste0("col-sm-", width), 
      div(content,
          style="border-radius:5px;"))
  
}

#### create miniBox for main box ####
mV_miniBag<-function(id,metabolites,dropdown_content,active=F, title){
  boxID<-paste0(id,"_miniBag")
  boxName=title
  
  boxStyle="display:flex;padding:0px;border-radius:5px 5px 0px 0px;margin:0px 2px;height:25px;"
  if(active==T){
    boxStyle=paste0(boxStyle,"background-color:white;border-top: 1px solid #ddd;border-left: 1px solid #ddd;border-right: 1px solid #ddd;border-bottom: 1px solid white;position:relative;top:1px;z-index:500")
  }else{
    boxStyle=paste0(boxStyle,"background-color:#f6f6f6;border-top: 1px solid #ddd;border-left: 1px solid #ddd;border-right: 1px solid #ddd;border-bottom: 1px solid transparent;")
  }
  titleStyle=ifelse(length(metabolites)>0, "font-size:10px;color:black", "font-size:10px;color:#d2310b")
  
  boxClass=ifelse(active, "metV-miniBag_active", "metV-miniBag")
  buttonClass=ifelse(active, "metV-miniBag_btn_active", "metV-miniBag_btn")
  
  tags$div(id=boxID,class=boxClass,#style=boxStyle,
           tags$div(style="text-align:center;width:100%;flex:1;",
                    actionButton(inputId = paste0(boxID,"_select"),
                                 onclick=paste0('$("#shiny-tab-metabdetail")[0].className.includes("active"){smoothScroll(document.getElementById("',id,"'))"),
                                 label=tags$span(style=ifelse(active, "display:-webkit-box;font-weight:bold;background:transparent","display:-webkit-box;background:transparent"),
                                                 boxName,
                                                 tags$span("(",length(metabolites),")",style=titleStyle)),
                                 class=buttonClass),
                    #style="padding:0px 5px;font-size:100%;border:transparent;width:100%;height:100%;background-color:transparent"),
                    bsTooltip(id=paste0(boxID,"SelectMainBagSelection"),placement="bottom",trigger="hover",title=HTML(paste(boxName,br(),"Select this bag")))),
           
           tags$div(style="",
                    tags$div( # remove
                      style="width:100%;height:100%",
                      actionButton(inputId = paste0(boxID,"_rm"),label=tags$div(icon("remove")),class=buttonClass),
                      #style="padding:0px 5px;font-size:100%;border:transparent;width:100%;height:100%;border-radius:0px 5px 5px 0px;background-color:transparent;"),
                      bsTooltip(id=paste0(boxID,"_rm"),placement="bottom",trigger="hover",title="delete bag")
                    )
           )
  )
}

#### Create box for metabolite browser ####
mV_plotBox<-function(title, id, sidebar_content, box_content, collapsed=F){
  name_activate=paste0(id, "_editActivate")
  name_submit=paste0(id, "_editSubmit")
  text_submit=paste0(id, "_textSubmit")
  bodyClass=ifelse(collapsed, "box-body collapse in", "box-body collapse")
  
  column(12,id=id,
         tags$div(class="box box-solid direct-chat",
                  tags$div(class="box-header",style="background-color:#f6f6f6;padding:5px;height:35px;",
                           tags$div(class="box-title",style="display:-webkit-box;", 
                                    tags$div(style="display:-webkit-box",
                                             conditionalPanel(condition=paste0("input.",name_activate,'== input.',name_submit),style="display:-webkit-box",
                                                              title,
                                                              actionButton(inputId =name_activate, style="padding:0px;color:#ddd;border:transparent",label=icon("edit"))
                                             ),
                                             conditionalPanel(condition=paste0("input.",name_activate,'!= input.',name_submit),style="display:-webkit-box",
                                                              textInput(inputId = text_submit, label=NULL, value="", placeholder="new name"),
                                                              actionButton(inputId = name_submit, label=icon("edit"), style="padding:0px;border:transparent"))
                                    )
                           ),
                           tags$div(class="box-tools pull-right",style="display:-webkit-box;top:0px;",
                                    actionButton(inputId = paste0(id, "_minimize"), label="-", 'data-toggle'="collapse", 'data-target'=paste0("#", id, "_collapseBody"))
                           )
                  ),
                  tags$div(class=bodyClass,id=paste0(id, "_collapseBody"),
                           tags$div(class="row",style="padding:0px 15px;",box_content),
                           tags$div(style="z-index:10;color:black;background-color:white;", class="direct-chat-contacts",
                                    tags$ul(class="contacts-list",
                                            tags$li(style="width:25%",sidebar_content)))
                  )
         )
  )
  
}


#### Create a dropdown menu ####
mv_dropdownMenu <- function(mi,tabName) {
  mi$children[[1]]$attribs['data-toggle']="tab"
  mi$children[[1]]$attribs['data-value'] = tabName
  if(length(mi$attribs$class)>0 && mi$attribs$class=="treeview"){
    mi$attribs$class=NULL
  }
  mi
}

mV_panel<-function(id=NULL,title=NULL,buttons=NULL,body=NULL,footer=NULL, width="50%", background="white", height="600px", style=NULL){
  width=validateCssUnit(width)
  styleTag=NULL
  if(!is.null(style)){styleTag=style}
  styleTag=append(styleTag, paste0(";width:",width,";"))
  
  height=validateCssUnit(height)
  titleTag=NULL
  
  if(!is.null(title)) titleTag=div(tags$b(title), style="flex:1")
  
  buttonsTag=NULL
  if(!is.null(buttons) & is.list(buttons)) buttonsTag<-div(lapply(buttons, function(x) div(x)), style="display:flex")
  
  bodyTag=NULL
  if(!is.null(body)) bodyTag=body
  
  footerTag=NULL
  if(!is.null(footer)) footerTag=div(class="mv_panel-footer",footer)
  
  div(style=styleTag,class="mv_panel",
      div(class="mv_panel-box", style=paste0("background:",background),
          div(class="mv_panel-title",titleTag,buttonsTag),
          div(class="mv_panel-body", bodyTag, style=paste0("height:", height,";")),footerTag
      )
  )
}



mV_plotBox<-function (...,id, title = NULL, footer = NULL, height = NULL,
                      collapsible = FALSE,collapsed = FALSE, class="plotBox",
                      closable = FALSE, enable_dropdown = FALSE,
                      dropdown_icon = "info", dropdown_menu = NULL, enable_sidebar = FALSE, 
                      sidebar_content = NULL, sidebar_width = 25, sidebar_background = "white", 
                      sidebar_start_open = FALSE, footer_padding = FALSE) {
  if (sidebar_width < 0 || sidebar_width > 100) 
    stop("The box sidebar should be between 0 and 100")
  
  
  if(is.null(class)) stop("add box class to plotBox")
  boxClass <-class
  if (collapsible && collapsed) {
    boxClass <- paste(boxClass, "collapsed-box")
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
    titleTag <- shiny::tags$h3(class = paste0(class,"-title"), title)
  }
  boxToolTag <- NULL
  if (collapsible || closable) {
    boxToolTag <- shiny::tags$div(class = "box-tools pull-right")
  }
  collapseTag <- NULL
  if (collapsible) {
    buttonStatus <- "default"
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
                                     `data-original-title` = "More information", title = NA, type = "button", 
                                     shiny::icon("info"))
  }
  boxToolTag <- shiny::tagAppendChildren(boxToolTag, 
                                         dropdownTag, sidebarTag, collapseTag, closableTag)
  headerTag <- NULL
  if (!is.null(titleTag) || !is.null(collapseTag)) {
    headerTag <- shiny::tags$div(class = paste0(class,"-header"), titleTag, 
                                 boxToolTag)
  }
  boxPlusTag <- shiny::tags$div(class = "col-sm-12", id=id,
                                shiny::tags$div(class = boxClass, 
                                                style = if (!is.null(style)) 
                                                  style, headerTag, shiny::tags$div(class = "plotBox-body", 
                                                                                    ..., if (enable_sidebar) {
                                                                                      shiny::tags$div(class = "direct-chat-contacts", 
                                                                                                      shiny::tags$ul(class = "contacts-list", shiny::tags$li(style = paste0("width: ", 
                                                                                                                                                                            sidebar_width, "%;"), sidebar_content)))
                                                                                    }), if (!is.null(footer)) 
                                                                                      shiny::tags$div(class = if (isTRUE(footer_padding)) 
                                                                                        paste0(class,"-footer")
                                                                                        else paste0(class,"-footer no-padding"), footer)))
  translation_rate <- paste0(100 - sidebar_width, "%")
  shiny::tagList(shiny::singleton(shiny::tags$head(shiny::tags$style(shiny::HTML(paste0(".direct-chat-contacts {\n                 -webkit-transform: translate(100%, 0);\n                 -ms-transform: translate(100%, 0);\n                 -o-transform: translate(100%, 0);\n                 transform: translate(100%, 0);\n                 position: absolute;\n                 top: 0;\n                 bottom: 0;\n                 height: 100%;\n                 width: 100%;\n                 background: ", 
                                                                                        sidebar_background, ";\n                 color: #fff;\n                 overflow: auto;\n              }\n              .direct-chat-contacts-open .direct-chat-contacts {\n                -webkit-transform: translate(", 
                                                                                        translation_rate, ", 0);\n                -ms-transform: translate(", 
                                                                                        translation_rate, ", 0);\n                -o-transform: translate(", 
                                                                                        translation_rate, ", 0);\n                transform: translate(", 
                                                                                        translation_rate, ", 0);\n              }\n              "))))), 
                 boxPlusTag)
}


