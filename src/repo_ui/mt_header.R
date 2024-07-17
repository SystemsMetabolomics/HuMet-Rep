
# MetaboTime functions: header #
# Author: Patrick Weinisch
# Last updated: 01.06.2021

mt_header_dropdown_new=function (..., label=NULL,icon = "eye",display_if='input.menu =="metabselect" || input.menu =="metabdetail" ||input.menu =="network" ||input.menu =="stats_pca" ||input.menu =="stats_ttest"'){
  
  items <- list(...)
  dropdownClass <- paste0("dropdown ", "warning", "-menu")
  tags$li(class = "dropdown warning-menu mt_header",
          `data-display-if`=display_if,
          a(href = "#", class = "mt_header-title dropdown-toggle", `data-toggle` = "dropdown",
            style="padding:0 5px; color:white;",
            div(
              div(label,class="mt_header-btn_large"),
              div(icon(icon),class="mt_header-btn_small",style="position:relative;top:25%")
            )
          ), 
          tags$ul(class = "dropdown-menu mt_dropdown-menu",
                  style="top:48px;",
                  tags$li(tags$ul(class = "menu", style="padding:0",
                                  items))
          ))
}


# header functions #### rm

mt_header_wrapper<-function (..., id, title, subtitle=NULL,headerText=NULL,fontsize="12px", hide_icon="info",style=NULL,display_if=NULL) {
  tags$li(class = "mt_dropdown-wrapper dropdown mtd_text",style=style,`data-display-if` = display_if,
          a(href = "#", class = "mt_dropdown-btn",  id=id,
            style="height:100%;padding:2px 5px;background:none;",
            div(style="text-align:center;overflow:hidden", class="mt_dropdown-btn_large",
                tags$span(title, style="display:block;font-weight:inherit;height:15px;"),
                subtitle),
            div(style="text-align:center", class="mt_dropdown-btn_small",icon(hide_icon))
          ), 
          tags$div(class = "mt_dropdown-content",
                   div(class="mt_dropdown-body",style=paste0("font-size:",validateCssUnit(fontsize)),
                       ...
                   )
          )
  )
}


mt_header_bag<-function(id,title,body_style=NULL, active=T){
  if(is.null(id))stop("add box id to mt_header_bag")
  if(is.null(title))stop("add title to mt_header_bag")
  bodyStyle=NULL
  if(!is.null(body_style))bodyStyle<-append(bodyStyle,body_style)
  
  if(active){
    body_class="mt_header_bag-body mt_header_bag-active"
  }else{
    body_class="mt_header_bag-body"
  }
  tags$div(id=paste0("header_",id,"_body"),class=body_class, style=bodyStyle,
           actionButton(inputId = paste0("header_",id,"_select"), class="mt_header_bag-select",
                        #onclick=paste0("smoothScroll(document.getElementById('",id,"'))"),
                        #onclick=paste0('$("#shiny-tab-metabdetail")[0].className.includes("active"){smoothScroll(document.getElementById("',id,"'))"),
                        label=div(
                          style="display:flex",
                          title
                        )
           ),
           bsTooltip(id=paste0("header_",id,"_select"),placement="bottom",trigger="hover",title=paste("Select this bag")),
           actionButton(inputId = paste0("header_",id,"_rm"),
                        label=tags$div(icon("remove")),
                        class="mt_header_bag-rm"),
           bsTooltip(id=paste0("header_",id,"_remove"),placement="bottom",trigger="hover",title="delete bag")
  )
}
mt_header_submenu<-function(.header, ..., style=NULL){
  styleTag="display:block; position:relative; float:left; width:100%;z-index:500;"
  if(!is.null(style)) styleTag=paste0(styleTag, style)
  .header[["children"]][[3]]<-tagAppendChild(.header[["children"]][[3]],
                                             div(..., id="header_submenu",
                                                 style=styleTag))
  
  return(.header)
}



mt_header_dropdown=function (..., title=NULL,subtitle=NULL, icon = NULL, display_if=NULL, color="white"){
  iconTag=NULL
  if(is.null(icon)){iconTag=icon("eye")}
  else{iconTag=icon(icon)
  }
  
  items <- list(...)
  dropdownClass <- paste0("dropdown ", "warning", "-menu")
  tags$li(class = dropdownClass, 
          `data-display-if`=display_if,
          a(href = "#", class = "mt_header-title dropdown-toggle", `data-toggle` = "dropdown",# style=paste0("text-align:center; padding:2px 5px; color:",paste0(color)),
            div(
              div(title,subtitle,class="mt_header-btn_large"),
              div(iconTag,class="mt_header-btn_small",style="position:relative;top:25%")
            )
            ), 
          tags$ul(class = "dropdown-menu",
                  style="padding:5px;top:48px;",
                  tags$li(tags$ul(class = "menu", style="padding:0",
                                  items))
                  ))
}
