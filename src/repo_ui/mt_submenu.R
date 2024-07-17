# submenu functions


mt_submenu_item<-function(..., id=NULL,width="20%", title=NULL, footer=NULL){
  titleContent=NULL
  if(!is.null(title)){
    titleContent=div(title, style="font-weight:bold;font-size:12px;")
  }
  footerContent=NULL
  if(!is.null(title)){
    footerContent=div(footer, class="mt_submenu-footer")
  }
  styleTag =NULL
  if(!is.null(width)){
    styleTag=paste0("width:",validateCssUnit(width),";")
  }
  
  div(style=styleTag,
      class="mt_submenu-body",
      titleContent,
      div(..., class="mt_submenu-content"),
      footerContent
  )
}