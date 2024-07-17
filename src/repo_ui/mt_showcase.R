mt_showcase_panel<-function (inputId,..., header=NULL, style="min-width:33%", body_style=NULL,header_style="background:#ddd",footer_style=NULL,btn_label="More") 
{
  div(
    class="mt_showcase_panel-content",
    style=style,
    div(
      class="mt_showcase_panel-header",
      header,
      style=header_style
    ),
    div(
      class="mt_showcase_panel-body",
      ...,
      style=body_style
    ),
    div(
      class="mt_showcase_panel-footer",
      actionButton(inputId = inputId, label=btn_label, class="mt_showcase_panel-btn"),
      style=footer_style
    )
  )
}


mt_showcase_container<-function(..., center=T,width="80%", size="lg"){
  sizeTag=NULL
  if(size=="lg") sizeTag="mv_showcase-lg"
  div(style=paste0("width:",width,";"),
      class=paste("mt_showcase-wrapper",sizeTag),
      ...
  )
}

mt_showcase_img<-function(img_src, border_radius="5px",img_background="white", link=NULL){
  div(style=paste0("height:inherit;border-top-right-radius:",border_radius,"; border-top-left-radius:",border_radius, ";background:",img_background),
      img(src = img_src, class="mt_showcase-img")
  )
}



mt_href<-function(href,label, color=NULL, style=NULL){
  if(is.null(color) & "link_highlight" %in% names(options$general$colors)){
    color=options$general$colors$link_highlight #"#b98474"
  }else{
    color="black"
  }
  out=a(href=href,target="_blank",tags$span(label), style=paste0("color:",color,";", style))
  return(out)
}


