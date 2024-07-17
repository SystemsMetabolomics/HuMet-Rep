# functions about
about_study_block<-function(...){
  div(style="display:flex;flex-direction:row;border-radius:5px; background:#ddd;padding:10px",
      ...)
}

about_study_tp<-function(tag, tp, width=NULL, tooltip=T){
  if(!exists("info_sample")) stop("about_tp_button() could not find info_sample")
  
  if(is.null(width)){
    width=paste0(info_sample$challenge_width[which(info_sample$timepoint==tp)],"%")
  }
  
  nr=which(info_sample$timepoint==tp)
  tooltipTag=NULL
  if(tooltip){
    tooltipTag=shinyBS::bsTooltip(id=paste0(tag,tp), placement = "bottom", trigger="hover", title=info_sample$sliderLabels[nr])
  }
  out=div(style=paste0("display:inline-block;border-top:2px solid black;width:",width,";"),
    actionButton(inputId=paste0(tag,tp), 
                 class="about-tpbutton",
               label=div(
                 div(style="height:10px",paste0(ifelse(info_sample$Plasma[nr],"●",""))),
                 div(style="height:10px",paste0(ifelse(info_sample$Urine[nr],"■",""))),
                 div(style="height:10px",paste0(ifelse(info_sample$BA[nr],"▲",""))),
                 div(style="height:10px",paste0(ifelse(info_sample$BC[nr],"◆",""))),
                 div(style=ifelse(grepl(".15", info_sample$day_time[nr]),"height:10px;visibility:hidden","height:10px;"),
                     paste0(info_sample$day_time[nr]),
                     )
               )
    ),
    tooltipTag
  )
  return(out)
}

about_study_info<-function(id){
  if(!exists("info_sample")) stop("about_tp_button() could not find info_sample")
  out=div(style="border-top:2px solid transparent;width:90px;margin-top:auto;position:relative;bottom:25px;",
            div(actionButton(inputId=paste0(id,"_plasma"),label="Plasma",class="about-infobutton"),style="height:10px"),
            div(actionButton(inputId=paste0(id,"_urine"),label="Urine",class="about-infobutton"),style="height:10px"),
            div(actionButton(inputId=paste0(id,"_ba"), label="Breath air",class="about-infobutton"),style="height:10px"),
            div(actionButton(inputId=paste0(id,"_bc"), label="Breath condensate",class="about-infobutton"),style="height:10px")
          )
          
  return(out)
}

about_study_night<-function(color="orange", font_size="20px"){
  
  div(style=paste0("display:inline-block;width:15px;margin: auto 10px;text-align:center;color:",color,";","font-size:",font_size,";"),
      icon("moon")
      )
}
about_study_challenge<-function(..., width="100%",title=NULL, title_img=NULL){
  titleTag=NULL
  if(!is.null(title)){
    titleTag=div(paste0(title), style="font-size:12px;text-align:center")
  }
  
  imgTag=NULL
  if(!is.null(title_img)){
    imgTag=div(img(src=title_img, style="display:block; height:40px; margin-left:auto;margin-right:auto"))
  }
  
  div(style=paste0("width:",width,";","display:inline-block;"),
      imgTag,
      titleTag,
    div(..., style=paste0("display:flex"))
  )
}

about_study_day<-function(..., width="50%",day_name=NULL){
    div(style=paste0("display:inline-block;background:#efefef; border-radius:5px;padding:10px;width:",width),
        div(tags$b(day_name,style="display:block;text-align:center")),
        div( ..., style="width:100%;")
    )
}

about_study_complete<-function(tag,width="100%"){
  if(!exists("info_sample")) stop("about_study_complete() could not find info_sample")
  
  my_info = info_sample %>% filter(!is.na(width))
  my_info$width=my_info$width %>%as.numeric()
  challenges=data.frame(
    stringsAsFactors = F,
    names=my_info$challengeName %>% unique(),
    width=c("100%",
            "45%","45%",
            "33%","33%","20%",
            "66%","20%")
    #width=c("49%","24%","24%","19%","19%","9%","39%","9%")
    )
  my_challenge=lapply(challenges$names, function(x){
    about_study_challenge(title=x,width=challenges$width[which(challenges$names==x)], title_img=paste0("img/logo/",x,"_small.png"),
      lapply(my_info$timepoint[which(my_info$challengeName==x)], function(tp){
        about_study_tp(tag, tp, width=paste0(my_info$challenge_width[which(my_info$timepoint==tp)],"%"), tooltip=F)
      })
    )
  })
  
  names(my_challenge)=challenges$names
  
  div(style=paste0("width:",width),
      
    div( # block 1
      style="display:inline-block;width:100%;margin-top:5px;",
      about_study_block(
        about_study_info(id=paste0(tag,"_info")),
        img(src="img/logo/chicken_small.png",style="display:inline-block;height:30px;margin-right:10px;margin-top:auto;margin-bottom:auto;"),
        about_study_day(
          day_name="Day 1",
          my_challenge$Fasting
        ),
        about_study_night(),
        about_study_day(day_name="Day 2",
                        my_challenge$`Fasting recovery (SLD)`,
                        my_challenge$`Standard liquid diet`
        )
      )
    ),
    div(style="text-align:center",
      tags$b("//"),br(),
      tags$b("4 weeks break")
      ),
    div(
      # block 2
      style="display:inline-block;width:100%;margin-top:5px;",
      about_study_block(
        about_study_info(id=paste0(tag,"_info")),
        img(src="img/logo/chicken_small.png",style="display:inline-block;height:30px;margin-right:10px;margin-top:auto;margin-bottom:auto;"),
        about_study_day(
          day_name="Day 3",
                my_challenge$`Oral glucose tolerance test`,
                my_challenge$`Lunch (SLD)`,
                my_challenge$`Physical activity`
        ),
        about_study_night(),
        about_study_day(
          day_name="Day 4",
        my_challenge$`Oral lipid tolerance test`,
        my_challenge$`Cold stress test`
        )
      )
    )
  )
}

