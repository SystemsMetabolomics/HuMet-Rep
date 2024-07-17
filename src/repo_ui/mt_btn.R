mt_btn_checkboxes<-function(inputId, label=NULL, choice_info=NULL, selected=NULL, size="normal", direction="horizontal",justified=F,individual=F, width=NULL, status="metVgrp"){
  #labelTag
  if(is.null(label)){labelTag=NULL}else{labelTag=tags$b(label)}
  
  choice_list=list()
  for(i in 1:length(choice_info)){
    choice_list[[choice_info[[i]]$label]]=choice_info[[i]]$value
  }
  
  if(is.null(selected)){
    selected_list=NULL
  }else{
    selected_list=NULL
    for(i in 1:length(choice_info)){
      if(choice_info[[i]]$value %in% selected){
        selected_list=append(selected_list,choice_info[[i]]$value)
      }else{next}
    }
  }
  
  tags$div(
    labelTag,
    #checkboxGroupButtons(inputId=inputId, label=NULL, selected=selected_list,choices=choice_list, size=size, direction = direction, 
    #                     justified=justified, individual=individual, width=width, status=status),
    prettyCheckboxGroup(inputId=inputId, label=NULL, selected=selected_list,choices=choice_list,width=width),
                        #size=size, direction = direction, justified=justified, individual=individual,status=status),
    lapply(1:length(choice_info),function(x){
      #tags$script(paste0("$(\"input:checkbox[name='",inputId,"'][value='",choice_info[[x]]$value,"']\").parent().css('background-color', '",choice_info[[x]]$color,"');"))
      tags$script(paste0("$(\"input:checkbox[name='",inputId,"'][value='",choice_info[[x]]$value,"']\").parent.css('background-color', '",choice_info[[x]]$color,"');"))
    })
  )
}

mt_btn_checkboxes2<-function(inputId, label=NULL, choice_info=NULL, selected=NULL, size="normal", direction="horizontal",justified=F,individual=F, width=NULL, status="metVgrp"){
  #labelTag
  if(is.null(label)){labelTag=NULL}else{labelTag=tags$b(label)}
  
  choice_list=list()
  for(i in 1:length(choice_info)){
    choice_list[[choice_info[[i]]$label]]=choice_info[[i]]$value
  }
  
  if(is.null(selected)){
    selected_list=NULL
  }else{
    selected_list=NULL
    for(i in 1:length(choice_info)){
      if(choice_info[[i]]$value %in% selected){
        selected_list=append(selected_list,choice_info[[i]]$value)
      }else{next}
    }
  }
  
  tags$div(
    labelTag,
    #checkboxGroupButtons(inputId=inputId, label=NULL, selected=selected_list,choices=choice_list, size=size, direction = direction, 
    #                     justified=justified, individual=individual, width=width, status=status),
    prettyCheckboxGroup(inputId=inputId, label=NULL, selected=selected_list,choices=choice_list,width=width),
    #size=size, direction = direction, justified=justified, individual=individual,status=status),
    lapply(1:length(choice_info),function(x){
      #tags$script(paste0("$(\"input:checkbox[name='",inputId,"'][value='",choice_info[[x]]$value,"']\").parent().css('background-color', '",choice_info[[x]]$color,"');"))
      tags$script(paste0("$(\"input:checkbox[name='",inputId,"'][value='",choice_info[[x]]$value,"']\").parent.css('background-color', '",choice_info[[x]]$color,"');"))
    })
  )
}

mt_btn_radio<-function(inputId, label=NULL, choice_info=NULL, selected=NULL, size="normal", direction="horizontal",justified=F,individual=F, width=NULL, status="metVgrp"){
  #labelTag
  if(is.null(label)){labelTag=NULL}else{labelTag=tags$b(label)}
  choice_list=list()
  for(i in 1:length(choice_info)){
    choice_list[[choice_info[[i]]$label]]=choice_info[[i]]$value
  }
  
  if(is.null(selected)){
    selected_list=NULL
  }else{
    selected_list=NULL
    for(i in 1:length(choice_info)){
      if(choice_info[[i]]$value %in% selected){
        selected_list=append(selected_list,choice_info[[i]]$value)
      }else{next}
    }
    selected_list=selected_list[1]
  }
  
  tags$div(
    labelTag,
    radioGroupButtons(inputId=inputId, label=NULL, selected=selected_list,choices=choice_list, size=size, direction = direction, 
                      justified=justified, individual=individual, width=width, status=status)
  )
}

