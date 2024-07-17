# HuMet functions:browser #
# Author: Patrick Weinisch
# Last updated: 24.09.2021


# process data average  --------------------------
browser_process_average=function(x.data, subjects){
  timepoint_to_plottimepoint<-function(x, code=info_sample){
    chr_tp=rownames(x)
    num_tp=as.numeric(substr(chr_tp,3,4))
    if(70 %in% num_tp){
      num_tp[which(num_tp==70)]<-10.5
    }
    
    if(72 %in% num_tp){
      num_tp[which(num_tp==71)]<-11.5
    }
    
    if(72 %in% num_tp){
      num_tp[which(num_tp==72)]<-27.5
    }
    tp_output=code$plot_timepoint[which(code$timepoint%in%num_tp)]
    setdiff(num_tp, code$timepoint)
    return(tp_output)
  }
  out=suppressWarnings(lapply(x.data, function(x)
    data.frame(timepoints=timepoint_to_plottimepoint(x),
               mean=as.numeric(rowMeans(x[,which(as.numeric(colnames(x))%in% subjects)],na.rm=T)),
               sd=as.numeric(apply(x[,which(as.numeric(colnames(x))%in% subjects)], MARGIN=1,FUN = sd, na.rm=T)),
               min=as.numeric(apply(x[,which(as.numeric(colnames(x))%in% subjects)], MARGIN=1,FUN = min, na.rm=T)),
               max=as.numeric(apply(x[,which(as.numeric(colnames(x))%in% subjects)], MARGIN=1,FUN = max, na.rm=T)))))
  return(out)
}

# process data single  ---------------------------
browser_process_single =function(x.data, subjects){
  
  timepoint_to_plottimepoint<-function(x, code=info_sample){
    chr_tp=rownames(x)
    num_tp=as.numeric(substr(chr_tp,3,4))
    tp_output=code$plot_timepoint[which(code$timepoint%in%num_tp)]
    return(tp_output)
  }
  check_tp=function(x){
    plot_tp=timepoint_to_plottimepoint(x)
    if(nrow(x)>length(plot_tp)){
      useRows=as.numeric(substr(rownames(x),3,4))
      useRows=which(useRows %in% info_sample$timepoint)
      x=x[useRows,]
    }
    return(x)
  }
  if(any(is.na(names(x.data)))){
    rm_data_list=which(is.na(names(x.data)))
    x.data[rm_data_list]=NULL
  }
  for(metabolites in names(x.data)){
    x.data[[metabolites]]=x.data[[metabolites]]%>%check_tp()
    x.data[[metabolites]]=x.data[[metabolites]][,which(as.numeric(colnames(x.data[[metabolites]]))%in% subjects)] # filter subjects
    plot_tp=timepoint_to_plottimepoint(x.data[[metabolites]])
    
    x.data[[metabolites]]=cbind(timepoint_to_plottimepoint(x.data[[metabolites]]),x.data[[metabolites]]) #add plot time points
    colnames(x.data[[metabolites]])[1]="timepoints"
    x.data[[metabolites]]=as.data.frame(x.data[[metabolites]])
  }
  return(x.data)
}

# process timeLim --------------------------------
browser_process_time_limit=function(timepoint){
  if(is.null(timepoint)){out=c(1:52)}
  else{
    if(!exists("info_sample")) stop("browser_process_timelimit() could not find info_sample")
    out=info_sample$plot_timepoint[which(info_sample$timepoint %in% timepoint)]
  }
  return(out)
}

# plot template ----------------------------------
browser_plot_tmp =function(blocks=T, xlab="Time of day [h]"){
  
  
  ## time point blocks
  sample_timepoints_day1 =  info_sample$plot_timepoint[which(info_sample$day==1)]
  sample_timepoints_day2 =  info_sample$plot_timepoint[which(info_sample$day==2)]
  sample_timepoints_day3 =  info_sample$plot_timepoint[which(info_sample$day==3)]
  sample_timepoints_day4 =  info_sample$plot_timepoint[which(info_sample$day==4)]
  sample_timepoints_block1= info_sample$plot_timepoint[which(info_sample$block==1)]
  sample_timepoints_block2= info_sample$plot_timepoint[which(info_sample$block==2)]
  daytimes=                 info_sample$day_time[1:60]
  
  ## define xlab  (limits)
  x_lim=c(0,52)
  if(is.null(xlab)) xlab="Time points"
  
  ## create dummy_data
  sample_timepoints = info_sample$plot_timepoint[1:60] 
  
  dummy_data = data.frame(x = sample_timepoints,
                          y= 1,
                          stringsAsFactors = F) %>%
    highcharter::list_parse()
  
  # highcharts plot  theme 
  hc_theme <- options$browser$hc_theme
  
  
  ## draw empty plot
  hc_plot <- highchart() %>%
    hc_plotOptions(series=list(stickyTracking=FALSE, allowPointSelect=F, animation=FALSE), tooltip=list(borderWidth=1))%>%
    hc_add_theme(hc_theme) %>%
    hc_tooltip(crosshairs = T,useHTML=T, borderWidth = 1, sort = F, table = T,shared=F, split=F, borderColor=NULL) %>%
    hc_chart(zoomType="x",panning=TRUE,panKey="shift") %>%
    #hc_add_series(data=dummy_data,name="dummy_data") %>%
    hc_add_series(data=data.frame(x=-60,y=60),name="dummy_annotation", color="black", id="dummy_annotation",showInLegend = F)%>% 
    hc_yAxis(title="values",min=-1, max=1)%>%
    hc_xAxis(title = list(text = as.character(xlab)), 
             min=x_lim[1]-0.16,
             max=ifelse(x_lim[2]==max(sample_timepoints_day1) || x_lim[2]==max(sample_timepoints_day2) || x_lim[2]==max(sample_timepoints_day3) || x_lim[2]==max(sample_timepoints_day4),
                        x_lim[2]+0.16,
                        sample_timepoints[which(sample_timepoints==x_lim[2])+1]+0.16))%>%
    hc_xAxis(labels=list(formatter= JS("function(){
                                       if ($.inArray(this.value,[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16]) > -1) { return (this.value+8).toFixed(2); }
                                       else if ($.inArray(this.value,[18,19,20,21,22,23,24,25,26]) > -1) { return (this.value-10).toFixed(2); }
                                       else if (this.value==22.25) { return Number(this.value-10.1).toFixed(2); }
                                       else if ($.inArray(this.value,[22.5,23.5]) > -1) { return Number(this.value-10.2).toFixed(2); }
                                       else if (this.value==22.75) { return Number(this.value-10.3).toFixed(2); } 
                                       else if ($.inArray(this.value,[28,29,30,31,32,33,34,35,36,37,38,39]) > -1) { return (this.value-20).toFixed(2); }
                                       else if ($.inArray(this.value,[28.25,36.25]) > -1) { return Number(this.value-20.1).toFixed(2); }
                                       else if ($.inArray(this.value,[28.5,29.5,30.5,36.5,37.5]) > -1) { return Number(this.value-20.2).toFixed(2); }
                                       else if ($.inArray(this.value,[28.75,36.75]) > -1) {return Number(this.value-20.3).toFixed(2); }
                                       else if ($.inArray(this.value,[42,43,44,45,46,47,48,49,50,51,52]) > -1) { return (this.value-34).toFixed(2); }
                                       else if (this.value==50.25) { return Number(this.value-34.1).toFixed(2); }
                                       else if ($.inArray(this.value,[42.5,43.5,50.5,51.5]) > -1) { return Number(this.value-34.2).toFixed(2); }
                                       else if (this.value==50.75) { return (this.value-34.3).toFixed(2); }}")))%>%
    hc_xAxis(
      plotLines = list(list(width=0.5, value=17.5,color="black",zIndex=1), # day 1 end
                       list(width=0.5, value=as.character(0),color="blue",dashStyle="Dot",zIndex=1), #fasting
                       
                       list(width=0.5, value=as.character(18),color="blue",dashStyle="Dot",zIndex=1), # SLDr beginn
                       list(width=0.5, value=as.character(22),color="blue",dashStyle="Dot",zIndex=1), # SLDr end / SLD1 beginn 
                       list(width=0.5, value=as.character(26),color="blue",dashStyle="Dot",zIndex=1), # SLD1 end
                       
                       list(width=0.5, value=26.95,color="black",zIndex=4), # 4 week break
                       list(width=0.5, value=27.05,color="black",zIndex=4), # 4 week break
                       
                       list(width=0.5, value=as.character(28),color="blue",dashStyle="Dot",zIndex=1), # OGTT beginn
                       list(width=0.5, value=as.character(32),color="blue",dashStyle="Dot",zIndex=1), # OGTT end /SLD2 beginn
                       list(width=0.5, value=as.character(36), color="blue", dashStyle="Dot",zIndex=1),  # SLD2 end /PAT start
                       list(width=0.5, value=as.character(36.5),color="blue",dashStyle="Dot",zIndex=1), # PAT break
                       list(width=0.5, value=as.character(39),color="blue",dashStyle="Dot",zIndex=1), # PAT end
                       list(width=0.5, value=40.5,color="black",zIndex=4), # Day 3 end
                       
                       list(width=0.5, value=as.character(sample_timepoints["73"]),color="blue",dashStyle="Dot",zIndex=1),
                       list(width=0.5, value=as.character(42),color="blue",dashStyle="Dot",zIndex=1),
                       list(width=0.5, value=as.character(50),color="blue",dashStyle="Dot",zIndex=1)
      ))
  
  if(!blocks){
    hc_plot <- hc_plot %>%
      hc_xAxis(
        plotBands = list(
          list(color="rgba(0,0,0,0.01)",from=0,to=17.5),
          list(color="rgba(0,0,0,0.05)",from=17.5,to=26.95),
          list(color="rgba(0,0,0,0.01)",from=27.05,to=41.5),
          list(color="rgba(0,0,0,0.05)",from=40.5,to=58)
        )
      )
  }
  
  hc_plot <- hc_plot %>%
    hc_rm_series(names="dummy_data")
  return(hc_plot)
}

# plot annotation ----------------------------------
browser_plot_annotation =function(hc){
  ymax=hc$x$hc_opts$yAxis$max
  ymin=hc$x$hc_opts$yAxis$min
  yrange=abs(ymax)+ abs(ymin)
  yhigh=ymax+yrange/10
  ylow=ymax
  ymean=mean(c(yhigh,ylow),na.rm = T)
  
  cur_challenge = info_sample[!duplicated(info_sample$challengeName),] %>%
    select(challengeName, timepoint, plot_timepoint, challengeTPbeginn, challengeTPend, challengeColor, challengeNameShort)%>%
    rename(challenge=challengeName, beginn=challengeTPbeginn, end=challengeTPend, color=challengeColor, label=challengeNameShort)
  cur_challenge$tooltip=c("Extended Fasting",
                          "Standard Liquid Diet recovery","Standard Liquid Diet Block 1","Oral Glucose Tolerance Test","Standard Liquid Diet Block 2","Physical Activity Test","Oral lipid tolerance test","Cold stress")
  
  hc_plot = hc %>%
    hc_yAxis(max=yhigh)
  
  for(i in 1:nrow(cur_challenge)){
    hc_plot <- hc_plot %>% 
      hc_add_series(
        data=list(
          list(x=cur_challenge$beginn[i], y=as.numeric(yhigh+1), high=as.numeric(yhigh),low=as.numeric(ylow)),
          list(x=cur_challenge$end[i], y=as.numeric(yhigh+1), high=as.numeric(yhigh),low=as.numeric(ylow))
        ),
        lineColor=cur_challenge$color[i],fillColor=cur_challenge$color[i], 
        type="arearange",marker=list(enabled=F),enableMouseTracking=F,animation=F,showInLegend=F) %>%
      hc_add_series(
        data=data.frame(
          x=mean(c(cur_challenge$beginn[i],cur_challenge$end[i]), na.rm=T),
          y=ylow,
          label=cur_challenge$label[i],
          tooltip=cur_challenge$tooltip[i]
        ),
        dataLabels=list(format='{point.label}', align="center",enabled=T,verticalAlign="bottom"),
        zIndex=50,
        color=cur_challenge$color[i],
        cursor="eye",showInLegend=F,
        tooltip = list(useHTML=T,headerFormat=NULL,pointFormat='{point.tooltip}',backgroundColor=cur_challenge$color[i], color="black"),
        marker=list(enabled=F,symbol="circle"),
        type="scatter",lineWidth=20
      )
  }
  return(hc_plot)
}

# plot average with min max ------------------------

browser_plot_average_min_max=function(m.data,type="line",timeLim=NULL,ylab=NULL, split_chal=F){
  if(!exists("info_sample")) stop("browser_single_average_min_max() could not find info_sample")
  if(is.null(ylab)) ylab="values"
  if(nrow(m.data[[1]])==0) stop("browser_single_average_min_max() m.data does not contain any information")
  
  # get information
  timeLim = browser_process_time_limit(timeLim)
  curr_id= names(m.data)[1]
  curr_name= help_convert(curr_id, to="labels")
  curr_col = "black"
  curr_sym_mean = "circle"
  curr_sym_extreme="diamond"
  type_range="arearange"
  curr_data = m.data[[1]] %>% 
    dplyr::mutate(
      conc=ylab,
      ID=curr_id
    )
  
  # add split data
  if(split_chal){
    curr_data = curr_data %>% 
      dplyr::left_join(info_sample, by=c("timepoints"="plot_timepoint"))
    # plot by challenge
    new_data = curr_data %>% 
      dplyr::filter(timepoint %in% c(2,10,12,22,29,33,41,50)) %>% 
      dplyr::mutate(mean=0, max=0, min=0, sd=0)
    
    new_data[which(new_data$timepoint==2), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(0,1, "8.00","Fasting 12h", 8)
    new_data[which(new_data$timepoint==22), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(28,21, "8.00","Oral glucose tolerance test 0min", 8)
    new_data[which(new_data$timepoint==41), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(42,40, "8.00","Oral lipid tolerance test 0min", 8)
    
    
    curr_data$challengeName[which(curr_data$timepoint==10)] = "Fasting"
    curr_data$challengeName[which(curr_data$timepoint==12)] = "Fasting recovery (SLD)"
    curr_data$challengeName[which(curr_data$timepoint==29)] = "Oral glucose tolerance test"
    curr_data$challengeName[which(curr_data$timepoint==33)] = "Lunch (SLD)"
    curr_data$challengeName[which(curr_data$timepoint==50)] = "Oral lipid tolerance test"
    
    curr_data =curr_data %>% 
      rbind(new_data) %>% 
      dplyr::left_join(info_met, by="ID") %>%
      dplyr::mutate(
        tooltip=paste0(
          "<head><style>table, th, td {border-collapse: collapse;} th, td {padding: 1px;}</style></head><body>",
          "<table><tbody>",
          "<caption><b>",labels,"</b></caption>",
          "<tr><td>Study tp:</td><td>",timepoint," (",challengeTime,")","</td></tr>",
          "<tr><td>Time in study:</td><td>","Block: ",block,"; Day:",day,"; Time: ",day_time,"</td></tr>",
          "<tr><td>Super-pathway:</td><td>",SUPER.PATHWAY,"</td></tr>",
          "<tr><td>Sub-pathway:</td><td>",SUB.PATHWAY,"</td></tr>",
          "<tr><td>Platform/Fluid:</td><td>",platform_name," / ",Fluid,"</td></tr>",
          "<tr><td>Mean±sd (min/max):</td><td>",round(mean,2)," ± ", round(sd,2)," (",round(min,2)," / ",round(max,2),")","</td></tr>",
          "</tbody></table></body>",sep=""
        )
      ) %>% 
      dplyr::arrange(timepoint, challengeTPbeginn)
    
    plot_list = lapply(unique(curr_data$challengeName),function(x) curr_data %>% 
                         dplyr::filter(challengeName==x) %>% 
                         dplyr::select(timepoints,mean, min, max, tooltip, day_time, day) %>% 
                         dplyr::mutate(sort_order= as.numeric(timepoints)) %>% 
                         dplyr::rename(x=timepoints, y=mean, low=min, high=max, daytimes=day_time) %>% 
                         dplyr::arrange(sort_order))
    names(plot_list) = unique(curr_data$challengeName)
  }
  else{
    # plot by blocks
    curr_data =curr_data %>% 
      dplyr::left_join(info_sample, by=c("timepoints"="plot_timepoint"))%>% 
      dplyr::left_join(info_met, by="ID") %>%
      dplyr::mutate(
        tooltip=paste0(
          "<head><style>table, th, td {border-collapse: collapse;} th, td {padding: 1px;}</style></head><body>",
          "<table><tbody>",
          "<caption><b>",labels,"</b></caption>",
          "<tr><td>Study tp:</td><td>",timepoint," (",challengeTime,")","</td></tr>",
          "<tr><td>Time in study:</td><td>","Block: ",block,"; Day:",day,"; Time: ",day_time,"</td></tr>",
          "<tr><td>Super-pathway:</td><td>",SUPER.PATHWAY,"</td></tr>",
          "<tr><td>Sub-pathway:</td><td>",SUB.PATHWAY,"</td></tr>",
          "<tr><td>Platform/Fluid:</td><td>",platform_name," / ",Fluid,"</td></tr>",
          "<tr><td>Mean±sd (min/max):</td><td>",round(mean,2)," ± ", round(sd,2)," (",round(min,2)," / ",round(max,2),")","</td></tr>",
          "</tbody></table></body>",sep=""
        )
      ) %>% 
      dplyr::arrange(timepoint, challengeTPbeginn)
    
    plot_list = lapply(1:2,function(x) curr_data %>% 
                         dplyr::filter(block==x) %>% 
                         dplyr::select(timepoints,mean, min, max, tooltip, day_time, day) %>% 
                         dplyr::mutate(sort_order= as.numeric(timepoints)) %>% 
                         dplyr::rename(x=timepoints, y=mean, low=min, high=max, daytimes=day_time)%>% 
                         dplyr::arrange(sort_order))
    names(plot_list) = c("block1","block2")
  }
  ## Create chart
  hc_plot=browser_plot_tmp() %>%
    hc_legend(enabled=FALSE)%>%
    hc_yAxis(max=max(curr_data$max, na.rm=T)*1.2, 
             min=ifelse(min(curr_data$min, na.rm=T)<0, min(curr_data$min, na.rm=T)*1.2, min(curr_data$min, na.rm=T)*0.8),
             startOnTick= F, 
             endOnTick=F,
             title=list(text=as.character(ylab)))%>%
    hc_exporting(enabled = T,
                 scale=2,
                 filename = "combined_plot"
                 # formAttributes = list(target = "_blank"), 
                 # url="https://humet.helmholtz-muenchen.de/hc-export/",
                 # buttons=list(contextButton=list(menuItems=list(
                 #   list(text="Save as PNG, 4:3",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PNG, 4:3, no legend",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PNG, 16:9",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PNG, 16:9, no legend",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PDF, 4:3",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PDF, 4:3, no legend",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PDF, 16:9",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PDF, 16:9, no legend",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}"))
                 # )))
                 )
  
  if(!is.null(timeLim)){
    hc_plot <- hc_plot %>%
      hc_xAxis(min=timeLim[1], max=timeLim[length(timeLim)])
  }
  
  for(x in names(plot_list)){
    hc_plot <- hc_plot %>%
      hc_add_series(data=plot_list[[x]],
                    name=curr_name,
                    id=paste(curr_id,1,sep=""),
                    tooltip = list(useHTML=T,headerFormat=NULL,pointFormat='{point.tooltip}'),
                    zIndex=50,
                    color=curr_col,marker=list(symbol=as.character(curr_sym_mean)),type=type,lineWidth=4)%>%
      hc_add_series(data=plot_list[[x]],
                    name=curr_name,
                    id=paste(curr_id,2,sep=""),
                    enableMouseTracking=F,
                    color=curr_col,
                    marker=list(enabled=F),
                    type=type_range,fillOpacity=0.3)
  }
  hc_plot=hc_plot %>%
    browser_plot_annotation()
  return(hc_plot)
}

# plot average with error bar ------------------------
browser_plot_average_errorbar=function(m.data,type="line",timeLim=NULL,ylab=NULL, split_chal=F){
  if(!exists("info_sample")) stop("browser_single_average_min_max() could not find info_sample")
  if(is.null(ylab)) ylab="values"
  if(nrow(m.data[[1]])==0) stop("browser_single_average_min_max() m.data does not contain any information")
  
  ## get information
  timeLim = browser_process_time_limit(timeLim)
  curr_id= names(m.data)[1]
  curr_name= help_convert(curr_id, to="labels")
  curr_col = "black"
  curr_sym_mean = "circle"
  curr_sym_extreme="diamond"
  type_range="errorbar"
  curr_data = m.data[[1]] %>% 
    dplyr::mutate(
      min=mean-sd,
      max=mean+sd,
      conc=ylab,
      ID=curr_id
    )
  
  # add split data
  if(split_chal){
    curr_data = curr_data %>% 
      dplyr::left_join(info_sample, by=c("timepoints"="plot_timepoint"))
    # plot by challenge
    new_data = curr_data %>% 
      dplyr::filter(timepoint %in% c(2,10,12,22,29,33,41,50)) %>% 
      dplyr::mutate(mean=0, max=0, min=0, sd=0)
    
    new_data[which(new_data$timepoint==2), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(0,1, "8.00","Fasting 12h", 8)
    new_data[which(new_data$timepoint==22), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(28,21, "8.00","Oral glucose tolerance test 0min", 8)
    new_data[which(new_data$timepoint==41), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(42,40, "8.00","Oral lipid tolerance test 0min", 8)
    
    
    curr_data$challengeName[which(curr_data$timepoint==10)] = "Fasting"
    curr_data$challengeName[which(curr_data$timepoint==12)] = "Fasting recovery (SLD)"
    curr_data$challengeName[which(curr_data$timepoint==29)] = "Oral glucose tolerance test"
    curr_data$challengeName[which(curr_data$timepoint==33)] = "Lunch (SLD)"
    curr_data$challengeName[which(curr_data$timepoint==50)] = "Oral lipid tolerance test"
    
    curr_data =curr_data %>% 
      rbind(new_data) %>% 
      dplyr::left_join(info_met, by="ID") %>%
      dplyr::mutate(
        tooltip=paste0(
          "<head><style>table, th, td {border-collapse: collapse;} th, td {padding: 1px;}</style></head><body>",
          "<table><tbody>",
          "<caption><b>",labels,"</b></caption>",
          "<tr><td>Study tp:</td><td>",timepoint," (",challengeTime,")","</td></tr>",
          "<tr><td>Time in study:</td><td>","Block: ",block,"; Day:",day,"; Time: ",day_time,"</td></tr>",
          "<tr><td>Super-pathway:</td><td>",SUPER.PATHWAY,"</td></tr>",
          "<tr><td>Sub-pathway:</td><td>",SUB.PATHWAY,"</td></tr>",
          "<tr><td>Platform/Fluid:</td><td>",platform_name," / ",Fluid,"</td></tr>",
          "<tr><td>Mean±sd (min/max):</td><td>",round(mean,2)," ± ", round(sd,2)," (",round(min,2)," / ",round(max,2),")","</td></tr>",
          "</tbody></table></body>",sep=""
        )
      ) %>% 
      dplyr::arrange(timepoint, challengeTPbeginn)
    
    plot_list = lapply(unique(curr_data$challengeName),function(x) curr_data %>% 
                         dplyr::filter(challengeName==x) %>% 
                         dplyr::select(timepoints,mean, min, max, tooltip, day_time, day) %>% 
                         dplyr::mutate(sort_order= as.numeric(timepoints)) %>% 
                         dplyr::rename(x=timepoints, y=mean, low=min, high=max, daytimes=day_time) %>% 
                         dplyr::arrange(sort_order))
    names(plot_list) = unique(curr_data$challengeName)
  }
  else{
    # plot by blocks
    curr_data =curr_data %>% 
      dplyr::left_join(info_sample, by=c("timepoints"="plot_timepoint"))%>% 
      dplyr::left_join(info_met, by="ID") %>%
      dplyr::mutate(
        tooltip=paste0(
          "<head><style>table, th, td {border-collapse: collapse;} th, td {padding: 1px;}</style></head><body>",
          "<table><tbody>",
          "<caption><b>",labels,"</b></caption>",
          "<tr><td>Study tp:</td><td>",timepoint," (",challengeTime,")","</td></tr>",
          "<tr><td>Time in study:</td><td>","Block: ",block,"; Day:",day,"; Time: ",day_time,"</td></tr>",
          "<tr><td>Super-pathway:</td><td>",SUPER.PATHWAY,"</td></tr>",
          "<tr><td>Sub-pathway:</td><td>",SUB.PATHWAY,"</td></tr>",
          "<tr><td>Platform/Fluid:</td><td>",platform_name," / ",Fluid,"</td></tr>",
          "<tr><td>Mean±sd (min/max):</td><td>",round(mean,2)," ± ", round(sd,2)," (",round(min,2)," / ",round(max,2),")","</td></tr>",
          "</tbody></table></body>",sep=""
        )
      ) %>% 
      dplyr::arrange(timepoint, challengeTPbeginn)
    
    plot_list = lapply(1:2,function(x) curr_data %>% 
                         dplyr::filter(block==x) %>% 
                         dplyr::select(timepoints,mean, min, max, tooltip, day_time, day) %>% 
                         dplyr::mutate(sort_order= as.numeric(timepoints)) %>% 
                         dplyr::rename(x=timepoints, y=mean, low=min, high=max, daytimes=day_time)%>% 
                         dplyr::arrange(sort_order))
    names(plot_list) = c("block1","block2")
  }
  
  ## Create chart
  hc_plot=browser_plot_tmp() %>%
    hc_legend(enabled=FALSE)%>%
    hc_yAxis(max=max(curr_data$max, na.rm=T)*1.2, 
             min=ifelse(min(curr_data$min, na.rm=T)<0, min(curr_data$min, na.rm=T)*1.2, min(curr_data$min, na.rm=T)*0.8),
             startOnTick= F, 
             endOnTick=F,
             title=list(text=as.character(ylab)))%>%
    # hc_exporting(enabled = TRUE,
    #              #scale=2,
    #              filename = "line-plot",
    #              buttons = list('csv', 'excel', 'pdf', 'png', 'jpeg', 'svg'))
    hc_exporting(enabled = T,
                 scale=2,
                 filename = "combined_plot"
                 # formAttributes = list(target = "_blank"),
                 # url="https://humet.helmholtz-muenchen.de/hc-export/",
                 # buttons=list(contextButton=list(menuItems=list(
                 #   list(text="Save as PNG, 4:3",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PNG, 4:3, no legend",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PNG, 16:9",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PNG, 16:9, no legend",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PDF, 4:3",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PDF, 4:3, no legend",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PDF, 16:9",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PDF, 16:9, no legend",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}"))
                 # )))
                 )
  
  if(!is.null(timeLim)){
    hc_plot <- hc_plot %>%
      hc_xAxis(min=timeLim[1], max=timeLim[length(timeLim)])
  }
  
  for(x in names(plot_list)){
    hc_plot <- hc_plot %>%
      hc_add_series(data=plot_list[[x]],
                    name=curr_name,
                    id=paste(curr_id,1,sep=""),
                    tooltip = list(useHTML=T,headerFormat=NULL,pointFormat='{point.tooltip}'),
                    zIndex=50,
                    color=curr_col,marker=list(symbol=as.character(curr_sym_mean)),type=type,lineWidth=4)%>%
      hc_add_series(data=plot_list[[x]],
                    name=curr_name,
                    id=paste(curr_id,2,sep=""),
                    enableMouseTracking=F,
                    color=curr_col,
                    marker=list(enabled=F),
                    type=type_range,
                    fillOpacity=0.3)
  }
  hc_plot=hc_plot %>%
    browser_plot_annotation()
  return(hc_plot)
}

# plot single with individuals ------------------------
browser_plot_single=function(m.data,type="line",timeLim=NULL,ylab=NULL, split_chal=F){
  if(!exists("info_sample")) stop("browser_plot_single() could not find info_sample")
  
  if(is.null(ylab)) ylab="values"
  if(nrow(m.data[[1]])==0) stop("browser_plot_single() m.data does not contain any information")
  if(!"subject" %in% names(options$general)) stop("browser_plot_single() subject data is  missing in options")
  person_colors = options$general$subject
  timeLim=browser_process_time_limit(timeLim)
  #get information
  
  curr_id= names(m.data)
  curr_name= help_convert(curr_id, to="labels")
  curr_col = "black"
  curr_sym_mean = "circle"
  curr_sym_extreme="diamond"
  curr_data = m.data[[1]] %>% 
    dplyr::mutate(
      conc=ylab,
      ID=curr_id
    )
  cur_subject <- intersect(person_colors$subject_number, names(curr_data))
  plot_list=list()
  # add split data
  if(split_chal){
    for(i in cur_subject){
      this_data <- curr_data %>% 
        select(all_of(i), timepoints, conc,ID) %>% 
        dplyr::rename("value"=paste0(i)) %>% 
        dplyr::left_join(info_sample, by=c("timepoints"="plot_timepoint"))
      
      new_data = this_data %>% 
        dplyr::filter(timepoint %in% c(2,10,12,22,29,33,41,50)) %>% 
        dplyr::mutate(value=0)
      
      new_data[which(new_data$timepoint==2), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(0,1, "8.00","Fasting 12h", 8)
      new_data[which(new_data$timepoint==22), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(28,21, "8.00","Oral glucose tolerance test 0min", 8)
      new_data[which(new_data$timepoint==41), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(42,40, "8.00","Oral lipid tolerance test 0min", 8)
      
      
      this_data$challengeName[which(this_data$timepoint==10)] = "Fasting"
      this_data$challengeName[which(this_data$timepoint==12)] = "Fasting recovery (SLD)"
      this_data$challengeName[which(this_data$timepoint==29)] = "Oral glucose tolerance test"
      this_data$challengeName[which(this_data$timepoint==33)] = "Lunch (SLD)"
      this_data$challengeName[which(this_data$timepoint==50)] = "Oral lipid tolerance test"
      
      this_data =this_data %>% 
        rbind(new_data) %>% 
        dplyr::left_join(info_met, by="ID") %>%
        dplyr::mutate(
          tooltip=paste0(
            "<head><style>table, th, td {border-collapse: collapse;} th, td {padding: 1px;}</style></head><body>",
            "<table><tbody>",
            "<caption><b>",labels,"</b></caption>",
            "<tr><td>Study tp:</td><td>",timepoint," (",challengeTime,")","</td></tr>",
            "<tr><td>Time in study:</td><td>","Block: ",block,"; Day:",day,"; Time: ",day_time,"</td></tr>",
            "<tr><td>Super-pathway:</td><td>",SUPER.PATHWAY,"</td></tr>",
            "<tr><td>Sub-pathway:</td><td>",SUB.PATHWAY,"</td></tr>",
            "<tr><td>Platform/Fluid:</td><td>",platform_name," / ",Fluid,"</td></tr>",
            "<tr><td>Value:</td><td>",round(value,2),"</td></tr>",
            "</tbody></table></body>",sep=""
          )
        ) %>%
        dplyr::arrange(timepoint, challengeTPbeginn)
      plot_list[[i]] = lapply(unique(this_data$challengeName),function(x) this_data %>% 
                                dplyr::filter(challengeName==x) %>% 
                                dplyr::select(timepoints,value, tooltip, day_time, day) %>% 
                                dplyr::mutate(sort_order= as.numeric(timepoints)) %>% 
                                dplyr::rename(x=timepoints, y=value,daytimes=day_time) %>% 
                                dplyr::arrange(sort_order))
      names( plot_list[[i]]) = unique(this_data$challengeName)
    }
  }
  else{
    for(i in cur_subject){
      this_data <- curr_data %>% 
        select(all_of(i), timepoints, conc,ID) %>% 
        dplyr::rename("value"=paste0(i)) %>% 
        dplyr::left_join(info_sample, by=c("timepoints"="plot_timepoint")) %>% 
        dplyr::left_join(info_met, by="ID") %>%
        dplyr::mutate(
          tooltip=paste0(
            "<head><style>table, th, td {border-collapse: collapse;} th, td {padding: 1px;}</style></head><body>",
            "<table><tbody>",
            "<caption><b>",labels,"</b></caption>",
            "<tr><td>Study tp:</td><td>",timepoint," (",challengeTime,")","</td></tr>",
            "<tr><td>Time in study:</td><td>","Block: ",block,"; Day:",day,"; Time: ",day_time,"</td></tr>",
            "<tr><td>Super-pathway:</td><td>",SUPER.PATHWAY,"</td></tr>",
            "<tr><td>Sub-pathway:</td><td>",SUB.PATHWAY,"</td></tr>",
            "<tr><td>Platform/Fluid:</td><td>",platform_name," / ",Fluid,"</td></tr>",
            "<tr><td>Value:</td><td>",round(value,2),"</td></tr>",
            "</tbody></table></body>",sep=""
          )
        ) %>%
        dplyr::arrange(timepoint, challengeTPbeginn)
      plot_list[[i]] = lapply(1:2,function(x) this_data %>% 
                                dplyr::filter(block==x) %>% 
                                dplyr::select(timepoints,value, tooltip, day_time, day) %>% 
                                dplyr::mutate(sort_order= as.numeric(timepoints)) %>% 
                                dplyr::rename(x=timepoints, y=value, daytimes=day_time)%>% 
                                dplyr::arrange(sort_order))
      names( plot_list[[i]])  = c("block1","block2")
    }
    
  }
  
  #create chart
  
  hc_plot=browser_plot_tmp() %>%
    hc_legend(enabled=T)%>%
    hc_yAxis(max=abs(max(curr_data[,intersect(names(curr_data),person_colors$subject_number)], na.rm=T))*1.2, 
             min=ifelse(min(curr_data[,intersect(names(curr_data),person_colors$subject_number)], na.rm=T)<0, min(curr_data[,intersect(names(curr_data),person_colors$subject_number)], na.rm=T)*1.2, min(curr_data[,intersect(names(curr_data),person_colors$subject_number)], na.rm=T)*0.8),
             startOnTick= F, 
             endOnTick=F,
             title=list(text=as.character(ylab)))%>%
    hc_exporting(enabled = T,
                 scale=2,
                 filename = "combined_plot"
                 # formAttributes = list(target = "_blank"), 
                 # url="https://humet.helmholtz-muenchen.de/hc-export/",
                 # buttons=list(contextButton=list(menuItems=list(
                 #   list(text="Save as PNG, 4:3",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PNG, 4:3, no legend",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PNG, 16:9",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PNG, 16:9, no legend",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PDF, 4:3",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PDF, 4:3, no legend",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PDF, 16:9",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PDF, 16:9, no legend",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}"))
                 # )))
                 )
  
  if(!is.null(timeLim)){
    hc_plot <- hc_plot %>%
      hc_xAxis(min=timeLim[1], max=timeLim[length(timeLim)])
  }
  
  for(x in names(plot_list)){
    curr_col = person_colors$subject_color[which(person_colors$subject_number==x)]
    curr_sym = person_colors$subject_symbol[which(person_colors$subject_number==x)]
    
    hc_plot <- hc_plot %>%
      hc_add_series(data = data.frame(x=-2, y=-2),
                    name=x,   
                    color=curr_col,
                    marker=list(symbol=as.character(curr_sym)),
                    id=x
      )
      
    for(i in names(plot_list[[x]])){
      # curr_col = person_colors$subject_color[which(person_colors$subject_number==x)]
      # curr_sym = person_colors$subject_symbol[which(person_colors$subject_number==x)]
      hc_plot <- hc_plot %>%
        hc_add_series(data = plot_list[[x]][[i]],
                      name=x,
                      id=x, #asdfasdfasdf
                      linkedTo=x,
                      tooltip = list(useHTML=T,headerFormat=NULL,pointFormat='{point.tooltip}'),
                      color=curr_col,
                      marker=list(symbol=as.character(curr_sym)),
                      type=type)
    }
  }
  
  hc_plot=hc_plot %>% 
    browser_plot_annotation()
    #hc_subtitle(text = "And this is a subtitle with more information",
    #          align = "left", style = list(color = "black", fontWeight = "bold"))
  
  return(hc_plot)
}


# plot average with multiple metabolites --------------
browser_plot_average_aggregated=function(m.data, type="line", timeLim=NULL,ylab=NULL, split_chal=F){
  if(!exists("info_sample")) stop("browser_plot_average_aggregated() could not find info_sample")
  if(is.null(ylab)) ylab="values"
  if(nrow(m.data[[1]])==0) stop("browser_plot_average_aggregated() m.data does not contain any information")
  
  # get information 
  timeLim=browser_process_time_limit(timeLim)
  met_ids=names(m.data)
  
  met_info=data.frame(ID=names(m.data),stringsAsFactors = F) %>% merge(info_met %>% dplyr::select(ID, labels), by="ID", all.x = T)
  met_info$symbols=rep(c("circle","square","triangle"),nrow(met_info))[1:nrow(met_info)]
  met_info$colors=rainbow(nrow(met_info),start=0, end=5/6)[1:nrow(met_info)]
  
  plot_list=list()
  # add split data
  if(split_chal){
    for(i in met_info$ID){
      this_data <- m.data[[i]] %>% 
        dplyr::mutate(ID=i) %>% 
        dplyr::left_join(info_sample, by=c("timepoints"="plot_timepoint"))
      
      new_data = this_data %>% 
        dplyr::filter(timepoint %in% c(2,10,12,22,29,33,41,50)) %>% 
        dplyr::mutate(mean=0, sd=0, min=0, max=0)
      
      new_data[which(new_data$timepoint==2), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(0,1, "8.00","Fasting 12h", 8)
      new_data[which(new_data$timepoint==22), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(28,21, "8.00","Oral glucose tolerance test 0min", 8)
      new_data[which(new_data$timepoint==41), c("timepoints","timepoint","day_time", "challengeTime","timepoint_time")] =c(42,40, "8.00","Oral lipid tolerance test 0min", 8)
      
      
      this_data$challengeName[which(this_data$timepoint==10)] = "Fasting"
      this_data$challengeName[which(this_data$timepoint==12)] = "Fasting recovery (SLD)"
      this_data$challengeName[which(this_data$timepoint==29)] = "Oral glucose tolerance test"
      this_data$challengeName[which(this_data$timepoint==33)] = "Lunch (SLD)"
      this_data$challengeName[which(this_data$timepoint==50)] = "Oral lipid tolerance test"
      
      this_data =this_data %>% 
        rbind(new_data) %>% 
        dplyr::left_join(info_met, by="ID") %>%
        dplyr::mutate(
          tooltip=paste0(
            "<head><style>table, th, td {border-collapse: collapse;} th, td {padding: 1px;}</style></head><body>",
            "<table><tbody>",
            "<caption><b>",labels,"</b></caption>",
            "<tr><td>Study tp:</td><td>",timepoint," (",challengeTime,")","</td></tr>",
            "<tr><td>Time in study:</td><td>","Block: ",block,"; Day:",day,"; Time: ",day_time,"</td></tr>",
            "<tr><td>Super-pathway:</td><td>",SUPER.PATHWAY,"</td></tr>",
            "<tr><td>Sub-pathway:</td><td>",SUB.PATHWAY,"</td></tr>",
            "<tr><td>Platform/Fluid:</td><td>",platform_name," / ",Fluid,"</td></tr>",
            "<tr><td>Mean±sd (min/max):</td><td>",round(mean,2)," ± ", round(sd,2)," (",round(min,2)," / ",round(max,2),")","</td></tr>",
            "</tbody></table></body>",sep=""
          )
        ) %>%
        dplyr::arrange(timepoint, challengeTPbeginn) %>% 
        dplyr::mutate(symbol=met_info$symbols[which(met_info$ID==i)],
                      color=met_info$colors[which(met_info$ID==i)])
      
      plot_list[[i]] = lapply(unique(this_data$challengeName),function(x) this_data %>% 
                                dplyr::filter(challengeName==x) %>% 
                                dplyr::select(timepoints,mean, tooltip, day_time, day, color, symbol) %>% 
                                dplyr::mutate(sort_order= as.numeric(timepoints)) %>% 
                                dplyr::rename(x=timepoints, y=mean,daytimes=day_time) %>% 
                                dplyr::arrange(sort_order))
      names( plot_list[[i]]) = unique(this_data$challengeName)
    }
  }
  else{
    for(i in met_info$ID){
      this_data <- m.data[[i]] %>% 
        dplyr::mutate(ID=i) %>% 
        dplyr::left_join(info_sample, by=c("timepoints"="plot_timepoint")) %>% 
        dplyr::left_join(info_met, by="ID") %>%
        dplyr::mutate(
          tooltip=paste0(
            "<head><style>table, th, td {border-collapse: collapse;} th, td {padding: 1px;}</style></head><body>",
            "<table><tbody>",
            "<caption><b>",labels,"</b></caption>",
            "<tr><td>Study tp:</td><td>",timepoint," (",challengeTime,")","</td></tr>",
            "<tr><td>Time in study:</td><td>","Block: ",block,"; Day:",day,"; Time: ",day_time,"</td></tr>",
            "<tr><td>Super-pathway:</td><td>",SUPER.PATHWAY,"</td></tr>",
            "<tr><td>Sub-pathway:</td><td>",SUB.PATHWAY,"</td></tr>",
            "<tr><td>Platform/Fluid:</td><td>",platform_name," / ",Fluid,"</td></tr>",
            "<tr><td>Mean±sd (min/max):</td><td>",round(mean,2)," ± ", round(sd,2)," (",round(min,2)," / ",round(max,2),")","</td></tr>",
            "</tbody></table></body>",sep=""
          )
        ) %>%
        dplyr::arrange(timepoint, challengeTPbeginn) %>% 
        dplyr::mutate(symbol=met_info$symbols[which(met_info$ID==i)],
                      color=met_info$colors[which(met_info$ID==i)])
      
      plot_list[[i]] = lapply(1:2,function(x) this_data %>% 
                                dplyr::filter(block==x) %>% 
                                dplyr::select(timepoints,mean, tooltip, day_time, day, color, symbol) %>% 
                                dplyr::mutate(sort_order= as.numeric(timepoints)) %>% 
                                dplyr::rename(x=timepoints, y=mean, daytimes=day_time)%>% 
                                dplyr::arrange(sort_order))
      names( plot_list[[i]])  = c("block1","block2")
    }
    
  }
  
  
  # generate plot 
  hc_plot=browser_plot_tmp() %>%
    hc_legend(enabled=T)%>%
    hc_exporting(enabled = T,
                 scale=2,
                 filename = "combined_plot"
                 # formAttributes = list(target = "_blank"), 
                 # url="https://humet.helmholtz-muenchen.de/hc-export/",
                 # buttons=list(contextButton=list(menuItems=list(
                 #   list(text="Save as PNG, 4:3",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PNG, 4:3, no legend",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PNG, 16:9",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PNG, 16:9, no legend",onclick=JS("function() { this.exportChart({type:'image/png',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PDF, 4:3",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PDF, 4:3, no legend",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:720,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}")),
                 #   list(text="Save as PDF, 16:9",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{itemStyle:{fontSize:'9.5px'}}});}")),
                 #   list(text="Save as PDF, 16:9, no legend",onclick=JS("function() { this.exportChart({type:'application/pdf',sourceWidth:960,sourceHeight:540},{chart:{style:{fontFamily:'sans-serif'}},legend:{enabled:false}});}"))
                 # )))
                 )
  if(!is.null(timeLim)){
    hc_plot <- hc_plot %>%
      hc_xAxis(min=timeLim[1], max=timeLim[length(timeLim)])
  }
  
  plot_max=NULL
  plot_min=NULL

  for(x in names(plot_list)){
    hc_plot <- hc_plot %>%
      hc_add_series(data = data.frame(x=-2, y=-2),
                    name=info_met$labels[which(info_met$ID==x)],   
                    color=unique(plot_list[[x]][[1]]$color),
                    marker=list(symbol=as.character(unique(plot_list[[x]][[1]]$symbol))),
                    id=x
      )
    
    for(i in names(plot_list[[x]])){
      plot_max=c(plot_max, max(plot_list[[x]][[i]]$y, na.rm=T))
      plot_min=c(plot_min, min(plot_list[[x]][[i]]$y, na.rm=T))
      hc_plot <- hc_plot %>%
        hc_add_series(data = plot_list[[x]][[i]],
                      name=x,
                      id=x,
                      linkedTo=x,
                      tooltip = list(useHTML=T,headerFormat=NULL,pointFormat='{point.tooltip}'),
                      color=unique(plot_list[[x]][[i]]$color),
                      marker=list(symbol=unique(plot_list[[x]][[i]]$symbol)),
                      type=type)
    }
  }
  
  hc_plot=hc_plot %>% 
    hc_yAxis(max=max(plot_max, na.rm=T)*1.2, 
             min=ifelse(min(plot_min, na.rm=T)<0, min(plot_min, na.rm=T)*1.2, min(plot_min, na.rm=T)*0.8),
             startOnTick= F, 
             endOnTick=F,
             title=list(text=as.character(ylab)))%>%
    browser_plot_annotation()
  
  return(hc_plot)
}

# add information to plots --------------
browser_metabolite_annotation_inter_var<-function(x.data, tp, subj, mean=T){
  # timepoint
  out=sapply(names(x.data), function(y){
    this_data=x.data[[y]]
    match_tp=data.frame(name=rownames(this_data),
                        my_tp=rownames(this_data) %>% substring(3,4),
                        timepoint=rownames(this_data) %>% substring(3,4) %>% as.numeric()
    ) %>% 
      dplyr::filter(timepoint %in% tp)
    
    match_subject=data.frame(name=names(this_data),
                             subject=names(this_data) %>% as.numeric) %>%
      dplyr::filter(subject %in% subj)
    
    inter_var=sapply(match_tp$name, function(x) var(this_data[x,]%>% t(), na.rm=T)) %>% as.numeric()
    if(mean){
      my_out= mean(inter_var, na.rm=T)%>% round(2)
    }
    else{
      my_out=data.frame(timepoint=match_tp$timepoint,
                     var= inter_var %>% as.numeric())
    }
    my_out
  }, USE.NAMES = TRUE)
  return(out)
}

browser_metabolite_annotation_intra_var<-function(x.data, tp, subj, mean=T){
  # subject
  out=sapply(names(x.data), function(y){
    this_data=x.data[[y]]
    match_tp=data.frame(name=rownames(this_data),
                        my_tp=rownames(this_data) %>% substring(3,4),
                        timepoint=rownames(this_data) %>% substring(3,4) %>% as.numeric()
    ) %>% 
      dplyr::filter(timepoint %in% tp)
    
    match_subject=data.frame(name=names(this_data),
                             subject=names(this_data) %>% as.numeric) %>%
      dplyr::filter(subject %in% subj)
    
    inter_var=sapply(match_subject$name, function(x) var(this_data[,x], na.rm=T)) %>% as.numeric()
    if(mean){
      my_out= mean(inter_var, na.rm=T) %>% round(2)
    }
    else{
      my_out=data.frame(subject=match_subject$subject,
                     var= inter_var %>% as.numeric())
    }
    my_out
  }, USE.NAMES = TRUE)
  return(out)
}

browser_metabolite_annotation <- function(id, table_id=NULL, rdata=NULL, subj=NULL, tp=NULL) {
  if(!exists("info_met")) stop("browser_metabolite_annotation(), could not find info_met")
  my_info=info_met %>% 
    dplyr::filter(ID %in% id) %>%
    dplyr::select(labels, SUPER.PATHWAY, SUB.PATHWAY, HMDB, KEGG, PUBCHEM,chebi,chebi_synonyms,lipidmaps_synonyms,swisslipids_synonyms) %>% 
    dplyr::mutate(HMDB= HMDB %>% rep_link_hmdb(),
                  KEGG=KEGG %>% rep_link_kegg(),
                  PUBCHEM = PUBCHEM %>% rep_link_pubchem(),
                  chebi = chebi %>% rep_link_chebi(),
                  ) %>% 
    setNames(names(.[]) %>% gsub(pattern="_",replacement=" ")) %>% 
    dplyr::rename("Super-pathway*"=SUPER.PATHWAY,"Sub-pathway*"=SUB.PATHWAY, Metabolite=labels) %>% 
    tibble::column_to_rownames(var="Metabolite")
  
  if(!is.null(rdata)){
    my_info[["inter-individual"]]=rdata[id] %>% browser_metabolite_annotation_inter_var(subj=subj, tp=tp)
    my_info[["intra-individual"]]=rdata[id] %>% browser_metabolite_annotation_intra_var(subj=subj, tp=tp)
  }
  
  if(!is.null(rep_datatable_row_hover)){
    my_table=DT::datatable(my_info %>% t() %>% as.data.frame(),   
                           rownames=T,
                           width="100%",
                           escape = F,
                           options=list(dom='t',
                                        autoWidth = F,
                                        scrollX = TRUE,
                                        scrollY = TRUE,
                                        rowCallback = rep_datatable_row_hover (table_id),
                                        columnDefs = list(
                                          #list(width = '10px', targets = "_all")
                                          #list(width = '8px', targets = c(1:5))
                                        )
                           )
    ) %>% suppressWarnings()
  }else{
    my_table=DT::datatable(my_info %>% t() %>% as.data.frame(),   
                           rownames=F,
                           width="100%",
                           escape = F,
                           style="bootstrap",
                           selection = "none",
                           options=list(dom='t',
                                        autoWidth = F,
                                        scrollX = TRUE,
                                        scrollY = TRUE,
                                        
                                        columnDefs = list(
                                          #list(width = '10px', targets = "_all")
                                          #list(width = '8px', targets = c(1:5))
                                        )
                           )
    ) %>% suppressWarnings()
  }
  return(my_table)
}


# color imputed --------------
browser_single_imputed <- function(x.plot, raw_data=db_data$norm_imp, add_imp=T, col="red"){
  if(add_imp){
  }else{
    out.plot=x.plot
  }
  return(out.plot)
}

# test
#x.plot=browser_process_average(x.data=db_data$norm_imp[1], subjects=1:15)%>%
#  browser_plot_average_errorbar()
#m.data=browser_process_single(x.data=db_data$norm_imp[1], subjects=1:15) %>% 
#  browser_plot_single() 
#
#m.data
