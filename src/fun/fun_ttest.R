# All functions for module ttest

ttest_plot_volcano<-function(x.data, colorby=c("super","platform"), type_thresh=c("none","fdr","bonferroni")){
  if(!colorby %in% c("super","platform","none") | length(colorby)!=1) stop("statistic_ttest_volcano() could not match colorby")
  if(nrow(x.data)==0 | is.null(x.data)){
    #return empty plot if no information is given
    out_plot=NULL
  }
  else{
    #generate plot
    thresh_bonferroni=0.05/2652/56
    #cut_at=x.data$info$cut_at
    my_data = x.data %>% 
      mutate(metName=Metabolite,
             x=foldchange,
             y=-log10(pvalue),
             text=paste0(
               "<b>", Metabolite, "</b><br>",
               "Platform: ", platform_name,"<br>",
               "Fluid: ", Fluid,"<br>",
               "Time point: ", baseline , " vs. ", timepoint, "<br>",
               "Log2 fc: ", round(foldchange,2), "<br>",
               "p Value: ", signif(pvalue, 2), "<br>"
             ))
    if(type_thresh=="bonferroni"){
      subtitle=paste0("Data points with p Value>" ,signif(thresh_bonferroni,2), " (bonferroni adjusted) were excluded due to performance issues")
    }
    else if(type_thresh=="fdr"){
      subtitle="Data points with q Value>0.05 were excluded due to performance issues"
    }
    else{
      subtitle="no cut"
    }
    
    if(colorby=="super"){
      my_data$color=my_data$SUPER.PATHWAY
      color_pal <- setNames(options$network$info_super_pathway$color.background,options$network$info_super_pathway$SUPER.PATHWAY)#setNames(my_data$super.color, my_data$SUPER.PATHWAY)
      #colorscale=NULL
    }
    else if(colorby=="platform"){
      my_data$color=my_data$platform_name
      this_color_option <- data.frame(
        platform = c("metabolon hd4 [nt-ms]","Biocrates p150 [t-ms]","In-house biochemistry [chem.]","numares (Lipofit) [NMR]","Lipidyzer [nt-ms]","In-house PTR-MS [PTRMS]", "In-house FTICR-MS [ICR]","NA" ),
        color= c("#E41A1C","#377EB8","#4DAF4A","#984EA3","#FF7F00","#A65628","#F781BF","#bfbfbf"),
        stringsAsFactors = F
      )
      color_pal <- setNames(this_color_option$platform,this_color_option$color)#setNames(data$platform.color, data$Platform)
      #colorscale=NULL
    }
    else{
      color_pal = NULL
      colorscale=NULL
    }
    
    if(!is.null(subtitle)){
      out_plot=plot_ly( 
        data=my_data,                 
        x=~x,
        y=~y,
        text=~text,
        #colorscale=colorscale,
        color=~color,
        colors=color_pal,
        opacity=0.8,
        type="scatter",
        hoverinfo = "text",
        mode="markers")%>%
        layout(
          xaxis = list(title="log2 fold change"), 
          yaxis = list(title="-log10 P value"),
          shapes = list(
            type = "line", 
            x0 = 0, 
            x1 = 1, 
            xref = "paper",
            y0 = -log10(thresh_bonferroni), 
            y1 = -log10(thresh_bonferroni), 
            line = list(color = "blue", dash="dash", name="bonferroni adjusted threshold")
          ),
          legend = list(orientation = 'h')
        )%>% htmlwidgets::onRender("function(el, x) {
                      Plotly.d3.select('.cursor-crosshair').style('cursor', 'default')
    }
                                   ")%>%
        config(displaylogo = FALSE, modeBarButtonsToRemove = c("sendDataToCloud", "zoomOut2d","zoomIn2d","toggleSpikelines","autoScale2d","resetScale2d","hoverClosestCartesian","hoverCompareCartesian","select2d","lasso2d"))
    }
  }
  return(out_plot)
}