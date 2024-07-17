# header elements
ui_elements=list()

# study_design ####
ui_elements[["study_design"]]<-tagList(
  div(style="width:100%;height:40px;overflow:hidden;",
      sliderTextInput(inputId = "study_slider", label=NULL,hide_min_max=T,grid=F,width="100%",force_edges=T,choices=as.character(info_sample$challengeTime),
                      selected=as.character(info_sample$challengeTime[c(1,length(info_sample$challengeTime))])
      )
  ),
  #tags$span("Participants fasted for 36h from day 0 / 7 PM till day 2 / 8 AM. During the fasting period, subjects received 2.7 L of mineral water according to a predefined drinking schedule.",
  #class="sd-info_text"),
  #div(style="display:block;float:left;padding:0 10px;width:100%",
  #    actionButton(inputId = "study_btn_fasting", label="Fasting", style="width:15.1%;float:left;", class="btn-study_design"),
  #    actionButton(inputId = "study_btn_sldr", label="SLD r",style="width:6.6%;float:left;",class="btn-study_design"),
  #    actionButton(inputId = "study_btn_sld1", label="SLD 1",style="width:13.3%;float:left;",class="btn-study_design"),
  #    div(style="width:1.7%;float:left;"),
  #    actionButton(inputId = "study_btn_ogtt", label="OGTT",style="width:15%;float:left;",class="btn-study_design"),
  #    actionButton(inputId = "study_btn_sld2", label="SLD 2",style="width:6.6%;float:left;",class="btn-study_design"),
  #    actionButton(inputId = "study_btn_pat", label="PAT",style="width:11.7%;float:left;",class="btn-study_design"),
  #    div(style="width:1.7%;float:left;"),
  #    actionButton(inputId = "study_btn_ogtt", label="OLTT",style="width:16.6%;float:left;",class="btn-study_design"),
  #    actionButton(inputId = "study_btn_stress", label="Stress",style="width:11.6%;float:left;",class="btn-study_design")
  #),
  div(#all
    style="display:block;float:left;padding:0 10px;width:max-content",
    div(#block1
      style="display:block;float:left;padding:0;width:35.1%",
      actionButton(inputId = "study_btn_fasting", label="Fasting", style="width:43%;float:left;", class="btn-study_design"),
      actionButton(inputId = "study_btn_sldr", label="SLD r",style="width:19%;float:left;",class="btn-study_design"),
      actionButton(inputId = "study_btn_sld1", label="SLD 1",style="width:38%;float:left;",class="btn-study_design"),
      div(#day 1
        style="display:block;float:left;padding:0;width:38%",
        actionButton(inputId = "study_btn_day1", label="Day 1",style="width:100%",class="btn-study_design")
      ),
      div(#day 2
        style="display:block;float:left;padding:0;width:62%",
        actionButton(inputId = "study_btn_day2", label="Day 2",style="width:100%",class="btn-study_design")
      ),
      actionButton(inputId = "study_btn_block1", label="Block 1",style="width:100%",class="btn-study_design")
    ),
    
    div(style="display:block;float:left;width:1.7%;border-right:1px solid lightgray;height:inherit;background-color:grey;overflow:hidden;",
        tags$span("4w break", style="writing-mode:tb-rl;font-weight:600;margin-top:5px;color:white;")
    ),
    div(#block2
      style="display:block;float:left;padding:0;width:63.2%",
      actionButton(inputId = "study_btn_ogtt", label="OGTT",style="width:23.5%;float:left;",class="btn-study_design"),
      actionButton(inputId = "study_btn_sld2", label="SLD 2",style="width:10.5%;float:left;",class="btn-study_design"),
      actionButton(inputId = "study_btn_pat", label="PAT",style="width:18.6%;float:left;",class="btn-study_design"),
      div(style="width:1.7%;float:left;display:block;"," "),
      actionButton(inputId = "study_btn_oltt", label="OLTT",style="width:26.3%;float:left;margin-left:2.6%",class="btn-study_design"),
      actionButton(inputId = "study_btn_stress", label="Stress",style="width:18.5%;float:left;",class="btn-study_design"),
      div(#day 3
        style="display:block;float:left;padding:0;width:55.3%",
        actionButton(inputId = "study_btn_day3", label="Day 3",style="width:100%",class="btn-study_design")
      ),
      div(#day 4
        style="display:block;float:left;padding:0;width:44.7%",
        actionButton(inputId = "study_btn_day4", label="Day 4",style="width:100%",class="btn-study_design")
      ),
      actionButton(inputId = "study_btn_block2", label="Block 2",style="width:100%",class="btn-study_design")
    ),
    actionButton(inputId = "study_btn_all", label="Select all",style="width:100%;display:block;float:left",class="btn-study_design")
  ) #end buttons
)


# study_sum_anthropometry ####
ui_elements[["study_sum_anthropometry"]]<-data.frame(
  stringsAsFactors=F,
  Parameter=c("Age (yr)","Height (m)", "Weight (kg)","BMI (kg/m2)" ,"Fat mass (kg)","Fat free mass (kg)", "Waist circumference (cm)","Hip circumference (cm)","Heart rate (min -1)","Blood pressure, systolic (mmHg)","Blood pressure, diastolic (mmHg)"),
  `Mean ± SD` =c("27.8 ±  2.9", "1.8 ±  0.1", "77.5 ±  7.1", "23.1 ±  1.8", "14.4 ±  3.3", "59.5 ±   59", "80.5 ±  4.6", "90.1 ±  4.7", "62 ± 11.4", "118.8 ±  5.9", "81.9 ±  5.9"),
  `CV (%)` = c(10.7, 3.5, 9.1, 7.6, 23.1, 9.9, 5.7, 5.2, 18.4, 4.9, 7.3)
)

# study_sum_platforms ####
ui_elements[["study_sum_platforms"]]<-data.frame(
  stringsAsFactors = F,
  #Platform=c("Biocrates AbsoluteIDQ p150 (MS)","Lipofit (NMR)","Biochemistry","Metabolon (MS)","Lipidizer (MS)","Chenomx (NMR)","Metabolon (MS)","PTR-MS","FTICR-MS"),
  Platform=c("Biocrates p150 [t-ms]","numares (Lipofit) [NMR]","In-house biochemistry","Metabolon HD4 [nt-ms]", "Lipidyzer [nt-ms]","Chenomx [NMR]", "Metabolon HD4 [nt-ms]", "In-house PTR-MS [PTRMS]", "In-house FTICR-MS [ICR]"),
  Type=c("targeted MS", "NMR", "standard biochemistry","non-targeted MS","non-targeted MS","NMR","non-targeted MS", "Proton Transfer Reaction MS", "Fourier-transform ion cyclotorn resonance MS"),
  Medium=c("Plasma","Plasma","Plasma","Plasma","Plasma","Urine","Urine","Breath air","Breath condensate"),
  Subjects=c(15,15,15,15,7,15,15,11,5),
  Time=c(56,54,56,56,56,13,16,32,11),
  Metabolites=c(132,28,4,595,965,6,619,106,201),
  platform_code=c("[P,t-ms]", "[P, NMR]", "[P,chem.]","[P,nt-ms]",  "[P,lipidizer]", "[U, NMR]", "[U,nt-ms]", "[PA, PTRMS]","[BC,ICR]"),
  Platform_code=c("MS Biocrates p150","NMR","Biochemistry","non-targeted LCMS","Lipidizer","NMR","non-targeted LCMS","PTRMS","ICR-FT-MS")
)
ui_elements[["study_sum_platforms"]]$code=paste0(ui_elements[["study_sum_platforms"]]$Medium,"_",ui_elements[["study_sum_platforms"]]$Platform)

#ui_elements[["main_subject"]]=prettyCheckboxGroup(inputId="main_subject", icon=icon("circle"),shape="square",label="Subject", choices = 1:15, selected = 1:15,inline = F, width = "100%",status="danger")
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[1]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#000000;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[2]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#696969;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[3]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#d3d3d3;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[4]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#ffff00;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[5]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#8b4513;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[6]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#d2b48c;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[7]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#007000;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[8]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#00ff00;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[9]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#40e0d0;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[10]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#ff00ff;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[11]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#87ceeb;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[12]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#8a2be2;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[13]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#ff0000;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[14]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#0000cd;"
#ui_elements[["main_subject"]][["children"]][[2]][["children"]][[2]][[15]][[1]][["children"]][[2]][["children"]][[1]][["attribs"]]$style="color:#ffa500;"


# main_transformation
#ui_elements[["checkbox_main_trans"]]<-prettyRadioButtons(inputId = "main_trans", shape="square",outline=T, status="danger",label = NULL,selected = "zscore",inline = F,choices = list("Raw data (Run day normalized ion counts)"="raw","Z-scored data" = "zscore","Fold change"="fc"))
