library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(readr)
library(DT)
library(plotly)
library(ggplot2)
library(tidyverse)
library(stringr)
library(lubridate)
library(RColorBrewer)
#library(usmap)

#Functions
variable.unit.converter <- function(df,df.datadict,var.name,unit.type.new){
  #Retrieve information from data dictionary
  var1 <- df[var.name]
  var.type <- subset(df.datadict,name==var.name)$measurement_type
  unit.type <- subset(df.datadict,name==var.name)$measurement_unit
  
  #Check   
  #if (unit.type==unit.type.new) {return(var1)}
  
  if(var.type=='hourly_wage') {
    hours.var <- df[paste(var.name,'_hours',sep='')]
    
    if (unit.type.new == 'minute') {var1 <- var1/60} else 
      if (unit.type.new == 'hour') {var1 <- var1} else 
        if (unit.type.new == 'day') {var1 <- var1 * hours.var / 5} else 
          if (unit.type.new == 'week') {var1 <- var1 * hours.var} else 
            if (unit.type.new == 'month') {var1 <- var1 * hours.var * 4.3} else 
              if (unit.type.new == 'year') {var1 <- var1 * hours.var * 52} else 
              {var1 <- var1}
  }
  if (var.type == 'weight') {
    if (unit.type.new == 'ounce') {var1 <- var1/16} else
      if (unit.type.new == 'pound') {var1 <- var1} else
      {var1 <- var1}
  }
  if (var.type == 'volume') {
    if (unit.type.new == 'ounce') {var1 <- var1/128} else
      if (unit.type.new == 'gallon') {var1 <- var1} else
      {var1 <- var1}
  }
  return(var1)
}

create_df_display <- function(df,df.datadict,base_var.name,base_var.unit,var.names,var.units) {
  base_var <- variable.unit.converter(df,df.datadict,base_var.name,base_var.unit)
  df.display <- as.data.frame(df$year)
  
  for (i in 1:length(var.names)) {
    var <- variable.unit.converter(df,df.datadict,var.names[i],var.units[i])
    prop <- var/base_var
    df.display <- as.data.frame(cbind(df.display,prop))
  }
  
  colnames(df.display) <- c('year',var.names)
  df.display <- df.display %>% pivot_longer(cols=var.names)
  return(df.display)
}

#Main Value Data
dir <- 'C:\\Users\\Thoma\\OneDrive\\Desktop\\portoflio_project_1\\data'
df.raw <- read.csv(paste(dir,'main_data_file.csv',sep='\\'))
df.orig <- df.raw #df.raw used for table display.

#Data Dictionary
df.datadict <- read.csv(paste(dir,'data_dictionary.csv',sep='\\'))




# update.date <- df.raw$Data.As.Of[1]
# update.year <- as.integer(substr(update.date,7,10))
# last.year <- update.year-1
# last.last.year <- update.year-2

#df.raw<-read.csv('https://data.cdc.gov/api/views/muzy-jte6/rows.csv?accessType=DOWNLOAD')
# df.pop <- df.pop.raw %>% select(NAME,ESTIMATESBASE2020)
# colnames(df.pop)[2] <- 'population'
# colnames(df.pop)[1] <- 'Jurisdiction.of.Occurrence'

#Map Data
# US_states <- us_map()

# #This Year (TY) flag for df.orig
# TY.cut <- mdy(max(df.orig$Data.As.Of)) - days(365)
# for(i in 1:7){
#   if(TY.cut %in% as.Date(df.orig$Week.Ending.Date)){
#     break
#   } else {
#     TY.cut <- TY.cut + days(1)
#   }
# }

#Define Color Palette for Graphs
palette.main<-brewer.pal(n = 8, name = "Spectral")
ty.color<-palette.main[7]
ly.color<-palette.main[3]
lly.color<-palette.main[1]


#Set column names for drop-down.
friendly_disease_list<-c(
  "All_Causes",
  "All_Natural_Causes",
  "Septicemia",
  "Cancer",
  "Diabetes",
  "Alzheimers",
  "Influenza_and_Pneumonia",
  "Chronic_Lowe_Respiratory_Diseases",
  "Other_Respiratory_Diseases",
  "Nephritis(Kidney_Problems)",
  "Not_Elsewhere_Classified",
  "Heart_Diseases",
  "Cerebrovascular(Stroke,Aneurysm)",
  "Covid_19_Multiple_COD",
  "Covid_19_Underlying_COD"
)
# colnames(df.orig)[6:20]<-friendly_disease_list
# 
# #Define initial data frame
# df.orig <- df.orig %>% 
#   mutate(df.orig, ty_flag=Week.Ending.Date > TY.cut) %>% 
#   mutate(df.orig,disp=Covid_19_Underlying_COD)

# #Merge map data
# df.map<-df.orig %>% select(Jurisdiction.of.Occurrence,MMWR.Year,MMWR.Week,Covid_19_Underlying_COD)
# df.map<-merge(x=df.map,y=df.pop,by="Jurisdiction.of.Occurrence",all.x=TRUE)
# df.map$pop.prop.100k <- df.map$Covid_19_Underlying_COD / df.map$population
# df.map$pop.prop.100k <- df.map$pop.prop.100k*100000
# df.map$pop.prop.100k[is.na(df.map$pop.prop.100k)]<-0
# df.map$year.week<-paste(df.map$MMWR.Year,'.',df.map$MMWR.Week,sep='')
# df.map <- left_join(df.map, US_states, by=c("Jurisdiction.of.Occurrence"="full"))
# max.weekly<-ceiling(max(df.map$pop.prop,na.rm=TRUE))

# 
# #Set main dataframes to filter from
df.main.1 <- df.orig

#Set Drop-Down lists
variables.list <- as.data.frame(colnames(df.orig))
colnames(variables.list)<-c("vars")
variables.list <- variables.list %>% 
  subset(!vars %in% c('X','year')) %>%
  filter(!grepl('hours', vars))
variables.list

# #Set juristiction/cause of death lists
# region.list<-sort(unique(df.raw$Jurisdiction.of.Occurrence))
# region.list <- c('United States',region.list[region.list!='United States'])
# #cod_list is ordered for the user
# cod_list <- sort(friendly_disease_list)
# cod.list.1 <- c("Covid_19_Underlying_COD",cod_list[cod_list!="Covid_19_Underlying_COD"])
# cod.list.2 <- c("Covid_19_Underlying_COD",cod_list[cod_list!="Covid_19_Underlying_COD"])


shinyServer(function(input, output, session) {
  
  #Define default values for reactive variables.
  input.r <- reactiveValues(
    base_var.name.1.r = 'ground_beef',
    base_var.unit.1.r = 'unit',
    var.name.1.r = 'retail',
    var.unit.1.r = 'unit',
    var.name.2.r = 'financial',
    var.unit.2.r = 'unit',
    var.name.3.r = 'mining_logging_wages',
    var.unit.3.r = 'unit',
    
    var.names = c(var.name.1.r,var.name.2.r,var.name.3.r),
    var.units = c(var.unit.1.r,var.unit.2.r,var.unit.3.r),
    df.display = create_df_display(df.orig,df.datadict,base_var.name.1.r,base_var.unit.1.r,var.names,var.units)
  )
  
  #ObserveEvents Reactors
  
  #Reacts to UI element 'baseVarSelector'
  observeEvent(input$baseVar1, {
    input.r$base_var.name.1.r <- input$baseVar1
    print(paste('Base Variables changed, active base variable is now:',input.r$base_var.name.1.r))
  }) 
  
  #Reacts to UI element 'var_1_Selector'
  observeEvent(input$var.name.1.r, {
    input.r$var.name.1.r <- input$var.name.1.r
    print(paste('var1 changed, active var1 is now:',input.r$var.name.1.r))
  })
  #Reacts to UI element 'var_2_Selector'
  observeEvent(input$var.name.2.r, {
    input.r$var.name.2.r <- input$var.name.2.r
    print(paste('var2 changed, active var2 is now:',input.r$var.name.2.r))
  })  
  #Reacts to UI element 'var_3_Selector'
  observeEvent(input$var.name.3.r, {
    input.r$var.name.3.r <- input$var.name.3.r
    print(paste('var3 changed, active var3 is now:',input.r$var.name.3.r))
  })

  # #Reacts to UI element 'cod_2_Selector'
  # observeEvent(input$cod2, {
  #   input.r$cod_2.r <- input$cod2
  #   print(paste('cod_2 changed, active cod_2 is now:',input.r$cod_2.r))
  #   # input.r$df.ytd.2["disp"]<-input.r$df.ytd.1[input.r$cod_2.r]
  #   # input.r$df.ty.2["disp"]<-input.r$df.ty.1[input.r$cod_2.r]
  #   # input.r$df.ly.2["disp"]<-input.r$df.ly.1[input.r$cod_2.r]
  #   # input.r$df.lly.2["disp"]<-input.r$df.lly.1[input.r$cod_2.r]    
  #   # input.r$current.year.total.2 = sum(input.r$df.ytd.2["disp"],na.rm=TRUE)
  #   # input.r$last.year.total.2 = sum(input.r$df.ly.2["disp"],na.rm=TRUE)
  #   # input.r$last.last.year.total.2 = sum(input.r$df.lly.2["disp"],na.rm=TRUE)
  # })
  
  # #Year Checkboxes (Plot #1)
  # observeEvent(input$checkbox1, {
  #   if(input$checkbox1) input.r$view.r[1]<-TRUE
  #   if(!input$checkbox1) input.r$view.r[1]<-FALSE
  #   #print(paste("Checkbox 1 set to:",input$checkbox1,sep=""))
  # })
  # observeEvent(input$checkbox2, {
  #   if(input$checkbox2) input.r$view.r[2]<-TRUE
  #   if(!input$checkbox2) input.r$view.r[2]<-FALSE
  #   #print(paste("Checkbox 2 set to:",input$checkbox2,sep=""))
  # })


  #Selectors#
  
  #Base Variable Selector
  output$baseVarSelector <- renderUI({
    selectInput(
      inputId = "baseVar1",
      label = "Select a base Variable, or -default- to use USD:",
      choices = variables.list
    )
  })
  
  #First Variable
  output$var_1_Selector <- renderUI({
    selectInput(
      inputId = "var.name.1.r",
      label = "Select a variable to compare",
      choices = variables.list
    )
  })
  #Second Variable
  output$var_2_Selector <- renderUI({
    selectInput(
      inputId = "var.name.2.r",
      label = "Select a variable to compare",
      choices = variables.list
    )
  })
  #Third Variable
  output$var_3_Selector <- renderUI({
    selectInput(
      inputId = "var.name.3.r",
      label = "Select a variable to compare",
      choices = variables.list
    )
  })
  
  
  # #Second Cause of Death Selector
  # output$cod_2_Selector <- renderUI({
  #   selectInput(
  #     inputId = "cod2",
  #     label = "Select Cause of Death 2 (bottom graph):",
  #     choices = cod.list.2
  #   )
  # })
  
  #Summary Boxes#
  
  #Summary1#
  # output$mortality_summary_1 <- renderValueBox({
  #   valueBox(
  #     value=tags$p(input.r$current.year.total.1,style = "font-size: 50%;"),
  #     subtitle=paste("(",update.year,")",sep=""),
  #     color = "green"
  #   )
  # })
  # 
  # output$mortality_summary_2 <- renderValueBox({
  #   valueBox(
  #     value=tags$p(input.r$last.year.total.1,style = "font-size: 50%;"),
  #     subtitle=paste("(",last.year,")",sep=""),
  #     color = "orange"
  #   )
  # })  

  # output$cod_1<-renderValueBox({
  #   valueBox(
  #     value=tags$p(input.r$cod_1.r,style = "font-size: 50%;"),
  #     color = "aqua"
  #   )
  # })
  # output$cod_2<-renderValueBox({
  #   valueBox(
  #     value=tags$p(input.r$cod_1.r,style = "font-size: 50%;"),
  #     color = "aqua"
  #   )
  # })
  # 
  # output$summary_header_1 <- renderValueBox({
  #   valueBox(value="",
  #     subtitle="Annual Totals",
  #     color = "black"
  #   )
  # })

  
  #Plotting Functions
  # output$myplot1 <- renderPlotly({
  #   #PLOT #1 - TOP PLOT
  #   view.type <- input.r$view.r
    
    # #Color Settings
    # if(input.r$region.1.r=="United States"){
    #   if(input.r$cod_1.r %in% c("All_Causes","All_Natural_Causes")){
    #     y.top<-100000
    #   } else {
    #     y.top<-25000
    #   }
    # } else {
    #   if(input.r$cod_1.r %in% c("All_Causes","All_Natural_Causes")){
    #     y.top<-25000
    #   } else {
    #     y.top<-5000
    #   }
    # }
    
    #Plot frame
  #   myplot <- plot_ly(data=input.r$df.main.1,
  #                     x=~MMWR.Week,
  #                     mode = 'markers') %>%
  #     layout(
  #       title = paste(input.r$region.1.r,': ',input.r$cod_1.r,sep=""),
  #       xaxis = list(title="Month",
  #                    range=c(1,52),
  #                    showgrid = TRUE,
  #                    ticktext = c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec"),
  #                    tickvals = c(1,5,9,14,18,22,27,31,35,40,45,49)
  #                    ),
  #       yaxis = list(title="Mortality",range=c(0,y.top)),
  #       showlegend = TRUE,
  #       legend = list(
  #         title=list(text="Year", font=list(color="black", size=15)),
  #         orientation = 'h',
  #         xanchor = "center",
  #         x = .5,
  #         y = -.2
  #       )
  #     )
  #   
  #   #ytd
  #   if(view.type[1]){
  #     myplot <- myplot %>%
  #       add_trace(data=input.r$df.ytd.1,type="scatter",
  #                 x=~MMWR.Week,y=~disp,color="Year-to-Date",
  #                 mode = 'lines+markers',
  #                   marker=list(color=ty.color),
  #                   line=list(color=ty.color),
  #                 symbol= ~I('circle-dot'),
  #                 opacity = 1,
  #                 #labels="This Year",
  #                 text = paste0("Where: ", input.r$df.ytd.1$Jurisdiction.of.Occurrence, "<br>"
  #                               ,"Week: ", input.r$df.ytd.1$MMWR.Week, "<br>"
  #                               ,"Week Ending: ", input.r$df.ytd.1$Week.Ending.Date, "<br>"
  #                               #,"Mortality: %{input.r$df.ytd.1$disp,}"),
  #                               ,"Mortality: ", input.r$df.ytd.1$disp),
  #                 hoverinfo = 'text')
  #   }
  #   #ty
  #   if(view.type[1]){
  #     myplot <- myplot %>%
  #       add_trace(data=input.r$df.ty.1,type="scatter",
  #                 x=~MMWR.Week,y=~disp,color="Last 12 Months",
  #                 mode = 'lines+markers',
  #                 marker=list(color=ty.color),
  #                 line=list(color=ty.color),
  #                 symbol= ~I('circle-dot'),
  #                 opacity = 1,
  #                 text = paste0("Where: ", input.r$df.ty.1$Jurisdiction.of.Occurrence, "<br>"
  #                               ,"Week: ", input.r$df.ty.1$MMWR.Week, "<br>"
  #                               ,"Week Ending: ", input.r$df.ty.1$Week.Ending.Date, "<br>"
  #                               ,"Mortality: ", input.r$df.ty.1$disp),
  #                 hoverinfo = 'text')
  #   }
  #   #ly
  #   if(view.type[2]){
  #     myplot <- myplot %>%
  #       add_trace(data=input.r$df.ly.1,type="scatter",
  #                 x=~MMWR.Week,y=~disp,color=as.character(last.year),
  #                 mode = 'lines+markers',
  #                 marker=list(color=ly.color),
  #                 line=list(color=ly.color),
  #                 symbol= ~I('circle-dot'),
  #                 opacity = 1,
  #                 text = paste0("Where: ", input.r$df.ly.1$Jurisdiction.of.Occurrence, "<br>"
  #                               ,"Week: ", input.r$df.ly.1$MMWR.Week, "<br>"
  #                               ,"Week Ending: ", input.r$df.ly.1$Week.Ending.Date, "<br>"
  #                               ,"Mortality: ", input.r$df.ly.1$disp),
  #                 hoverinfo = 'text')
  #   }
  #   #lly
  #   if(view.type[3]){
  #   myplot <- myplot %>%
  #     add_trace(data=input.r$df.lly.1,type="scatter",
  #               x=~MMWR.Week,y=~disp,color=as.character(last.last.year),
  #               mode = 'lines+markers',
  #               marker=list(color=lly.color),
  #               line=list(color=lly.color),
  #               symbol= ~I('circle-dot'),
  #               opacity = 1,
  #               text = paste0("Where: ", input.r$df.lly.1$Jurisdiction.of.Occurrence, "<br>"
  #                             ,"Week: ", input.r$df.lly.1$MMWR.Week, "<br>"
  #                             ,"Week Ending: ", input.r$df.lly.1$Week.Ending.Date, "<br>"
  #                             ,"Mortality: ", input.r$df.lly.1$disp),
  #               hoverinfo = 'text')
  #   }
  #   
  #   return(myplot)
  #   
  # })
  
  # output$myplot2 <- renderPlotly({
  #   #PLOT #2 - BOTTOM PLOT
  #   view.type <- input.r$view.r
  #   
  #   #Color Settings
  #   if(input.r$region.2.r=="United States"){
  #     if(input.r$cod_2.r %in% c("All_Causes","All_Natural_Causes")){
  #       y.top<-100000
  #     } else {
  #       y.top<-25000
  #     }
  #   } else {
  #     if(input.r$cod_2.r %in% c("All_Causes","All_Natural_Causes")){
  #       y.top<-25000
  #     } else {
  #       y.top<-5000
  #     }
  #   }
  #   
  #   #Plot frame
  #   myplot <- plot_ly(data=input.r$df.main.1,
  #                     x=~MMWR.Week,
  #                     mode = 'markers') %>%
  #     layout(
  #       title = paste(input.r$region.2.r,': ',input.r$cod_2.r,sep=""),
  #       xaxis = list(title="Month",
  #                    range=c(1,52),
  #                    showgrid = TRUE,
  #                    ticktext = c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sep","Oct","Nov","Dec"),
  #                    tickvals = c(1,5,9,14,18,22,27,31,35,40,45,49)
  #       ),
  #       yaxis = list(title="Mortality",range=c(0,y.top)),
  #       showlegend = TRUE,
  #       legend = list(
  #         title=list(text="Year", font=list(color="black", size=15)),
  #         orientation = 'h',
  #         xanchor = "center",
  #         x = .5,
  #         y = -.2
  #       )
  #     )
  #   
  #   #ytd
  #   if(view.type[4]){
  #     myplot <- myplot %>%
  #       add_trace(data=input.r$df.ytd.2,type="scatter",
  #                 x=~MMWR.Week,y=~disp,color="Year-to-Date",
  #                 mode = 'lines+markers',
  #                 marker=list(color=ty.color),
  #                 line=list(color=ty.color),
  #                 symbol= ~I('circle-dot'),
  #                 opacity = 1,
  #                 #labels="This Year",
  #                 text = paste0("Where: ", input.r$df.ytd.1$Jurisdiction.of.Occurrence, "<br>"
  #                               ,"Week: ", input.r$df.ytd.1$MMWR.Week, "<br>"
  #                               ,"Week Ending: ", input.r$df.ytd.1$Week.Ending.Date, "<br>"
  #                               ,"Mortality: ", input.r$df.ytd.2$disp),
  #                 hoverinfo = 'text')
  #   }
  #   #ty
  #   if(view.type[4]){
  #     myplot <- myplot %>%
  #       add_trace(data=input.r$df.ty.2,type="scatter",
  #                 x=~MMWR.Week,y=~disp,color="Last 12 Months",
  #                 mode = 'lines+markers',
  #                 marker=list(color=ty.color),
  #                 line=list(color=ty.color),
  #                 symbol= ~I('circle-dot'),
  #                 opacity = 1,
  #                 text = paste0("Where: ", input.r$df.ty.1$Jurisdiction.of.Occurrence, "<br>"
  #                               ,"Week: ", input.r$df.ty.1$MMWR.Week, "<br>"
  #                               ,"Week Ending: ", input.r$df.ty.1$Week.Ending.Date, "<br>"
  #                               ,"Mortality: ", input.r$df.ty.2$disp),
  #                 hoverinfo = 'text')
  #   }
  #   #ly
  #   if(view.type[5]){
  #     myplot <- myplot %>%
  #       add_trace(data=input.r$df.ly.2,type="scatter",
  #                 x=~MMWR.Week,y=~disp,color=as.character(last.year),
  #                 mode = 'lines+markers',
  #                 marker=list(color=ly.color),
  #                 line=list(color=ly.color),
  #                 symbol= ~I('circle-dot'),
  #                 opacity = 1,
  #                 text = paste0("Where: ", input.r$df.ly.1$Jurisdiction.of.Occurrence, "<br>"
  #                               ,"Week: ", input.r$df.ly.1$MMWR.Week, "<br>"
  #                               ,"Week Ending: ", input.r$df.ly.1$Week.Ending.Date, "<br>"
  #                               ,"Mortality: ", input.r$df.ly.2$disp),
  #                 hoverinfo = 'text')
  #   }
  #   #lly
  #   if(view.type[6]){
  #     myplot <- myplot %>%
  #       add_trace(data=input.r$df.lly.2,type="scatter",
  #                 x=~MMWR.Week,y=~disp,color=as.character(last.last.year),
  #                 mode = 'lines+markers',
  #                 marker=list(color=lly.color),
  #                 line=list(color=lly.color),
  #                 symbol= ~I('circle-dot'),
  #                 opacity = 1,
  #                 text = paste0("Where: ", input.r$df.lly.1$Jurisdiction.of.Occurrence, "<br>"
  #                               ,"Week: ", input.r$df.lly.1$MMWR.Week, "<br>"
  #                               ,"Week Ending: ", input.r$df.lly.1$Week.Ending.Date, "<br>"
  #                               ,"Mortality: ", input.r$df.lly.2$disp),
  #                 hoverinfo = 'text')
  #   }
  #   
  #   return(myplot)
  #   
  # })
  
  # #Section Below is for Map Tab
  # output$mapYearSelector <- renderUI({
  #   selectInput(
  #     inputId = "mapYear",
  #     label = "Select a Year:",
  #     choices = c(last.last.year,last.year,update.year)
  #   )
  # })
  # 
  # observeEvent(input$mapYear, {
  #   input.r$map.year.r <- input$mapYear
  #   #print(paste('Map Year changed, active year is now:',input.r$map.year.r))
  #   input.r$df.map.active<-subset(input.r$df.map,MMWR.Year==input.r$map.year.r & MMWR.Week==input.r$map.week.r)
  # })
  # 
  # #Reacts to sliderInput 'mapWeek'
  # observeEvent(input$mapWeek, {
  #   input.r$map.week.r <- input$mapWeek
  #   #print(paste('Map Week changed, active week is now:',input.r$map.week.r))
  #   input.r$df.map.active<-subset(input.r$df.map,MMWR.Year==input.r$map.year.r & MMWR.Week==input.r$map.week.r)
  # })
  # 
  # output$map_plot <- renderPlot({
  #   plot<-ggplot(input.r$df.map.active, aes(x=x,
  #                                   y=y,
  #                                   group=group,
  #                                   fill=pop.prop.100k)) +
  #     geom_polygon(color="black", size=0.5) +
  #     theme_minimal() + scale_fill_gradient(name = "Deaths per 100,000",low="darkblue", high="red",limits = c(0,max.weekly))
  #   plot<-plot + theme(axis.line=element_blank(),axis.text.x=element_blank(),
  #                      axis.text.y=element_blank(),axis.ticks=element_blank(),
  #                      axis.title.x=element_blank(),
  #                      axis.title.y=element_blank(),
  #                      legend.position="bottom",
  #                      panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
  #                      panel.grid.minor=element_blank(),plot.background=element_blank())
  #   return(plot)
  # })
  
  #Raw Data Tables Tabs
  output$table_1 <- renderDataTable(df.raw)
  output$table_2 <- renderDataTable(df.raw)
})