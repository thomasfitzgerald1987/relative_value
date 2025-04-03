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

#Functions######################################################################
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

non_max_na_filter <- function(df){
  #Returns df as only rows with non-max NA.  So, if the most NA columns in any row is 10, removes all rows with 10 NAs.
  return(df[which(rowSums(is.na(df)) %in% rowSums(is.na(df))[rowSums(is.na(df))!=max(rowSums(is.na(df)))]),])
}

base_var_scatterplot <- function(df,df.datadict,base_var.name,base_var.unit,var.names,var.units) {
  base_var <- variable.unit.converter(df,df.datadict,base_var.name,base_var.unit)
  df.display <- as.data.frame(df$year)
  
  for (i in 1:length(var.names)) {
    var <- variable.unit.converter(df,df.datadict,var.names[i],var.units[i])
    prop <- var/base_var
    df.display <- as.data.frame(cbind(df.display,prop))
  }
  
  colnames(df.display) <- c('year',var.names)
  df.display <- df.display %>% pivot_longer(cols=var.names)
  
  myplot <- plot_ly(data=df.display,
          x=~year,mode='lines+markers', split= ~name) %>%
    add_trace(y = ~value) %>%
    layout(legend=list(title=list(text='Variables')),
           xaxis = list(title = 'Year'),
           yaxis = list(title = 'Gallons of Gasoline per Hour'))
  return(myplot)
}

#Data###########################################################################
#Main Value Data
dir <- 'C:\\Users\\Thoma\\OneDrive\\Desktop\\portoflio_project_1\\data'
df.raw <- read.csv(paste(dir,'main_data_file.csv',sep='\\'))
df.orig <- non_max_na_filter(df.raw) #df.raw used for table display.

#Data Dictionary
df.datadict <- read.csv(paste(dir,'data_dictionary.csv',sep='\\'))

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
  "Covid_19_Underlying_COD")

# #Set main dataframes to filter from
df.main.1 <- df.orig

#Set Drop-Down lists
variables.list <- as.data.frame(colnames(df.orig))
colnames(variables.list)<-c("vars")
variables.list <- variables.list %>% 
  subset(!vars %in% c('X','year')) %>%
  filter(!grepl('hours', vars))
variables.list

shinyServer(function(input, output, session) {
  #Define Reactive Variables
  input.r <- reactiveValues(
    base_var.name.1.r = 'ground_beef',
    base_var.unit.1.r = 'unit',
    var.name.1.r = 'retail',
    var.unit.1.r = 'unit',
    var.name.2.r = 'financial',
    var.unit.2.r = 'unit',
    var.name.3.r = 'mining_logging_wages',
    var.unit.3.r = 'unit')
  
  input.lists <- reactiveValues(
    var.names = c(isolate(input.r$var.name.1.r),
                  isolate(input.r$var.name.2.r),
                  isolate(input.r$var.name.3.r)),
    var.units = c(isolate(input.r$var.unit.1.r),
                  isolate(input.r$var.unit.2.r),
                  isolate(input.r$var.unit.3.r)))
  
  input.df <- reactiveValues(df.display = create_df_display(df.orig,df.datadict,
                                                            isolate(input.r$base_var.name.1.r),
                                                            isolate(input.r$base_var.unit.1.r),
                                                            isolate(input.lists$var.names),
                                                            isolate(input.lists$var.units)))
    
  
  #BaseVar
  output$base_var_Selector <- renderUI({
    selectInput(
      inputId = "base_var.name.1.r",
      label = "Select a base variable to compare against",
      choices = variables.list)
  })
  observeEvent(input$base_var.name.1.r, {
    input.r$var.name.1.r <- input$base_var.name.1.r
    print(paste('base_var changed, active base_var is now:',input.r$base_var.name.1.r))})
  
  #Variable 1
  output$var_1_Selector <- renderUI({
    selectInput(
      inputId = "var.name.1.r",
      label = "Select a variable to compare",
      choices = variables.list)
  })
  observeEvent(input$var.name.1.r, {
    input.r$var.name.1.r <- input$var.name.1.r
    print(paste('var1 changed, active var1 is now:',input.r$var.name.1.r))})
  
  #Variable 2
  output$var_2_Selector <- renderUI({
    selectInput(
      inputId = "var.name.2.r",
      label = "Select a variable to compare",
      choices = variables.list)
  })
  observeEvent(input$var.name.2.r, {
    input.r$var.name.2.r <- input$var.name.2.r
    print(paste('var2 changed, active var2 is now:',input.r$var.name.2.r))})  
  
  #Variable 3
  output$var_3_Selector <- renderUI({
    selectInput(
      inputId = "var.name.3.r",
      label = "Select a variable to compare",
      choices = variables.list)
  })
  observeEvent(input$var.name.3.r, {
    input.r$var.name.3.r <- input$var.name.3.r
    print(paste('var3 changed, active var3 is now:',input.r$var.name.3.r))})

  #Plots
  output$myplot1 <- renderPlotly({
    myplot <- base_var_scatterplot(input.df$df.display,df.datadict,
                                   input.r$base_var.name.1.r,input.r$base_var.unit.1.r,
                                   input.lists$var.names,
                                   input.lists$var.units)
  })
  
  output$table_1 <- renderDataTable(df.orig)
  output$table_2 <- renderDataTable(non_max_na_filter(input.df$df.display))
  
  
  
})#/shinyServer