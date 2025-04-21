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
variable.unit.options <- function(var.name,df.datadict){
  unit.type <- subset(df.datadict, var_name==var.name)$measurement_type
  #print(unit.type)
  if(length(unit.type)==0 || unit.type=='None'){return('none')}
  if(unit.type=='currency'){output <- 'unit'}
  if(unit.type=='hourly_wage'){output <- c('hour','minute','day','week','month','year')}
  if(unit.type=='weight'){output <- c('pound','ounce')}
  if(unit.type=='volume'){output <- c('gallon','ounce')}
  if(unit.type=='other'){output <- 'unit'}
  return(output)
}

variable.unit.converter <- function(df,df.datadict,var.name,unit.type.new){
  #Retrieve information from data dictionary
  var1 <- df[var.name]
  var.type <- subset(df.datadict,var_name==var.name)$measurement_type
  unit.type <- subset(df.datadict,var_name==var.name)$measurement_unit
  
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
  #print(paste('base_var_scatterplot:', var.names,var.units,sep=' '))
  var.names.raw <- var.names
  var.names <- var.names[var.names!='None']
  base_var <- variable.unit.converter(df,df.datadict,base_var.name,base_var.unit)

  df.display <- as.data.frame(df$year)
    
  for (i in 1:length(var.names)) {
    var <- variable.unit.converter(df,df.datadict,var.names[i],var.units[i])
    prop <- var/base_var
    df.display <- as.data.frame(cbind(df.display,prop))
  }
  
  colnames(df.display) <- c('year',var.names)
  df.display <- df.display %>% pivot_longer(cols=var.names)
  color_map <- setNames(c("blue", "orange", "darkgreen"), var.names.raw)
  
  myplot <- suppressMessages(plotly_build(plot_ly(
    data=df.display,
    x=~year,
    y=~value,
    split=~name,
    color=~name,
    colors=color_map,
    mode='lines+markers') %>%
    layout(legend=list(title=list(text='Variables')),
           xaxis = list(title = 'Year'),
           yaxis = list(title = paste(base_var.name, paste('(', base_var.unit,')',sep=''),sep=' ')))))
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

# #Set main dataframes to filter from
df.main.1 <- df.orig

#Set Drop-Down lists
variables.list <- as.data.frame(colnames(df.orig))
#units.list <- c('unit','minute','hour','day','week','month','year','ounce','pound','oun')
colnames(variables.list)<-c("All")
variables.list <- variables.list %>% 
  subset(!All %in% c('X','year')) %>%
  filter(!grepl('hours', All))

var.categories <- df.datadict %>% dplyr::select(var_category,var_name)
commodities.list <- variables.list %>% 
  subset(All %in% subset(var.categories,var_category=='Commodity')$var_name) %>%
  rename(Commodities = All)
employment.list <- variables.list %>% 
  subset(All %in% subset(var.categories,var_category=='Employment')$var_name) %>%
  rename(Employment = All)
currency.list <- variables.list %>% 
  subset(All %in% subset(var.categories,var_category=='Currency')$var_name) %>%
  rename(Currency = All)




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

  #Untested
  # observeEvent(input.r$var.name.1.r, {
  #   input.lists$var.names <- input.lists$var.names
  #   print(paste('Variable Changed changed, var.names is now:',input.lists$var.names))
  #     })
  
  #BaseVar
  output$base_var_Selector <- renderUI({
    selectInput(
      inputId = "base_var.name.1.r",
      label = "Base Variable:",
      choices = c(currency.list,commodities.list,employment.list))
  })
  observeEvent(input$base_var.name.1.r, {
    input.r$base_var.name.1.r <- input$base_var.name.1.r
    #print(paste('base_var changed, active base_var is now:',input.r$base_var.name.1.r))
    })
  #Base_Var_Unit
  output$base_var_unit_Selector <- renderUI({
    selectInput(
      inputId = "base_var.unit.1.r",
      label = "Unit of Measurement:",
      choices = variable.unit.options(input.r$base_var.name.1.r,df.datadict))})
  observeEvent(input$base_var.unit.1.r, {
    input.r$base_var.unit.1.r <- input$base_var.unit.1.r
    #print(paste('Base Var unit changed, active base var unit is now:',input.r$base_var.unit.1.r))
    })
  
  #Base_Var_Information
  output$baseVarName <- renderUI({
    textOutput("It's me.  Hi.  I'm the problem, it's me.")
  })
  observeEvent(input$base_var.name.1.r, {
    input.r$base_var.name.1.r <- input$base_var.name.1.r
    #print(paste('base_var changed, active base_var is now:',input.r$base_var.name.1.r))
  })
  
  #Variable 1
    #Name
    output$var_1_Selector <- renderUI({
      selectInput(
        inputId = "var.name.1.r",
        label = "Variable 1:",
        choices = c(commodities.list,employment.list,currency.list))
      })
    
    observeEvent(input$var.name.1.r, {
      input.r$var.name.1.r <- input$var.name.1.r
      #print(paste('var1 changed, active var1 is now:',input.r$var.name.1.r))
      })
    
    #Unit
    output$var_1_unit_Selector <- renderUI({
      selectInput(
        inputId = "var.unit.1.r",
        label = "Units:",
        choices = variable.unit.options(input.r$var.name.1.r,df.datadict))})
    
    observeEvent(input$var.unit.1.r, {
      input.r$var.unit.1.r <- input$var.unit.1.r
      #print(paste('var1 unit changed, active var1 unit is now:',input.r$var.unit.1.r))
      })
  
  #Variable 2
    #Name
    output$var_2_Selector <- renderUI({
      selectInput(
        inputId = "var.name.2.r",
        label = "Variable 2:",
        choices = c('None',commodities.list,employment.list,currency.list))
    })
    observeEvent(input$var.name.2.r, {
      input.r$var.name.2.r <- input$var.name.2.r
      #print(paste('var2 changed, active var2 is now:',input.r$var.name.2.r))
      })  
    #Unit
    output$var_2_unit_Selector <- renderUI({
      selectInput(
        inputId = "var.unit.2.r",
        label = "Units:",
        choices = variable.unit.options(input.r$var.name.2.r,df.datadict))})
    
    observeEvent(input$var.unit.2.r, {
      input.r$var.unit.2.r <- input$var.unit.2.r
      #print(paste('var2 unit changed, active var2 unit is now:',input.r$var.unit.2.r))
      })
    
  #Variable 3
  #Name
  output$var_3_Selector <- renderUI({
    selectInput(
      inputId = "var.name.3.r",
      label = "Variable 3:",
      choices = c('None',commodities.list,employment.list,currency.list))
  })
  observeEvent(input$var.name.3.r, {
    input.r$var.name.3.r <- input$var.name.3.r
    #print(paste('var3 changed, active var3 is now:',input.r$var.name.3.r))
    })
  #Unit
  output$var_3_unit_Selector <- renderUI({
    selectInput(
      inputId = "var.unit.3.r",
      label = "Units",
      choices = variable.unit.options(input.r$var.name.3.r,df.datadict))})
  
  observeEvent(input$var.unit.3.r, {
    input.r$var.unit.3.r <- input$var.unit.3.r
    #print(paste('var3 unit changed, active var3 unit is now:',input.r$var.unit.3.r))
    })
  
  
  
  
  #Plots
  
  output$myplot1 <- renderPlotly({
    #print(paste('df.display:',dim(df.orig),sep=' '))
    #print(paste('df.datadict:',dim(df.datadict),sep=' '))
    #print(paste('base_var.name:',input.r$base_var.name.1.r,sep=' '))
    #print(paste('base_var.unit:',input.r$base_var.unit.1.r),sep=' ')
    #print(paste('var.names:',length(input.lists$var.names),sep=' '))
    #print(paste('var.units:',length(input.lists$var.units),sep=' '))
    
    #function(df,df.datadict,base_var.name,base_var.unit,var.names,var.units)
    myplot <- base_var_scatterplot(df.orig,
                                   df.datadict,
                                   input.r$base_var.name.1.r,input.r$base_var.unit.1.r,
                                   c(input.r$var.name.1.r,input.r$var.name.2.r,input.r$var.name.3.r),
                                   c(input.r$var.unit.1.r,input.r$var.unit.2.r,input.r$var.unit.3.r))})
  
  output$text1 <- renderText(subset(df.datadict,var_name==input.r$base_var.name.1.r)$Description)
  output$text2 <- renderText(subset(df.datadict,var_name==input.r$var.name.1.r)$Description)
  output$text3 <- renderText(subset(df.datadict,var_name==input.r$var.name.2.r)$Description)
  output$text4 <- renderText(subset(df.datadict,var_name==input.r$var.name.3.r)$Description)
  
  output$table_1 <- renderDataTable(non_max_na_filter(input.df$df.display))
  output$table_2 <- renderDataTable(df.orig)
  
  
  
})#/shinyServer