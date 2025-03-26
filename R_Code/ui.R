library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(readr)
library(DT)
library(plotly)
library(tidyverse)
library(stringr)
library(lubridate)
library(RColorBrewer)

#Covid Dashboard UI
navbarPage(title = "Mortality from Covid & Comorbid Diseases",
           
  tabPanel("Seasonal Trends",
          dashboardPage(
            dashboardHeader(title = ""),
            #Sidebar
            dashboardSidebar(
              
              #Region Selector
              uiOutput("baseVarSelector"),
              
              uiOutput("var_1_Selector"),
              uiOutput("var_2_Selector"),
              uiOutput("var_3_Selector"),
              
              # checkboxInput(
              #   inputId="checkbox1",
              #   label="This Year",
              #   value=TRUE,
              # ),
              # checkboxInput(
              #   inputId="checkbox2",
              #   label="Last Year",
              #   value=FALSE,
              # ),
              # checkboxInput(
              #   inputId="checkbox3",
              #   label="Last Last Year",
              #   value=FALSE,
              # ),
              # 
              
              verbatimTextOutput = "The plot on the right is interactive: 
                you can hover over each point to see detailed information.",
              
              verbatimTextOutput("placeholder", placeholder = TRUE),
        
            ),#End Sidebar
            
            #Dashboard Body
            dashboardBody(
              fluidRow(
                box(width=11,
                    status="info",
                    #title="Cause of Death 1",
                    solidHeader=TRUE,
                    plotlyOutput("myplot1",width='auto',height='auto')
                ),
                valueBoxOutput("summary_header_1",width=1),
                valueBoxOutput("mortality_summary_1",width=1),
                valueBoxOutput("mortality_summary_2",width=1),
                valueBoxOutput("mortality_summary_3",width=1),
              ),
              
              fluidRow(
                box(width=11,
                    status="info",
                    #title="Cause of Death 2",
                    solidHeader=TRUE,
                    plotlyOutput("myplot2",width='auto',height='auto')
                ),
                valueBoxOutput("summary_header_2",width=1),
                valueBoxOutput("mortality_summary_4",width=1),
                valueBoxOutput("mortality_summary_5",width=1),
                valueBoxOutput("mortality_summary_6",width=1),
              )
            )#End Dashboard Body
          )),#End Panel 1
  
  # tabPanel("US Map",
  #          dashboardPage(
  #            dashboardHeader(title = ""),
  #            #Sidebar
  #            dashboardSidebar(
  #              #Year Selector
  #              uiOutput("mapYearSelector"),
  #              
  #              #Year Slider
  #              sliderInput("mapWeek",
  #                          "Week to display",
  #                          #Set Option
  #                          min = 1,
  #                          max = 52,
  #                          value=1,
  #                          sep="",
  #                          animate=animationOptions(interval=250,loop=TRUE)
  #              )
  #            ),#End Sidebar
  #            
  #            #Dashboard Body
  #            dashboardBody(
  #              fluidRow(
  #                box(width=12,
  #                    status="info",
  #                    title="US Covid Mortality per 100,000 persons, by State",
  #                    solidHeader=TRUE,
  #                    plotOutput("map_plot",width='auto',height='800px')
  #                )
  #              )
  #            )#End Dashboard Body
  #          )),#End Panel 2
  
  tabPanel("Raw Data",
          dataTableOutput('table_1')),#End Panel 3
  
  tabPanel("Raw Data 2",
           dataTableOutput('table_2'))#End Panel 4
           
)