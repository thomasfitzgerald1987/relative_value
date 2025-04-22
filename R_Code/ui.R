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


ui <- navbarPage(
  title = "navbarPage_TEST",
  
  tabPanel(title = "Tab 1",
    dashboardPage(
             #Header
             dashboardHeader(title = "Variable Selector"),
             #Sidebar
             dashboardSidebar(
               #style = "background: black",
                wellPanel(
                  style = "background: grey",
                  uiOutput("base_var_Selector",selected='ground_beef'),
                  uiOutput("base_var_unit_Selector"),
                ),
                wellPanel(
                  style = "background: blue",
                uiOutput("var_1_Selector"),
                uiOutput("var_1_unit_Selector"),
                ),
                wellPanel(
                  style = "background: orange",
                uiOutput("var_2_Selector"),
                uiOutput("var_2_unit_Selector"),
                ),
                wellPanel(
                  style = "background: green",
                uiOutput("var_3_Selector"),
                uiOutput("var_3_unit_Selector")
                )
              ),
             #Body
             dashboardBody(

               fluidRow(
                 height = 8,
                   box(width=12,
                       status="info",
                       #title="Cause of Death 1",
                       solidHeader=TRUE,
                       plotlyOutput("myplot1",width='auto',height='auto')
                   )),
               fluidRow(
                 height = 4,
                 box(
                   #title = "Base Variable", 
                   #solidHeader = FALSE, 
                   #status = "primary", 
                   style='background:grey',
                   width = 3, 
                   textOutput("text1")),
                 # box("Text",br(),
                 #     TextOutput(text1),
                 #     br(),"more text.",
                 #     style = 'background:grey',width=3),
                 box(style = 'background:blue', width=3,textOutput("text2")),
                 box(style = 'background:orange', width=3,textOutput("text3")),
                 box(style = 'background:green', width=3,textOutput("text4"))
               )
             )
)#/dashboardPage 1

),#/tabPanel 1
tabPanel(title = "Raw Data",
         dashboardPage(
           #Header
           dashboardHeader(title = "dashboardHeader2"),
           #Sidebar
           dashboardSidebar(
             title = "dashboardSideBar2"),
           #Body
           dashboardBody(dataTableOutput('table_2')),
)#/dashboardPage 2
),#/tabPanel2
tabPanel(title = "Sources",
         dashboardHeader(title = "Sources"),
         dashboardBody(
           fluidRow(
             box(title = "US Bureau of Labor Statistics"
                 )
           )))
)#/navBarPage






