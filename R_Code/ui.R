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
  title = "navbarPage",
  
  tabPanel(title = "Tab 1",
    dashboardPage(
             #Header
             dashboardHeader(title = "dashboardHeader"),
             #Sidebar
             dashboardSidebar(
                title = "dashboardSideBar",
                uiOutput("base_var_Selector",selected='ground_beef'),
                uiOutput("var_1_Selector"),
                uiOutput("var_2_Selector"),
                uiOutput("var_3_Selector")
                ),
             #Body
             dashboardBody(
               fluidRow(
                   box(width=11,
                       status="info",
                       #title="Cause of Death 1",
                       solidHeader=TRUE,
                       plotlyOutput("myplot1",width='auto',height='auto')
                   )),
               fluidRow(
                 box(width=11,
                     status="info",
                     solidHeader=TRUE,
                     dataTableOutput('table_2'))
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
           dashboardBody(dataTableOutput('table_1'))
)#/dashboardPage 2
)#/tabPanel2

)#/navBarPage






