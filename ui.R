#Load Required Libraries
library(shinydashboard)
library(caTools)
library(ggplot2)
library(gridExtra)
library(randomForest)
library(tidyverse)

#Create Dashboard
ui <- dashboardPage(
  #Add title in header
  dashboardHeader(title = "A System to Detect Credit Card Fraud", titleWidth = 400),
  
  #Create dashboard sidebar
  dashboardSidebar(
    sidebarMenu(
      #Menubar that links with sidebarPanel
      menuItem("Random Forest Algorithm", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  
  #Body of dashboard that displays items to the left of the GUI body.
  dashboardBody(
    tags$head(tags$style(HTML('.content-wrapper { height: 1590px; }'))),
    tabItems(
      # First tab content
      tabItem(tabName = "dashboard",
              sidebarPanel(
                #Input for practitoners to upload datasets
                fileInput("file1", "Choose Transaction Dataset (CSV File)",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
              ),
              
              #Main panel of the dashboard that displays items to the right of the GUI body from the server.
              mainPanel(
                
                h1("Random Forest Plot"),
                plotOutput("randomForestPlot"),
                textOutput("exploringData"),

                h1("Confusion Matrix"),
                plotOutput("confMatrix"),
                textOutput("velocityDetails"),
                
                h1("Accuracy (%)"),
                verbatimTextOutput("accuracy"),
                textOutput("accuracyText"),
                
                h1("Summary of Model"),
                verbatimTextOutput("summary")
              )
      )
    )
  )
)
