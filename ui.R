#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(devtools)
library(ggbiplot)
library(Hmisc)
library(caret)
library(DiagrammeR)
library(ggiraph)
library(ggiraphExtra)
library(shinythemes)
library(semPLS)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  theme = shinythemes::shinytheme("united"),
  navbarPage(
    "Advance Analysis",
    tabPanel(
      "Biplot Analysis",
      
      # Sidebar with a slider input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_biplot", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        
        #choose separator
        radioButtons("separator", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ",", inline = TRUE),
        
      ), #end of sidebarpanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            p("10 Top Data", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            tableOutput(outputId = "table_biplot"),
          ),#end of tab panel1 biplot
          
          tabPanel(
            p("Biplot Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            plotOutput(outputId = "cetak_data")
          ),#end of tab panel2 biplot
          
        ), #end od tabsetPanel biplot
      )
    ), #end of tabpanel1
    
    #regression analysis
    tabPanel(
      "Regression Analysis",
      
      # Sidebar with a slider input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_regression", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        
        #choose separator
        radioButtons("separator", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ",", inline = TRUE),
        textInput("dcol", "Dependent Variable", value = "",placeholder = "input here",width = 200),
        
        textInput("icol", "Independent Variable", value = "",placeholder = "input here",width = 200),
        
        submitButton("Submit")
      ), #end of sidebarpanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            p("10 Top Data", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            tableOutput(outputId = "table_regression"),
          ),#end of tab panel1 regression
          tabPanel(
            p("Summary of Regression", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            verbatimTextOutput("summary1"),
          ),#end of tab panel1 regression
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            plotOutput(outputId = "cetak_data2")
          ),#end of tab panel2 regression
          
        ), #end od tabsetPanel regression
      )
    ), #end of tabpanel2 regression
    
    tabPanel(
      "SEM Analysis",
      
      # Sidebar with a slider input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_SEM", "Choose CSV File",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv"),
        ),
        
        h4("Tentukan hubungan variabel (Inner Model):", style="
                  font-family: 'Poppins', sans-serif;
                  color: Black;
                  font-size: 16px;
                  font-weight: 500;
                  text-align:center
                  "),
        textAreaInput("source", "Source", value = "",placeholder = "input here", height = 150, width = 260),
        textAreaInput("target", "Target", value = "",placeholder = "input here", height = 150, width = 260),
        
        h4("Tentukan hubungan variabel (Outer Model):", style="
                  font-family: 'Poppins', sans-serif;
                  color: Black;
                  font-size: 16px;
                  font-weight: 500;
                  text-align:center
                  "),
        textAreaInput("source_ind", "Source", value = "",placeholder = "input here", height = 150, width = 260),
        textAreaInput("target_ind", "Target", value = "",placeholder = "input here", height = 150, width = 260),
        
        submitButton("Submit")
      ), #end of sidebarpanel
      
      # Show a plot of the generated distribution
      mainPanel(
        tabsetPanel(
          tabPanel(
            p("10 Top Data", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            tableOutput(outputId = "table_SEM"),
          ),#end of tab panel1 regression
          tabPanel(
            p("Summary of SEM", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            verbatimTextOutput("summary4"),
          ),#end of tab panel1 sem
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            grVizOutput("cetak_data4")
          ),#end of tab panel2 sem
          
        ), #end od tabsetPanel sem
      )
    ), #end of tabpanel2 sem
    
    
  ), #end of navbarpage
  
  

))
