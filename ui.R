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
library(ca)
library(mlbench)
library(dplyr)
library(clValid)
library(cluster)
library(semPlot)
library(lavaan)
library(haven)
library(shinyjs)
library(ggplot2)
library(scatterplot3d)
library(htmlwidgets)
library(pivottabler)

# Define UI for application that draws a histogram
not_sel <- "Not Selected"

shinyUI(fluidPage(
  useShinyjs(),
  
  theme = shinythemes::shinytheme("united"),
  #inisialize not sel
  
  titlePanel(
    windowTitle = "Advance Analysis",
    title = tags$head(tags$link(rel="icon", 
                                href="data:image/x-icon;base64,AAABAAEAEBAAAAEACABoBQAAFgAAACgAAAAQAAAAIAAAAAEACAAAAAAAAAEAAAAAAAAAAAAAAAEAAAAAAAAAAAAAPF/0AD90+wA5Q+gAL0DxAGhz2wA7QusAYKnpAIDG8QC80vYAg6nwAGeb5wBHS/sAOVv4ADmJ+wA3V/MAY17VADyR9QD//fcAO3f0AEB48QA8gfkAOkHvADdx/QA5ffQAYpXYAEps4QD0+fsAPEPyADpj8wBAbOoAdZHjADtT9wBCpv0AOHr1AD9Y6QDo+/8ARkfnAD1z8ADr+/8AK1vqADyY/AA3Q+sAOljyAD2q+wA7gvgAO377ADJe6gD5//QANVv7AP/+/wAvVO4AQITzAGSS4wCm0PAA6Or/ADh99wA4SO8A///9AJLC5wA9j/YAPI75APT8/gA9o/gAPEvnAD6b/gA2cPMA2+j+AD6X+QBCkPwAOpb8APH5/wAvQOYAOGb3ADdX+AA3W/UAOFnqADqY9wC35PsAQ2HsADhi8gA5l/oAOmDnADhR+QB/tOgARYLzAEJC6QA0OfsASVLdADtf8wBqheEAPW31AECK9gDi9P8Av+T8ADhI8gAyRO0AO3P1AHZ9zgBBmvsAOFLvAECL/AA7a/MALz/rAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADsHAAAAAAAAAAAAAAAAQU1RRClUAAAAAAAAAE4ANlxlC149PAAAAAAACCEcAwQAAAAAVS0AAAAAACwoWjFgJQAAABQTAAAAACQ/HwAAM19iAAAeZgAAAAA+Y0UAAAANZF0AT1kAAAAAAEYRGQAAAEovACMrAAAAAAAADjRDAAAKHUlADwAAAAAAAEcYJicAABpbYQEAAAAAAAAACUIXEgAAAiI4AAAAAAAAAABSUEswAAAuFQAAAAAAAAAAAEwgDDIAADUAAAAAAAAAAAAAUzlYN2dXAAAAAAAAAAAAAABIBhYqEAAAAAAAAAAAAAAAAAVWGzoAAP/PAAD/AwAA+gMAAODzAADgcwAAxjMAAMcTAADjkwAA8YMAAPDDAAD4YwAA/DMAAP4bAAD/AwAA/4MAAP/DAAA=", 
                                type="image/x-icon")
    )),
  
  navbarPage(
    "Advance Analysis",
    
    tags$style(type = "text/css", ".container-fluid {padding-left:0px;
                padding-right:0px; margin-right:0px; margin-left:0px;}"),
    
    
    tags$style(type = "text/css", "h2 {margin-top:0px; margin-bottom:0px;}"),
  
    
    tags$style(type = "text/css", ".navbar-brand {float: left; padding:15px 15px;
                   padding-left:100px; font-size: 18px; line-height:20px; height:50px;}"),
    
    tags$style(type = "text/css", ".pvtRendererArea {max-height: 500px; overflow: scroll;
    padding: 5px;
    max-width: 500px;}"),
    
    tags$style(type = "text/css", "#mypivot {width: 100%; 
    height: 100%; visibility: inherit;
    overflow: scroll;}"),
    
    tabPanel(
      "Pivot Table",
      
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
        fileInput("file_pivot", "Choose CSV or XLSX File",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",".xlsx",
                    "text/comma-separated-values,text/plain",
                    ".csv", ".xlsx")),
        
        radioButtons("radio_pivot", label = "Choose Variable Pivot",
                     choices = c("2 Variable" = "2Variable","3 Variable (1 Column 2 Row)" = "3Variable1col",
                                 "3 Variable (2 Column 1 Row)" = "3Variable2col")
        ),
        
        submitButton("Apply Variable"),
        
        br(),
        
        
        selectInput('colpiv1', 'Select 1st Column Pivot', choices = c(not_sel)),
        selectInput('colpiv2', 'Select 2nd Column Pivot', choices = c(not_sel)),
        selectInput('rowpiv1', 'Select 1st Row Pivot', choices = c(not_sel)),
        selectInput('rowpiv2', 'Select 2nd Row Pivot', choices = c(not_sel)),
        selectInput('calpiv', 'Select Calculation Pivot', choices = c(not_sel)),
        
        selectInput("methodpiv", "Choose Values of Pivot",
                     choices = c("Count" = "count","Average" = "average",
                                 "Min" = "min", "Max" = "max", "Median" = "median"
                                 )
        ),
        
        submitButton("Submit"),
        
        
      ), #end of sidebarpanel
      
      # Show a result of pivot
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
            dataTableOutput(outputId = "table_pivot"),
          ),#end of tab panel1 pivot
          tabPanel(
            p("Table Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            pivottablerOutput(outputId = "mypivot"),
          ),#end of tab panel1 pivot
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_pivot", label = "Download Plot"),
            plotOutput(outputId = "plot_pivot")
          ),#end of tab panel1 pivot
          
        ), #end od tabsetPanel pivot
      )
    ), #end of tabpanelpivot
    
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
        fileInput("file_biplot", "Choose CSV or XLSX File",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",".xlsx",
                    "text/comma-separated-values,text/plain",
                    ".csv", ".xlsx")),
        
        selectInput('colcount', 'Categorical variable', choices = c(not_sel)),
        
        
        submitButton("Submit"),
        
        
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
            dataTableOutput(outputId = "table_biplot"),
          ),#end of tab panel1 biplot
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_biplot", label = "Download Plot"),
            plotOutput(outputId = "cetak_data"),
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
        fileInput("file_regression", "Choose CSV or XLSX File",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",".xlsx",
                    "text/comma-separated-values,text/plain",
                    ".csv", ".xlsx")),
        
        #choose separator
        #radioButtons("separator", "Separator",
         #            choices = c(Comma = ",",
          #                       Semicolon = ";",
           #                      Tab = "\t"),
            #         selected = ",", inline = TRUE),
        
        radioButtons("radio_regress", label = "Choose Regression Method",
                     choices = list("SimpleRegression","MultipleRegression")
                      ),
        submitButton("Apply Method"),
        
        br(),
        
        selectInput('dcol', 'Dependent variable', choices = c(not_sel)),
        selectInput('icol', 'Independent variable', choices = c(not_sel)),
        selectInput('icolsec', 'Independent variable 2', choices = c(not_sel)),
        
        
        submitButton("Submit"),
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
            dataTableOutput(outputId = "table_regression"),
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
            downloadButton(outputId = "down_regresi", label = "Download Plot"),
            plotOutput(outputId = "cetak_data2")
          ),#end of tab panel2 regression
          
        ), #end od tabsetPanel regression
      )
    ), #end of tabpanel2 regression
    
    
    #corespondence analysis
    tabPanel(
      "Correspondence Analysis",
      
      # Sidebar with a numeric input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_ca", "Choose CSV or XLSX File",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",".xlsx",
                    "text/comma-separated-values,text/plain",
                    ".csv", ".xlsx")),
        
        selectInput('colcount_ca', 'Categorical variable', choices = c(not_sel)),
        
        
        submitButton("Submit"),
        
        #Select type file download
        #radioButtons("type", "Select type file download:",
        #             choices = c("pdf", "png"), selected = "pdf", inline = TRUE)
        
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
            dataTableOutput(outputId = "table_ca"),
          ),#end of tab panel1 ca
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_ca", label = "Download Plot"),
            plotOutput(outputId = "cetak_data5"),
          ),#end of tab panel2 ca
          
        ), #end od tabsetPanel ca
      )
    ), #end of tabpanel corresponden
    
    
    #hcluster analysis
    tabPanel(
      "Hierarchical Clustering",
      
      # Sidebar with a numeric input for number of bins 
      sidebarPanel(
        
        h2("Please Upload Your Data Below", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 24px;
                font-weight: 500;
                text-align:center
                "),
        
        #input file
        fileInput("file_hclus", "Choose CSV or XLSX File",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",".xlsx",
                    "text/comma-separated-values,text/plain",
                    ".csv", ".xlsx")),
        
        radioButtons("radio_hclus", label = "Choose Hierarchical Cluster Method",
                     choices = list("SingleLinkage", "CompleteLinkage", "AverageLinkage"),
                                   ),
        
        selectInput('colcount_hclus', 'Categorical variable', choices = c(not_sel)),
        
        
        numericInput('colcount_cluster', 'Cluster Count', 1, min = 1, max = 10),
        submitButton("Submit"),
        
        
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
            dataTableOutput(outputId = "table_hclus"),
          ),#end of tab panel1 hclus
          
          tabPanel(
            p("Summary of HAC", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            verbatimTextOutput("summary5"),
          ),#end of tab panel1 sem
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_hclus", label = "Download Plot"),
            plotOutput(outputId = "cetak_data6"),
          ),#end of tab panel2 hcluster
          
        ), #end od tabsetPanel hcluster
      )
    ), #end of tabpanel hcluster
    
    
    
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
        
        fileInput("file_lavaan", "Choose CSV or XLSX File",
                  multiple = TRUE,
                  accept = c(
                    "text/csv",".xlsx",
                    "text/comma-separated-values,text/plain",
                    ".csv", ".xlsx")),
        
        h4("Determine SEM Model:", style="
                  font-family: 'Poppins', sans-serif;
                  color: Black;
                  font-size: 16px;
                  font-weight: 500;
                  text-align:center
                  "),
        textAreaInput("indikator", "Indikator", value = "",placeholder = "input here", height = 150, width = 260),
        textAreaInput("label", "Label", value = "",placeholder = "input here", height = 150, width = 260),
        
        h4("Determine Relationship Model:", style="
                  font-family: 'Poppins', sans-serif;
                  color: Black;
                  font-size: 16px;
                  font-weight: 500;
                  text-align:center
                  "),
        
        
        textAreaInput("label2", "Label", value = "",placeholder = "input here", height = 150, width = 260),
        textAreaInput("label3", "Label", value = "",placeholder = "input here", height = 150, width = 260),
        
        
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
            dataTableOutput(outputId = "table_lavaan"),
          ),#end of tab panel1 regression
          tabPanel(
            p("Summary of SEM", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            verbatimTextOutput("summary6"),
          ),#end of tab panel1 sem
          
          tabPanel(
            p("Graphics Result", style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                text-align:center
                "),
            downloadButton(outputId = "down_lavaan", label = "Download Plot"),
            plotOutput("cetak_data7")
          ),#end of tab panel2 sem
          
        ), #end od tabsetPanel sem
      )
    ), #end of tabpanel2 sem
    
    
    
    
    tabPanel(
      "References",
      
      p(
      "1. If you want to analyze the characteristics of the attributes and objects of observation and want to see the relative position of these objects, it is recommended to use", strong("Biplot Analysis"),
      
      br(),
      br(),
      
      "2. if you want to see the analysis of the influence and predict the relationship that occurs between 2 objects based on the independent variable and the dependent variable, it is recommended to use", strong("Regression Analysis"),
      
      br(),
      br(),
      
      "3. if you want to analyze objects by looking at the categories of each object based on their respective interests, it is recommended to use", strong("Correspondence Analysis"),
      
      
      br(),
      br(),
      
      "4. If you want to group objects based on the similarity between these objects, it is advisable to use", strong("Hierarchical Cluster Analysis"),
      
      br(),
      br(),
      
      "5. if you want to analyze the relationship between bound objects based on the cause and effect of the existence of the object, it is recommended to use", strong("SEM Analysis"),
      
      br(),
      br(),
      
      style="
                font-family: 'Poppins', sans-serif;
                color: Black;
                font-size: 16px;
                font-weight: 500;
                padding-left: 15px;
                padding-right:15px;",
      
      ), #end of p references
    ), #end tabpanel references
    
    
  ), #end of navbarpage
  
  

))
