#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
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

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  output$cetak_data <- renderPlot({
    
    file_biplot <- input$file_biplot
    
    if (is.null(file_biplot))
      return(NULL)
    
    data = read.csv(file_biplot$datapath, sep = input$separator)
    
    data.normal=scale(data[,-1])
    data.pca=prcomp(data.normal, center=F)
    summary(data.pca)
    biplot(data.pca, scale=0, cex=0.7)
    g=ggbiplot(data.pca, obs.scale=1, var.scale=1, labels=(data[,1]),
               elipse=T, circle=T)
    g=g+scale_color_discrete(name='')
    g=g+theme(legend.direction="horizontal", legend.position="top")
    print(g)
    theme1 = g+theme_bw()
    print(theme1)
    
  }) #end of cetak biplot
  
  output$table_biplot <- renderTable({
    file_biplot <- input$file_biplot
    ext <- tools::file_ext(file_biplot$datapath)
    
    req(file_biplot)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_biplot))
      return(NULL)
    
    data4 = read.csv(file_biplot$datapath, sep = input$separator)
    head(data4, n=10)
    
  }, height = 500, width = 700) #end of biplot
  
  output$table_regression <- renderTable({
    file_regression <- input$file_regression
    ext <- tools::file_ext(file_regression$datapath)
    
    req(file_regression)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_regression))
      return(NULL)
    
    data2 = read.csv(file_regression$datapath, sep = input$separator)
    head(data2, n=10)
    
  }, height = 500, width = 700)
  
  output$summary1 <- renderPrint({
    file_regression <- input$file_regression
    ext <- tools::file_ext(file_regression$datapath)
    
    req(file_regression)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_regression))
      return(NULL)
    
    data2 = read.csv(file_regression$datapath, sep = input$separator)
    
    x <- input$dcol
    y <- input$icol
    
    if (is.null(x))
      return(NULL)
    else if (is.null(y))
      return(NULL)
    
    model=lm(data2[, c(x,y)], data=data2)
    summary(model)
  })#end of summary regression 1
  
  output$cetak_data2 <- renderPlot({
    
    file_regression <- input$file_regression
    ext <- tools::file_ext(file_regression$datapath)
    
    req(file_regression)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_regression))
      return(NULL)
    
    data2 = read.csv(file_regression$datapath)
    
    #selectedData <- reactive({
    #data2[, c(input$ycol,input$xcol)]
    #})
    x <- input$dcol
    y <- input$icol
    
    if (is.null(x))
      return(NULL)
    else if (is.null(y))
      return(NULL)
    
    model2=lm(formula = data2[, c(x,y)], data=data2)
    par(mfrow=c(2,2))
    plot(model2)
    
  },  height = 500, width = 700) #end of regression analysis 1
  
  output$table_multiregression <- renderTable({
    file_multiregression <- input$file_multiregression
    ext <- tools::file_ext(file_multiregression$datapath)
    
    req(file_multiregression)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_multiregression))
      return(NULL)
    
    data3 = read.csv(file_multiregression$datapath, sep = input$separator)
    head(data3, n=10)
    
  }, height = 500, width = 700)#end of multiregression
  
  output$summary2 <- renderPrint({
    file_multiregression <- input$file_multiregression
    ext <- tools::file_ext(file_multiregression$datapath)
    
    req(file_multiregression)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_multiregression))
      return(NULL)
    
    data3 = read.csv(file_multiregression$datapath, sep = input$separator)
    
    x <- data3[, c(input$dcol)]
    
    if (is.null(x))
      return(NULL)
    
    model=lm(formula = x~., data=data3)
    summary(model)
  })#end of summary multiregression 
  
  output$cetak_data3 <- renderPlot({
    
    file_multiregression <- input$file_multiregression
    ext <- tools::file_ext(file_multiregression$datapath)
    
    req(file_multiregression)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_multiregression))
      return(NULL)
    
    data3 = read.csv(file_multiregression$datapath)
    
    x <- data3[, c(input$dcol)]
    
    if (is.null(x))
      return(NULL)
    
    model2=lm(formula = x~., data=data3)
    par(mfrow=c(2,2))
    plot(model2)
    
  },  height = 500, width = 700) #end of multiregression analysis 
  
  output$table_SEM <- renderTable({
    file_SEM <- input$file_SEM
    ext <- tools::file_ext(file_SEM$datapath)
    
    req(file_SEM)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_SEM))
      return(NULL)
    
    datasem = read.csv(file_SEM$datapath)
    head(datasem, n=5)
  }
  )
  
  output$summary4 <- renderPrint({
    get_source <-read.csv(text = input$source, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source <- unlist(get_source)
    
    
    get_target <-read.csv(text = input$target, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target <- unlist(get_target)
    
    
    matrix_inner = cbind(get_source, get_target)
    colnames(matrix_inner) = c("Source", "Target")
    
    print(matrix_inner)
    cat(sprintf("\n\n"))
    
    get_source_ind <-read.csv(text = input$source_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source_ind <- unlist(get_source_ind)
    
    
    get_target_ind <-read.csv(text = input$target_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target_ind <- unlist(get_target_ind)
    
    
    matrix_outer = cbind(get_source_ind, get_target_ind)
    colnames(matrix_outer) = c("Source Indicator", "Target Indicator")
    
    print(matrix_outer)
    cat(sprintf("\n\n"))
    
    file_SEM <- input$file_SEM
    ext <- tools::file_ext(file_SEM$datapath)
    
    req(file_SEM)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_SEM))
      return(NULL)
    
    datasem = read.csv(file_SEM$datapath)
    
    model_sem <- plsm(data = datasem, strucmod = matrix_inner, measuremod = matrix_outer)
    
    cetak_sem <- sempls(model = model_sem, data = datasem, wscheme = "centroid")
    
    print(cetak_sem)
  })#end of summarySEM analysis
  
  output$cetak_data4 <- renderGrViz({
    
    get_source <-read.csv(text = input$source, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source <- unlist(get_source)
    
    
    get_target <-read.csv(text = input$target, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target <- unlist(get_target)
    
    
    matrix_inner = cbind(get_source, get_target)
    colnames(matrix_inner) = c("Source", "Target")
    
    
    get_source_ind <-read.csv(text = input$source_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source_ind <- unlist(get_source_ind)
    
    
    get_target_ind <-read.csv(text = input$target_ind, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target_ind <- unlist(get_target_ind)
    
    
    matrix_outer = cbind(get_source_ind, get_target_ind)
    colnames(matrix_outer) = c("Source Indicator", "Target Indicator")
    
    
    file_SEM <- input$file_SEM
    ext <- tools::file_ext(file_SEM$datapath)
    
    req(file_SEM)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_SEM))
      return(NULL)
    
    datasem = read.csv(file_SEM$datapath)
    
    model_sem <- plsm(data = datasem, strucmod = matrix_inner, measuremod = matrix_outer)
    
    cetak_sem <- sempls(model = model_sem, data = datasem, wscheme = "centroid")
    
    
    
    pathDiagram(cetak_sem, file = "graph_sem", full = TRUE, edge.labels = "both", output.type = "graphics", digits = 2, graphics.fmt = "pdf")
    grViz("graph_sem.dot")
    
  }) #end of SEM analysis

})
