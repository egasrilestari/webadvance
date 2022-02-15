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
library(dplyr)
library(mlbench)
library(ca)
library(clValid)
library(cluster)
library(semPlot)
library(lavaan)
library(haven)
library(ggplot2)
library(shinyjs)
library(scatterplot3d)
library(htmlwidgets)
library(writexl)
library(readr)
library(pivottabler)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  not_sel <- "Not Selected"
  
  #data pivot
  import_pivot <- reactive({
    file_pivot <- input$file_pivot
    ext <- tools::file_ext(file_pivot$datapath)
    
    req(file_pivot)
    if(ext != "csv" && ext != "xlsx"){
      validate("Please upload a csv or excel file")
    }
    
    if (is.null(file_pivot))
      return(NULL)
    
    if(stringr::str_ends(file_pivot$datapath, "csv")){
      datapiv = read.csv(file_pivot$datapath)
    } else if (stringr::str_ends(file_pivot$datapath, "(xlsx|xls)")) {
      datapiv = readxl::read_excel(file_pivot$datapath)
    }
    
  })
  
  observeEvent(import_pivot(), {
    choices <- c(not_sel, names(import_pivot()))
    updateSelectInput(inputId = 'colpiv1', choices = choices)
    updateSelectInput(inputId = 'colpiv2', choices = choices)
    updateSelectInput(inputId = 'rowpiv1', choices = choices)
    updateSelectInput(inputId = 'rowpiv2', choices = choices)
    updateSelectInput(inputId = 'calpiv', choices = choices)
  })
  
  output$table_pivot <- renderDataTable({
    import_pivot()
    
  }, options = list(
    searching = TRUE,
    autoWidth=TRUE,
    paging=TRUE,
    scrollX=TRUE,
    scrollY="500px",
    scrollCollapse = TRUE,
    fixedHeader=TRUE,
    fixedColumns=list(leftColumns = 2, rightColumns = 0, 
                      heightMatch = 'none')
  ),) #end of pivot
  
  
  observeEvent(input$radio_pivot, {
    if(input$radio_pivot == "2Variable"){
      shinyjs::disable("rowpiv2")
      shinyjs::disable("colpiv2")
      
    } else if (input$radio_pivot == "3Variable1col"){
      shinyjs::enable("rowpiv2")
      shinyjs::disable("colpiv2")
    } else {
      shinyjs::disable("rowpiv2")
      shinyjs::enable("colpiv2")
    }
  })
  
  method <- function(met){
    if(met == "count"){
      "n()"
    } else if (met == "min"){
      paste("min(", input$calpiv, ", na.rm=TRUE)")
    } else if (met == "max"){
      paste("max(", input$calpiv, ", na.rm=TRUE)")
    } else if (met == "average"){
      paste("mean(", input$calpiv, ", na.rm=TRUE)")
    } else if (met == "median"){
      paste("median(", input$calpiv, ", na.rm=TRUE)")
    }
  }
  
  #pivot result
  output$mypivot <- renderPivottabler({
    
    
    if(input$radio_pivot == "2Variable"){
      
      pt <- PivotTable$new()
      pt$addData(import_pivot())
      
      
      pt$addColumnDataGroups(input$colpiv1)
      
      pt$addRowDataGroups(input$rowpiv1)
      
      #calculation
      pt$defineCalculation(calculationName = input$calpiv,summariseExpression = method(input$methodpiv),
                           format = "%.1f")
      pt$evaluatePivot()
      pivottabler(pt)
      
    } else if (input$radio_pivot == "3Variable1col"){
      pt <- PivotTable$new()
      pt$addData(import_pivot())
      
      
      pt$addColumnDataGroups(input$colpiv1)
      
      pt$addRowDataGroups(input$rowpiv1)
      pt$addRowDataGroups(input$rowpiv2)
      
      #calculation
      pt$defineCalculation(calculationName = input$calpiv,summariseExpression = method(input$methodpiv),
                           format = "%.1f")
      pt$evaluatePivot()
      pivottabler(pt)
    } else {
      pt <- PivotTable$new()
      pt$addData(import_pivot())
      
      
      pt$addColumnDataGroups(input$colpiv1)
      pt$addColumnDataGroups(input$colpiv2)
      
      pt$addRowDataGroups(input$rowpiv1)
      
      #calculation
      pt$defineCalculation(calculationName = input$calpiv,summariseExpression = method(input$methodpiv),
                           format = "%.1f")
      pt$evaluatePivot()
      pivottabler(pt)
    }
    
    }, 
  )
  
  #choose method and grouping
      method2 <- function(i, y){
        if(i == "average"){
          mean(y, na.rm=TRUE)
        } else if (i == "min") {
          min(y, na.rm =TRUE)
        } else if (i == "max") {
          max(y, na.rm =TRUE)
        } else if (i == "median") {
          median(y, na.rm =TRUE)
        }
      }
  
  output$plot_pivot <- renderPlot({
    file_pivot <- input$file_pivot
    ext <- tools::file_ext(file_pivot$datapath)
    
    req(file_pivot)
    if(ext != "csv" && ext != "xlsx"){
      validate("Please upload a csv or excel file")
    }
    
    if (is.null(file_pivot))
      return(NULL)
    
    if(stringr::str_ends(file_pivot$datapath, "csv")){
      datapiv = read.csv(file_pivot$datapath)
    } else if (stringr::str_ends(file_pivot$datapath, "(xlsx|xls)")) {
      datapiv = readxl::read_excel(file_pivot$datapath)
    }
    
    if(input$radio_pivot == "2Variable"){
      kol2 = datapiv[, (input$colpiv1)]
      
      row12 = datapiv[, (input$rowpiv1)]
      cal = datapiv[, (input$calpiv)]
      
      #combine dataset
      final2 <- data.frame(kol2, row12, cal)
      
      df_final2 <- final2 %>%
        group_by(kol2, row12) %>%
        dplyr::summarise(val2 = n(),
                         another2 = method2(input$methodpiv, (cal)))
      df_final2
      df_final22 <- as.data.frame(df_final2)
      
      if(input$methodpiv == "count"){
        ggplot(df_final22, aes(x=row12, y=val2, fill=kol2)) + 
        geom_bar(position="dodge", stat="identity") +
        labs(title = "Pivot Bar Chart", x = (names(datapiv[input$rowpiv1])), y = ("Count")) +
        theme(plot.title = element_text(hjust = 0.5)) +
        guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
        
      } else {
        ggplot(df_final22, aes(x=row12, y=another2, fill=kol2)) + 
          geom_bar(position="dodge", stat="identity") +
          labs(title = "Pivot Bar Chart", x = (names(datapiv[input$rowpiv1])), y = ("Count")) +
          theme(plot.title = element_text(hjust = 0.5)) +
          guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
      }
      
      
      
    } else if (input$radio_pivot == "3Variable1col") {
      #choose pivot column
      kol1 = datapiv[, (input$colpiv1)]
      
      row1 = datapiv[, (input$rowpiv1)]
      row2 = datapiv[, (input$rowpiv2)]
      cal2 = datapiv[, (input$calpiv)]
      
      #combine dataset
      final <- data.frame(kol1, row1, row2, cal2)
      
      #grouping
      df_final <- final %>%
        group_by(kol1, row1, row2) %>%
        dplyr::summarise(val = n(),
                         another3 = method2(input$methodpiv, (cal2)))
      
      df_final[is.na(df_final)] = 0
      df_final$concat <- paste(df_final$row1, df_final$row2, sep = "-")
      df_final2 <- as.data.frame(df_final)
      df_final3 <- select(df_final2, -c(row1, row2))
      
      
      if(input$methodpiv == "count"){
      ggplot(df_final3, aes(x=concat, y=val, fill=kol1), 
      ) + 
        geom_bar(position="dodge", stat="identity") +
        labs(title = "Pivot Bar Chart", x = ("Pivot"), y = ("Count")) +
        theme(plot.title = element_text(hjust = 0.5),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
      } else {
        ggplot(df_final3, aes(x=concat, y=another3, fill=kol1), 
        ) + 
          geom_bar(position="dodge", stat="identity") +
          labs(title = "Pivot Bar Chart", x = ("Pivot"), y = ("Count")) +
          theme(plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
      }
    } else {
      #choose pivot column
      kol1_col = datapiv[, (input$colpiv1)]
      kol2_col = datapiv[, (input$colpiv2)]
      
      row_col = datapiv[, (input$rowpiv1)]
      cal3 = datapiv[, (input$calpiv)]
      
      #combine dataset
      final_col <- data.frame(kol1_col, kol2_col, row_col, cal3)
      
      #grouping
      df_final_col <- final_col %>%
        group_by(kol1_col, kol2_col, row_col) %>%
        dplyr::summarise(val = n(),
                         another4 = method2(input$methodpiv, (cal3)))
      
      df_final_col[is.na(df_final_col)] = 0
      df_final_col$concat_col <- paste(df_final_col$kol1_col, df_final_col$kol2_col, sep = "-")
      df_final2_col <- as.data.frame(df_final_col)
      df_final3_col <- select(df_final2_col, -c(kol1_col, kol2_col))
      
      if(input$methodpiv == "count"){
        ggplot(df_final3_col, aes(x=row_col, y=val, fill=concat_col), 
        ) + 
          geom_bar(position="dodge", stat="identity") +
          labs(title = "Pivot Bar Chart", x = ("Pivot"), y = ("Count")) +
          theme(plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
      } else {
        ggplot(df_final3_col, aes(x=row_col, y=another4, fill=concat_col), 
        ) +
          geom_bar(position="dodge", stat="identity") +
          labs(title = "Pivot Bar Chart", x = names(datapiv[input$colpiv1]), y = ("Count")) +
          theme(plot.title = element_text(hjust = 0.5),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          guides(fill=guide_legend(title="Pivot"))
      }
      
    }
  })
  
  output$down_pivot <- downloadHandler(
    filename = function(){
      paste("pivot","png", sep = ".")
    },
    content = function(file){
      png(file)
      
      file_pivot <- input$file_pivot
      ext <- tools::file_ext(file_pivot$datapath)
      
      req(file_pivot)
      if(ext != "csv" && ext != "xlsx"){
        validate("Please upload a csv or excel file")
      }
      
      if (is.null(file_pivot))
        return(NULL)
      
      if(stringr::str_ends(file_pivot$datapath, "csv")){
        datapiv = read.csv(file_pivot$datapath)
      } else if (stringr::str_ends(file_pivot$datapath, "(xlsx|xls)")) {
        datapiv = readxl::read_excel(file_pivot$datapath)
      }
      
      if(input$radio_pivot == "2Variable"){
        kol2 = datapiv[, (input$colpiv1)]
        
        row12 = datapiv[, (input$rowpiv1)]
        cal = datapiv[, (input$calpiv)]
        
        #combine dataset
        final2 <- data.frame(kol2, row12, cal)
        
        df_final2 <- final2 %>%
          group_by(kol2, row12) %>%
          dplyr::summarise(val2 = n(),
                           another2 = method2(input$methodpiv, (cal)))
        df_final2
        df_final22 <- as.data.frame(df_final2)
        
        if(input$methodpiv == "count"){
          plot2var <- ggplot(df_final22, aes(x=row12, y=val2, fill=kol2)) + 
                      geom_bar(position="dodge", stat="identity") +
                      labs(title = "Pivot Bar Chart", x = (names(datapiv[input$rowpiv1])), y = ("Count")) +
                      theme(plot.title = element_text(hjust = 0.5)) +
                      guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
          
        } else {
          plot2var <- ggplot(df_final22, aes(x=row12, y=another2, fill=kol2)) + 
                      geom_bar(position="dodge", stat="identity") +
                      labs(title = "Pivot Bar Chart", x = (names(datapiv[input$rowpiv1])), y = ("Count")) +
                      theme(plot.title = element_text(hjust = 0.5)) +
                      guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
        }
        
        
        
        print(plot2var)
      
        } else if (input$radio_pivot == "3Variable1col") {
          #choose pivot column
          kol1 = datapiv[, (input$colpiv1)]
          
          row1 = datapiv[, (input$rowpiv1)]
          row2 = datapiv[, (input$rowpiv2)]
          cal2 = datapiv[, (input$calpiv)]
          
          #combine dataset
          final <- data.frame(kol1, row1, row2, cal2)
          
          #grouping
          df_final <- final %>%
            group_by(kol1, row1, row2) %>%
            dplyr::summarise(val = n(),
                             another3 = method2(input$methodpiv, (cal2)))
          
          df_final[is.na(df_final)] = 0
          df_final$concat <- paste(df_final$row1, df_final$row2, sep = "-")
          df_final2 <- as.data.frame(df_final)
          df_final3 <- select(df_final2, -c(row1, row2))
          
          
          if(input$methodpiv == "count"){
            plot3var1col <- ggplot(df_final3, aes(x=concat, y=val, fill=kol1), 
                            ) + 
                              geom_bar(position="dodge", stat="identity") +
                              labs(title = "Pivot Bar Chart", x = ("Pivot"), y = ("Count")) +
                              theme(plot.title = element_text(hjust = 0.5),
                                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                              guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
          } else {
            plot3var1col <- ggplot(df_final3, aes(x=concat, y=another3, fill=kol1), 
                            ) + 
                              geom_bar(position="dodge", stat="identity") +
                              labs(title = "Pivot Bar Chart", x = ("Pivot"), y = ("Count")) +
                              theme(plot.title = element_text(hjust = 0.5),
                                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                              guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
          }
        print(plot3var1col)
      } else {
        #choose pivot column
        kol1_col = datapiv[, (input$colpiv1)]
        kol2_col = datapiv[, (input$colpiv2)]
        
        row_col = datapiv[, (input$rowpiv1)]
        cal3 = datapiv[, (input$calpiv)]
        
        #combine dataset
        final_col <- data.frame(kol1_col, kol2_col, row_col, cal3)
        
        #grouping
        df_final_col <- final_col %>%
          group_by(kol1_col, kol2_col, row_col) %>%
          dplyr::summarise(val = n(),
                           another4 = method2(input$methodpiv, (cal3)))
        
        df_final_col[is.na(df_final_col)] = 0
        df_final_col$concat_col <- paste(df_final_col$kol1_col, df_final_col$kol2_col, sep = "-")
        df_final2_col <- as.data.frame(df_final_col)
        df_final3_col <- select(df_final2_col, -c(kol1_col, kol2_col))
        
        if(input$methodpiv == "count"){
          plot3var2col <- ggplot(df_final3_col, aes(x=row_col, y=val, fill=concat_col), 
                          ) + 
                            geom_bar(position="dodge", stat="identity") +
                            labs(title = "Pivot Bar Chart", x = ("Pivot"), y = ("Count")) +
                            theme(plot.title = element_text(hjust = 0.5),
                                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                            guides(fill=guide_legend(title=names(datapiv[input$colpiv1])))
        } else {
          plot3var2col <- ggplot(df_final3_col, aes(x=row_col, y=another4, fill=concat_col), 
                          ) +
                            geom_bar(position="dodge", stat="identity") +
                            labs(title = "Pivot Bar Chart", x = names(datapiv[input$colpiv1]), y = ("Count")) +
                            theme(plot.title = element_text(hjust = 0.5),
                                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                            guides(fill=guide_legend(title="Pivot"))
        }
        
        print(plot3var2col)
      }
      
      dev.off()
    }
  )
  
  #data biplot
  import_biplot <- reactive({
    file_biplot <- input$file_biplot
    ext <- tools::file_ext(file_biplot$datapath)
    
    req(file_biplot)
    if(ext != "csv" && ext != "xlsx"){
      validate("Please upload a csv or excel file")
    }
    
    if (is.null(file_biplot))
      return(NULL)
    
    if(stringr::str_ends(file_biplot$datapath, "csv")){
      data4 = read.csv(file_biplot$datapath)
    } else if (stringr::str_ends(file_biplot$datapath, "(xlsx|xls)")) {
      data4 = readxl::read_excel(file_biplot$datapath)
    }
    
  })
  
  observeEvent(import_biplot(), {
    choices <- c(not_sel, names(import_biplot()))
    updateSelectInput(inputId = 'colcount', choices = choices)
  })
  
  
  output$table_biplot <- renderDataTable({
    import_biplot()
    
  }, options = list(
                    searching = TRUE,
                    autoWidth=TRUE,
                    paging=TRUE,
                    scrollX=TRUE,
                    scrollY="500px",
                    scrollCollapse = TRUE,
                    fixedHeader=TRUE,
                    fixedColumns=list(leftColumns = 2, rightColumns = 0, 
                                      heightMatch = 'none')
                  ),) #end of biplot
  
  output$cetak_data <- renderPlot({
    file_biplot <- input$file_biplot
    
    if (is.null(file_biplot))
      return(NULL)
    
    if(stringr::str_ends(file_biplot$datapath, "csv")){
      data = read.csv(file_biplot$datapath)
    } else if (stringr::str_ends(file_biplot$datapath, "(xlsx|xls)")) {
      data = readxl::read_excel(file_biplot$datapath)
    }
    
    data_bip = data[setdiff(names(data), input$colcount)]
    
    dt_new1bip <- data.matrix(data.frame(unclass(data_bip))) 
    
    dt_new2bip <- as.matrix(data[,(input$colcount)])
    final_dtbip <- data.frame(dt_new2bip, dt_new1bip)
    dt_new3bip <- final_dtbip %>%
      group_by(select(final_dtbip, c(1))) %>% 
      dplyr::summarise(across(everything(), list(sum)))
    data.normal=scale(dt_new3bip[,-1])
    data.pca=prcomp(data.normal, center=F)
    s <- summary(data.pca)
    
    biplot(data.pca, scale=0, las=1, cex=1, xlabs=as.matrix(dt_new3bip[, 1]),
           xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""),
           ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""),)
    # Add grid lines
    abline(v=0, col="grey50")
    abline(h=0, col="grey50")
  }, height = 800, width = 800) #end of cetak biplot
  
  output$down_biplot <- downloadHandler(
    #specify filename
    filename = function() {
      #biplot.png
      #biplot.pdf
      paste("biplot","png", sep = ".")
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #  pdf(file)
      
      file_biplot <- input$file_biplot
      
      if (is.null(file_biplot))
        return(NULL)
      
      if(stringr::str_ends(file_biplot$datapath, "csv")){
        data = read.csv(file_biplot$datapath)
      } else if (stringr::str_ends(file_biplot$datapath, "(xlsx|xls)")) {
        data = readxl::read_excel(file_biplot$datapath)
      }
      
      data_bip = data[setdiff(names(data), input$colcount)]
      
      dt_new1bip <- data.matrix(data.frame(unclass(data_bip))) 
      
      dt_new2bip <- as.matrix(data[,(input$colcount)])
      final_dtbip <- data.frame(dt_new2bip, dt_new1bip)
      dt_new3bip <- final_dtbip %>%
        group_by(select(final_dtbip, c(1))) %>% 
        dplyr::summarise(across(everything(), list(sum)))
      data.normal=scale(dt_new3bip[,-1])
      data.pca=prcomp(data.normal, center=F)
      s <- summary(data.pca)
      
      biplot(data.pca, scale=0, las=1, cex=1, xlabs=as.matrix(dt_new3bip[, 1]),
             xlab=paste("PCA 1 (", round(s$importance[2]*100, 1), "%)", sep = ""),
             ylab=paste("PCA 2 (", round(s$importance[5]*100, 1), "%)", sep = ""),)
      # Add grid lines
      abline(v=0, col="grey50")
      abline(h=0, col="grey50")
      
      
      dev.off()
    }
  ) #end of download button for biplot
  
 
  
  observeEvent(input$radio_regress, {
    if(input$radio_regress == "SimpleRegression"){
      shinyjs::disable("icolsec")
      
    } else {
      shinyjs::enable("icolsec")
    }
  })
  
  #data regression
  import_regress <- reactive({
    file_regression <- input$file_regression
    ext <- tools::file_ext(file_regression$datapath)
    
    req(file_regression)
    
    if(ext != "csv" && ext != "xlsx"){
      validate("Please upload a csv or excel file")
    }
    
    if (is.null(file_regression))
      return(NULL)
    
    if(input$radio_regress == "SimpleRegression"){
      shinyjs::disable("icolsec")
      
    } else if(input$radio_regress == "MultipleRegression") {
      shinyjs::enable("icolsec")
    }
    
    if(stringr::str_ends(file_regression$datapath, "csv")){
      data2 = read.csv(file_regression$datapath)
    } else if (stringr::str_ends(file_regression$datapath, "(xlsx|xls)")) {
      data2 = readxl::read_excel(file_regression$datapath)
    }
    
    
  })
  
  observeEvent(import_regress(), {
    choices <- c(not_sel, names(import_regress()))
    updateSelectInput(inputId = 'dcol', choices = choices)
    updateSelectInput(inputId = 'icol', choices = choices)
    updateSelectInput(inputId = 'icolsec', choices = choices)
  })
  
  
  output$table_regression <- renderDataTable({
    import_regress()
    
  }, options = list(
                    searching = TRUE,
                    autoWidth=TRUE,
                    paging=TRUE,
                    scrollX=TRUE,
                    scrollY="500px",
                    scrollCollapse = TRUE,
                    fixedHeader=TRUE,
                    fixedColumns=list(leftColumns = 2, rightColumns = 0, 
                                      heightMatch = 'none')
                  ),) #data table regression
  
  
  output$summary1 <- renderPrint({
    file_regression <- input$file_regression
    ext <- tools::file_ext(file_regression$datapath)
    
    req(file_regression)
    #validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_regression))
      return(NULL)
    
    if(stringr::str_ends(file_regression$datapath, "csv")){
      data2 = read.csv(file_regression$datapath)
    } else if (stringr::str_ends(file_regression$datapath, "(xlsx|xls)")) {
      data2 = readxl::read_excel(file_regression$datapath)
    }
    
    x <- input$dcol
    y <- input$icol
    z <- input$icolsec
    
    if (is.null(x))
      return(NULL)
    else if (is.null(y))
      return(NULL)
    else if (is.null(z))
      return(NULL)
    
    #nomalize
    norm <- data.matrix(data.frame(unclass(data2)))
    
    #change int dataframe
    final <- data.frame(norm)
    final
    
    if(input$radio_regress == "SimpleRegression"){
      data = final[, c(x,y)]
      model=lm(final[, c(x,y)], data=final)
      shinyjs::disable("icolsec")
      
    } else if(input$radio_regress == "MultipleRegression") {
      shinyjs::enable("icolsec")
      
      model=lm(final[, c(x,y,z)], data=final)
    }
    
    summary(model)
  })#end of summary regression 1
  
  output$cetak_data2 <- renderPlot({
    
    file_regression <- input$file_regression
    ext <- tools::file_ext(file_regression$datapath)
    
    req(file_regression)
    #validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_regression))
      return(NULL)
    
    if(stringr::str_ends(file_regression$datapath, "csv")){
      data2 = read.csv(file_regression$datapath)
    } else if (stringr::str_ends(file_regression$datapath, "(xlsx|xls)")) {
      data2 = readxl::read_excel(file_regression$datapath)
    }
    
    #selectedData <- reactive({
    #data2[, c(input$ycol,input$xcol)]
    #})
    x <- input$dcol
    y <- input$icol
    z <- input$icolsec
    
    if (is.null(x))
      return(NULL)
    else if (is.null(y))
      return(NULL)
    else if (is.null(z))
      return(NULL)
    
    #nomalize
    norm <- data.matrix(data.frame(unclass(data2)))
    
    #change int dataframe
    final <- data.frame(norm)
    final
    #model2=lm(final[, c(x,y)], data=final)
    #par(mfrow=c(2,2))
    #plot(model2)
    
    if(input$radio_regress == "SimpleRegression"){
      shinyjs::disable("icolsec")
      
      #plotting
      ggplotRegression <- function (fit) {
      
      require(ggplot2)
      
      ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
        geom_point() +
        stat_smooth(formula = y ~ x, method = "lm", col = "red") +
        labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                           "Intercept =",signif(fit$coef[[1]],5 ),
                           " Slope =",signif(fit$coef[[2]], 5),
                           " P =",signif(summary(fit)$coef[2,4], 5)))+
        theme_bw()
      }
      ggplotRegression(lm(final[, c(x,y)], data = final))
      
    } else if(input$radio_regress == "MultipleRegression") {
      shinyjs::enable("icolsec")
      
      #plotting3d
      p <- scatterplot3d(final[, c(y)], final[, c(z)], final[, c(x)],
                         xlab = colnames(final[y]), ylab = colnames(final[z]), zlab = colnames(final[x]),
                         highlight.3d = TRUE, type = "h", pch=16, grid = TRUE,
                         mar = c(2.5, 2.5, 2, 1.5), angle = 55, main = "Multiple Regression 3 Variable")
      p$plane3d(lm(final[, c(x,y,z)], data=final),draw_polygon = TRUE, draw_lines = TRUE, 
                polygon_args = list(col = rgb(.1, .2, .7, .5)))
    }
    
    
  },  height = 500, width = 700) #end of regression analysis 1
  
  output$down_regresi <- downloadHandler(
    #specify filename
    filename = function() {
      #regresi.png
      #regresi.pdf
      paste("regresi","png", sep = ".")
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #  pdf(file)
      
      file_regression <- input$file_regression
      ext <- tools::file_ext(file_regression$datapath)
      
      req(file_regression)
      #validate(need(ext == "csv", "Please upload a csv file"))
      
      if (is.null(file_regression))
        return(NULL)
      
      
      if(stringr::str_ends(file_regression$datapath, "csv")){
        data2 = read.csv(file_regression$datapath)
      } else if (stringr::str_ends(file_regression$datapath, "(xlsx|xls)")) {
        data2 = readxl::read_excel(file_regression$datapath)
      }
      
      #selectedData <- reactive({
      #data2[, c(input$ycol,input$xcol)]
      #})
      x <- input$dcol
      y <- input$icol
      z <- input$icolsec
      
      if (is.null(x))
        return(NULL)
      else if (is.null(y))
        return(NULL)
      else if (is.null(z))
        return(NULL)
      
      #nomalize
      norm <- data.matrix(data.frame(unclass(data2)))
      
      #change int dataframe
      final <- data.frame(norm)
      final
      
      if(input$radio_regress == "SimpleRegression"){
        shinyjs::disable("icolsec")
        
        #plotting
        
        ggplotRegression <- function (fit) {
          
          #require(ggplot2)
          
          ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
            geom_point() +
            stat_smooth(formula = y ~ x, method = "lm", col = "red") +
            labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                               "Intercept =",signif(fit$coef[[1]],5 ),
                               " Slope =",signif(fit$coef[[2]], 5),
                               " P =",signif(summary(fit)$coef[2,4], 5)))+
            theme_bw()
        }
        print(ggplotRegression(lm(final[, c(x,y)], data = final)))
      } else if(input$radio_regress == "MultipleRegression") {
        shinyjs::enable("icolsec")
        
        #plotting3d
        p <- scatterplot3d(final[, c(y)], final[, c(z)], final[, c(x)],
                           xlab = colnames(final[y]), ylab = colnames(final[z]), zlab = colnames(final[x]),
                           highlight.3d = TRUE, type = "h", pch=16, grid = TRUE,
                           mar = c(2.5, 2.5, 2, 1.5), angle = 55, main = "Multiple Regression 3 Variable")
        p$plane3d(lm(final[, c(x,y,z)], data=final),draw_polygon = TRUE, draw_lines = TRUE, 
                  polygon_args = list(col = rgb(.1, .2, .7, .5)))
      }
      
      dev.off()
    }
  ) #end of download button for regresi
  
  
  #data sem
  import_sem <- reactive({
    file_SEM <- input$file_SEM
    ext <- tools::file_ext(file_SEM$datapath)
    
    req(file_SEM)
    validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_SEM))
      return(NULL)
    
    datasem = read.csv(file_SEM$datapath)
  })
  
  
  output$table_SEM <- renderTable({
    import_sem() 
  }, options = list(
                    searching = TRUE,
                    autoWidth=TRUE,
                    paging=TRUE,
                    scrollX=TRUE,
                    scrollY="500px",
                    scrollCollapse = TRUE,
                    fixedHeader=TRUE,
                    fixedColumns=list(leftColumns = 2, rightColumns = 0, 
                                      heightMatch = 'none')
                  ),) #data table sem
  
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
    
  }) #end of Plot SEM analysis
  
  output$down_sem <- downloadHandler(
    #specify filename
    filename = function() {
      "sem.png"
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #pdf(file)
      
      #SEM graph
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
      
      #graph %>% export_svg %>% charToRaw %>% rsvg_png()
      #export_graph(grViz("graph_sem.dot"),
       #            file_name = file,
        #           file_type = "png")
      
      dev.off()
    },
    contentType = 'image/png'
  ) #end of download button for sem
  
  import_ca <- reactive({
    file_ca <- input$file_ca
    ext <- tools::file_ext(file_ca$datapath)
    
    req(file_ca)
    
    if(ext != "csv" && ext != "xlsx"){
      validate("Please upload a csv or excel file")
    }
    
    if (is.null(file_ca))
      return(NULL)
    
    if(stringr::str_ends(file_ca$datapath, "csv")){
      data5 = read.csv(file_ca$datapath)
    } else if (stringr::str_ends(file_ca$datapath, "(xlsx|xls)")) {
      data5 = readxl::read_excel(file_ca$datapath)
    }
    
  })
  
  #select input ca
  observeEvent(import_ca(), {
    choices <- c(not_sel, names(import_ca()))
    updateSelectInput(inputId = 'colcount_ca', choices = choices)
  })
  
  
  output$table_ca <- renderDataTable({
    
    import_ca()
    
  }, options = list(
                    searching = TRUE,
                    autoWidth=TRUE,
                    paging=TRUE,
                    scrollX=TRUE,
                    scrollY="500px",
                    scrollCollapse = TRUE,
                    fixedHeader=TRUE,
                    fixedColumns=list(leftColumns = 2, rightColumns = 0, 
                                      heightMatch = 'none')
                  ),) #end of table ca
  
  output$cetak_data5 <- renderPlot({
    file_ca <- input$file_ca
    ext <- tools::file_ext(file_ca$datapath)
    
    req(file_ca)
    #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
    
    if (is.null(file_ca))
      return(NULL)
    
    if(stringr::str_ends(file_ca$datapath, "csv")){
      data5 = read.csv(file_ca$datapath)
    } else if (stringr::str_ends(file_ca$datapath, "(xlsx|xls)")) {
      data5 = readxl::read_excel(file_ca$datapath)
    }
    
    data_ca = data5[setdiff(names(data5), input$colcount_ca)]
    
    dt_new1 <- data.matrix(data.frame(unclass(data_ca))) 
    
    dt_new2 <- as.matrix(data5[, input$colcount_ca])
  
    final_dt <- data.frame(dt_new2, dt_new1)
    
    dt_new3 <- final_dt %>%
      group_by(select(final_dt, c(1))) %>% 
      dplyr::summarise(across(everything(), list(sum)))
    dt_new3
    korespon = as.matrix(dt_new3[, -1])
    dimnames(korespon) = list(x = as.matrix(dt_new3[, 1]), y = colnames(dt_new3[,-1]))
    korespon.ca = ca(korespon)
    plot(korespon.ca, what = c("all", "all"), mass=TRUE, 
         contrib="relative", main="Analisis Korespondensi")
    
  },height = 500, width = 700)
  
  output$down_ca <- downloadHandler(
    #specify filename
    filename = function() {
      #biplot.png
      #biplot.pdf
      paste("correspondence","png", sep = ".")
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #  pdf(file)
      
      file_ca <- input$file_ca
      ext <- tools::file_ext(file_ca$datapath)
      
      req(file_ca)
      #validate(need(ext == "csv, xlsx", "Please upload a csv od excel file"))
      
      if (is.null(file_ca))
        return(NULL)
      
      if(stringr::str_ends(file_ca$datapath, "csv")){
        data5 = read.csv(file_ca$datapath)
      } else if (stringr::str_ends(file_ca$datapath, "(xlsx|xls)")) {
        data5 = readxl::read_excel(file_ca$datapath)
      }
      
      data_ca = data5[setdiff(names(data5), input$colcount_ca)]
      data_ca
      dt_new1 <- data.matrix(data.frame(unclass(data_ca))) 
      dt_new1
      dt_new2 <- as.matrix(data5[, input$colcount_ca])
      dt_new2
      final_dt <- data.frame(dt_new2, dt_new1)
      final_dt
      dt_new3 <- final_dt %>%
        group_by(select(final_dt, c(1))) %>% 
        dplyr::summarise(across(everything(), list(sum)))
      dt_new3
      korespon = as.matrix(dt_new3[, -1])
      dimnames(korespon) = list(x = as.matrix(dt_new3[, 1]), y = colnames(dt_new3[,-1]))
      korespon.ca = ca(korespon)
      plot(korespon.ca, what = c("all", "all"), mass=TRUE, 
           contrib="relative", main="Analisis Korespondensi")
      
      dev.off()
    }
  ) #end of download button for ca
  
  
  output$cetak_data6 <- renderPlot({
    
    file_hclus <- input$file_hclus
    
    if (is.null(file_hclus))
      return(NULL)
    
    if(stringr::str_ends(file_hclus$datapath, "csv")){
      data6 = read.csv(file_hclus$datapath)
    } else if (stringr::str_ends(file_hclus$datapath, "(xlsx|xls)")) {
      data6 = readxl::read_excel(file_hclus$datapath)
    }
    
    #choose cluster column
    features = data6[setdiff(names(data6), input$colcount_hclus)]
    
    #normization of categorical data
    features1 <- data.matrix(data.frame(unclass(features)))
    
    #target data to matrix
    target <- as.matrix(data6[, input$colcount_hclus])
    
    #combine
    clus_final <- data.frame(target, features1)
    
    #grouping
    clus_final2 <- clus_final %>%
      group_by(select(clus_final, c(1))) %>%
      dplyr::summarise(across(everything(), list(sum)))
    
    #normalization all of data
    data_nor <- scale(clus_final2[, -1], scale = TRUE)
    
    #count distance
    euc <- dist(data_nor, method = "euclidean")
    
    #method
    if(input$radio_hclus == "SingleLinkage") {
    
      single <- hclust(euc, method = "single")
      plot(single, hang = -2, cex = 1, labels = clus_final2$target) 
      
      #count cluster
      rect.hclust(single, input$colcount_cluster)
    } else if (input$radio_hclus == "CompleteLinkage") {
      
      complete <- hclust(euc, method = "complete")
      plot(complete, hang = -2, cex = 1, labels = clus_final2$target) 
      
      #count cluster
      rect.hclust(complete, input$colcount_cluster)
    } else if (input$radio_hclus == "AverageLinkage") {
      
      average <- hclust(euc, method = "average")
      plot(average, hang = -2, cex = 1, labels = clus_final2$target) 
      
      #count cluster
      rect.hclust(average, input$colcount_cluster)
    }
    
    
    
    
  }) #end of cetak hclus
  
  import_hclus <- reactive({
    file_hclus <- input$file_hclus
    ext <- tools::file_ext(file_hclus$datapath)
    
    req(file_hclus)
    
    if(ext != "csv" && ext != "xlsx"){
      validate("Please upload a csv or excel file")
    }
    
    if (is.null(file_hclus))
      return(NULL)
    
    if(stringr::str_ends(file_hclus$datapath, "csv")){
      data7 = read.csv(file_hclus$datapath)
    } else if (stringr::str_ends(file_hclus$datapath, "(xlsx|xls)")) {
      data7 = readxl::read_excel(file_hclus$datapath)
    }
    
  })
  
  observeEvent(import_hclus(), {
    choices <- c(not_sel, names(import_hclus()))
    updateSelectInput(inputId = 'colcount_hclus', choices = choices)
  })
  
  output$table_hclus <- renderDataTable({
    
    import_hclus()
    
  }, options = list(
                    searching = TRUE,
                    autoWidth=TRUE,
                    paging=TRUE,
                    scrollX=TRUE,
                    scrollY="500px",
                    scrollCollapse = TRUE,
                    fixedHeader=TRUE,
                    fixedColumns=list(leftColumns = 2, rightColumns = 0, 
                                      heightMatch = 'none')
                  ),) #end of hclus
  
  
  output$down_hclus <- downloadHandler(
    #specify filename
    filename = function() {
      #biplot.png
      #biplot.pdf
      paste("cluster","png", sep = ".")
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #  pdf(file)
      
      file_hclus <- input$file_hclus
      
      if (is.null(file_hclus))
        return(NULL)
      
      if(stringr::str_ends(file_hclus$datapath, "csv")){
        data6 = read.csv(file_hclus$datapath)
      } else if (stringr::str_ends(file_hclus$datapath, "(xlsx|xls)")) {
        data6 = readxl::read_excel(file_hclus$datapath)
      }
      
      #choose cluster column
      features = data6[setdiff(names(data6), input$colcount_hclus)]
      
      #normization of categorical data
      features1 <- data.matrix(data.frame(unclass(features)))
      
      #target data to matrix
      target <- as.matrix(data6[, input$colcount_hclus])
      
      #combine
      clus_final <- data.frame(target, features1)
      
      #grouping
      clus_final2 <- clus_final %>%
        group_by(select(clus_final, c(1))) %>%
        dplyr::summarise(across(everything(), list(sum)))
      
      #normalization all of data
      data_nor <- scale(clus_final2[, -1], scale = TRUE)
      
      #count distance
      euc <- dist(data_nor, method = "euclidean")
      
      #method
      if(input$radio_hclus == "SingleLinkage") {
        
        single <- hclust(euc, method = "single")
        plot(single, hang = -2, cex = 1, labels = clus_final2$target) 
        
        #count cluster
        rect.hclust(single, input$colcount_cluster)
      } else if (input$radio_hclus == "CompleteLinkage") {
        
        complete <- hclust(euc, method = "complete")
        plot(complete, hang = -2, cex = 1, labels = clus_final2$target) 
        
        #count cluster
        rect.hclust(complete, input$colcount_cluster)
      } else if (input$radio_hclus == "AverageLinkage") {
        
        average <- hclust(euc, method = "average")
        plot(average, hang = -2, cex = 1, labels = clus_final2$target) 
        
        #count cluster
        rect.hclust(average, input$colcount_cluster)
      }
      
      dev.off()
    }
  ) #end of download button for hcluster
  
  
  output$summary5 <- renderPrint({
    file_hclus <- input$file_hclus
    ext <- tools::file_ext(file_hclus$datapath)
    
    req(file_hclus)
    #validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_hclus))
      return(NULL)
    
    if(stringr::str_ends(file_hclus$datapath, "csv")){
      data9 = read.csv(file_hclus$datapath)
    } else if (stringr::str_ends(file_hclus$datapath, "(xlsx|xls)")) {
      data9 = readxl::read_excel(file_hclus$datapath)
    }
    
    #choose cluster column
    features = data9[setdiff(names(data9), input$colcount_hclus)]
    
    #normization of categorical data
    features1 <- data.matrix(data.frame(unclass(features)))
    
    #target data to matrix
    target <- as.matrix(data9[, input$colcount_hclus])
    
    #combine
    clus_final <- data.frame(target, features1)
    
    #grouping
    clus_final2 <- clus_final %>%
      group_by(select(clus_final, c(1))) %>%
      dplyr::summarise(across(everything(), list(sum)))
    
    #normalization all of data
    data_nor <- scale(clus_final2[, -1], scale = TRUE)
    
    #count distance
    euc <- dist(data_nor, method = "euclidean")
    
    row.names(data_nor) <- 1:nrow(data_nor)
    data_nor
    #method
    if(input$radio_hclus == "SingleLinkage") {
      single <- hclust(euc, method = "single")
      
      internal <- clValid((data_nor), nClust = 2:input$colcount_cluster, 
                          clMethods = "agnes", 
                          validation = "stability", 
                          metric = "euclidean",
                          method = "single")
      
    } else if (input$radio_hclus == "CompleteLinkage") {
      complete <- hclust(euc, method = "complete")
      
      
      internal <- clValid((data_nor), nClust = 2:input$colcount_cluster, 
                          clMethods = "agnes", 
                          validation = "stability", 
                          metric = "euclidean",
                          method = "complete")
      
    } else if (input$radio_hclus == "AverageLinkage") {
      average <- hclust(euc, method = "average")
      
      
      internal <- clValid((data_nor), nClust = 2:input$colcount_cluster, 
                          clMethods = "agnes", 
                          validation = "stability", 
                          metric = "euclidean",
                          method = "average")
      
    }
    
    sld_method <- input$radio_hclus
    print(paste("Method Selected", sld_method))
    
    summary(internal)
    
    d1 <- dist(data_nor[, -1])
    
    d2 <- cophenetic(hclust(euc, method = "single"))
    corrr <- cor(d1, d2)
    
    d3 <- cophenetic(hclust(euc, method = "complete"))
    corrr1 <- cor(d1, d3)
    
    d4 <- cophenetic(hclust(euc, method = "average"))
    corrr2 <- cor(d1, d4)
    
    tb_cor <- data.frame(corrr, corrr1, corrr2)
    colnames(tb_cor) <- c("Single Linkage", "Complete Linkage", "Average Linkage")
    tb_cor
    
  })#end of summary hclus
  
  import_lavaan <- reactive({
    file_lavaan <- input$file_lavaan
    ext <- tools::file_ext(file_lavaan$datapath)
    
    req(file_lavaan)
    
    if(ext != "csv" && ext != "xlsx"){
      validate("Please upload a csv or excel file")
    }
    
    if (is.null(file_lavaan))
      return(NULL)
    
    if(stringr::str_ends(file_lavaan$datapath, "csv")){
      datalav = read.csv(file_lavaan$datapath)
    } else if (stringr::str_ends(file_lavaan$datapath, "(xlsx|xls)")) {
      datalav = readxl::read_excel(file_lavaan$datapath)
    }
  })
  
  output$table_lavaan <- renderDataTable({
    
    import_lavaan()
    
  }, options = list(
                    searching = TRUE,
                    autoWidth=TRUE,
                    paging=TRUE,
                    scrollX=TRUE,
                    scrollY="500px",
                    scrollCollapse = TRUE,
                    fixedHeader=TRUE,
                    fixedColumns=list(leftColumns = 2, rightColumns = 0, 
                                      heightMatch = 'none')
                  ),) #end of lavaan
  
  
  output$summary6 <- renderPrint({
    file_lavaan <- input$file_lavaan
    ext <- tools::file_ext(file_lavaan$datapath)
    
    req(file_lavaan)
    #validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_lavaan))
      return(NULL)
    
    if(stringr::str_ends(file_lavaan$datapath, "csv")){
      datalav = read.csv(file_lavaan$datapath)
    } else if (stringr::str_ends(file_lavaan$datapath, "(xlsx|xls)")) {
      datalav = readxl::read_excel(file_lavaan$datapath)
    }
    
    #nomalize
    normlav <- data.matrix(data.frame(unclass(datalav)))
    normlav
    
    corrdatalav = cor(normlav)
    corrdatalav
    
    finallav <- data.frame(normlav)
    finallav
    
    get_source <-read.csv(text = input$indikator, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source <- unlist(get_source)
    
    
    get_target <-read.csv(text = input$label, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target <- unlist(get_target)
    
    
    matrix_inner = cbind(get_target, get_source)
    colnames(matrix_inner) = c("Indikator", "Label")
    
    
    get_source_ind <-read.csv(text = input$label2, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source_ind <- unlist(get_source_ind)
    
    
    get_target_ind <-read.csv(text = input$label3, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target_ind <- unlist(get_target_ind)
    
    matrix_outer = cbind(get_source_ind, get_target_ind)
    colnames(matrix_outer) = c("Label", "Label")
    
    
    dframe1 <- paste(matrix_inner[, 1], matrix_inner[, 2], sep = '=~')
    
    
    dframe2 <- paste(matrix_outer[, 1], matrix_outer[, 2], sep = '~')
    
    
    modellav <- cbind(c(dframe1, dframe2))
    
    fitlav <- sem(modellav, data = finallav)
    fitlav
    
    summary(fitlav, standardized=TRUE)
  })#end of summary lavaan
  
  
  output$cetak_data7 <- renderPlot({
    file_lavaan <- input$file_lavaan
    ext <- tools::file_ext(file_lavaan$datapath)
    
    req(file_lavaan)
    #validate(need(ext == "csv", "Please upload a csv file"))
    
    if (is.null(file_lavaan))
      return(NULL)
    
    if(stringr::str_ends(file_lavaan$datapath, "csv")){
      datalav = read.csv(file_lavaan$datapath)
    } else if (stringr::str_ends(file_lavaan$datapath, "(xlsx|xls)")) {
      datalav = readxl::read_excel(file_lavaan$datapath)
    }
    
    #nomalize
    normlav <- data.matrix(data.frame(unclass(datalav)))
    normlav
    
    corrdatalav = cor(normlav)
    corrdatalav
    
    finallav <- data.frame(normlav)
    finallav
    
    get_source <-read.csv(text = input$indikator, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source <- unlist(get_source)
    
    
    get_target <-read.csv(text = input$label, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target <- unlist(get_target)
    
    
    matrix_inner = cbind(get_target, get_source)
    colnames(matrix_inner) = c("Indikator", "Label")
    
    
    get_source_ind <-read.csv(text = input$label2, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_source_ind <- unlist(get_source_ind)
    
    
    get_target_ind <-read.csv(text = input$label3, header = FALSE, sep = "", na.strings = c("", "NA"))
    get_target_ind <- unlist(get_target_ind)
    
    matrix_outer = cbind(get_source_ind, get_target_ind)
    colnames(matrix_outer) = c("Label", "Label")
    
    
    dframe1 <- paste(matrix_inner[, 1], matrix_inner[, 2], sep = '=~')
    
    
    dframe2 <- paste(matrix_outer[, 1], matrix_outer[, 2], sep = '~')
    
    
    modellav <- cbind(c(dframe1, dframe2))
    
    fitlav <- sem(modellav, data = finallav)
    fitlav
    
    semPaths(fitlav, what = "std", whatLabels = "std", rotation=2,sizeMan=8,
             sizeMan2 = 5, sizeLat = 12, sizeLat2 = 5,nCharNodes=0, 
             nCharEdges=0, residuals = FALSE,
             style="lisrel",curvePivot=TRUE, edge.color="black", 
             intercepts = FALSE, edge.label.cex = 0.9)
    title("SEM Analysis with Lavaan")
  },  height = 800, width = 800) #end of lavaan plot
  
  output$down_lavaan <- downloadHandler(
    #specify filename
    filename = function() {
      #biplot.png
      #biplot.pdf
      paste("semplot","png", sep = ".")
    }, 
    content = function(file) {
      #open the device
      #create plot
      #close the device
      #png()
      #pdf()
      #if(input$type == "png")
      png(file)
      #else
      #  pdf(file)
      
      file_lavaan <- input$file_lavaan
      ext <- tools::file_ext(file_lavaan$datapath)
      
      req(file_lavaan)
      #validate(need(ext == "csv", "Please upload a csv file"))
      
      if (is.null(file_lavaan))
        return(NULL)
      
      if(stringr::str_ends(file_lavaan$datapath, "csv")){
        datalav = read.csv(file_lavaan$datapath)
      } else if (stringr::str_ends(file_lavaan$datapath, "(xlsx|xls)")) {
        datalav = readxl::read_excel(file_lavaan$datapath)
      }
      
      #nomalize
      normlav <- data.matrix(data.frame(unclass(datalav)))
      normlav
      
      corrdatalav = cor(normlav)
      corrdatalav
      
      finallav <- data.frame(normlav)
      finallav
      
      get_source <-read.csv(text = input$indikator, header = FALSE, sep = "", na.strings = c("", "NA"))
      get_source <- unlist(get_source)
      
      
      get_target <-read.csv(text = input$label, header = FALSE, sep = "", na.strings = c("", "NA"))
      get_target <- unlist(get_target)
      
      
      matrix_inner = cbind(get_target, get_source)
      colnames(matrix_inner) = c("Indikator", "Label")
      
      
      get_source_ind <-read.csv(text = input$label2, header = FALSE, sep = "", na.strings = c("", "NA"))
      get_source_ind <- unlist(get_source_ind)
      
      
      get_target_ind <-read.csv(text = input$label3, header = FALSE, sep = "", na.strings = c("", "NA"))
      get_target_ind <- unlist(get_target_ind)
      
      matrix_outer = cbind(get_source_ind, get_target_ind)
      colnames(matrix_outer) = c("Label", "Label")
      
      
      dframe1 <- paste(matrix_inner[, 1], matrix_inner[, 2], sep = '=~')
      
      
      dframe2 <- paste(matrix_outer[, 1], matrix_outer[, 2], sep = '~')
      
      
      modellav <- cbind(c(dframe1, dframe2))
      
      fitlav <- sem(modellav, data = finallav)
      fitlav
      
      semPaths(fitlav, what = "std", whatLabels = "std", rotation=2,sizeMan=8,
               sizeMan2 = 5, sizeLat = 12, sizeLat2 = 5,nCharNodes=0, 
               nCharEdges=0, residuals = FALSE,
               style="lisrel",curvePivot=TRUE, edge.color="black", 
               intercepts = FALSE, edge.label.cex = 0.9)
      title("SEM Analysis with Lavaan")
      
      dev.off()
    }
  ) #end of download button for lavaan

})
