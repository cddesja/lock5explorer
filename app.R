if(!require(devtools)){
  install.packages("devtools")
  library(devtools)
}

install_github("cddesja/Lock5Data")
library(Lock5Data)

if(!require(DT)){
  install.packages("DT")
  library(DT)
}

if(!require(tools)){
  install.packages("tools")
  library(tools)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)
}
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}

get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  data.frame(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


# get data helper function
getdata <- function(...) {
  e <- new.env()
  name <- data(..., envir = e)[1]
  e[[name]]
}

lock.data <- data(package = "Lock5Data")
lock.data <- lock.data$results[, "Item"]

shinyApp(
  ui = navbarPage("Lock 5 Data Explorer",
                  tabPanel("Dataset",
                           fluidRow(
                             column(4,
                                    selectInput("data", "Select a dataset from the Lock 5 textbook:", lock.data)),
                             column(8,
                                    uiOutput("documentation")))),
                  tabPanel("Data Table",
                           DT::dataTableOutput("dt")),
                  tabPanel("Single Variable",
                           fluidRow(
                             column(4,
                                    selectInput("single", label = "Select a variable", choices = names(ACS))),
                             column(8,
                               plotOutput("plot", height = "600px"),
                               verbatimTextOutput("sum.single")
                             )
                           )
                  ),
                  tabPanel("Multiple Variables",
                           sidebarPanel(selectInput("x1", label = "Select the X variable", choices = names(ACS)[1]),
                                        selectInput("x2", label = "Select the Y variable", choices = names(ACS)[2]),
                                        conditionalPanel(
                                          condition = "output.vartypes",
                                          selectInput(
                                            "plot.type", "Select the type of plot",
                                            c("Boxplot", "Dotplot", "Histogram")
                                          )),
                                        conditionalPanel(
                                          condition = "output.quant",
                                          checkboxInput(
                                            "cor", "Find the correlation?",value = FALSE),
                                          checkboxInput(
                                            "regline", "Find and plot the regression line?",value = FALSE),
                                          checkboxInput(
                                            "fixx", "Is the X variable suppose to be categorical?",value = FALSE),
                                          checkboxInput(
                                            "fixy", "Is the Y variable suppose to be categorical?",value = FALSE))),
                           mainPanel(tabsetPanel(type = "tabs",
                                                 tabPanel("Plot",  plotOutput("bi.plot", height = "600px")),
                                                 tabPanel("Summary", 
                                                          verbatimTextOutput("cor.out"),
                                                          verbatimTextOutput("reg.line"),
                                                          verbatimTextOutput("sum.bi"),
                                                          verbatimTextOutput("std.out"))))
                           )
                  )
  ,
  server = function(input, output, session) ({
    tmp <- tempfile()
    onSessionEnded(function(){ unlink(tmp) })
    
    app.data <- reactive({
      getdata(list = input$data)
    })
    
    output$vartypes <-reactive({
      dat.tmp <- get.xs()
      if((is.numeric(dat.tmp[, 1]) & !is.numeric(dat.tmp[,2])) | (is.numeric(dat.tmp[, 2]) & !is.numeric(dat.tmp[,1]))){
        TRUE
      } else {
        FALSE
      }
    })
    outputOptions(output, "vartypes", suspendWhenHidden = FALSE)
    
    output$quant <-reactive({
      dat.tmp <- get.xs()
      if((is.numeric(dat.tmp[, 1]) & is.numeric(dat.tmp[,2]))){
        TRUE
      } else {
        FALSE
      }
    })
    outputOptions(output, "quant", suspendWhenHidden = FALSE)
    

    get.var <- reactive({
      subset(app.data(), select = input$single)
    })
    
    get.xs <- reactive({
      subset(app.data(), select = c(input$x1, input$x2))
    })
    
    RdDatabase <- reactive({
      Rd_db("Lock5Data")
    })
    
    output$documentation <- renderUI({
      rdfile <- paste0(input$data, ".Rd")
      req(rdfile %in% names(RdDatabase()))
      Rd2HTML(RdDatabase()[[rdfile]], tmp, no_links = TRUE, package = "Lock5Data")
      includeHTML(tmp)
    })

    observe({
      updateSelectInput(session, "single", choices = names(app.data()))
      updateSelectInput(session, "x1", choices = names(app.data()), selected = names(app.data())[1])
      updateSelectInput(session, "x2", choices = names(app.data()), selected = names(app.data())[2])
    })

    
    output$dt <- DT::renderDataTable({
      DT::datatable(app.data())
    })

    output$plot <- renderPlot({
      
      
      if(is.numeric(unlist(get.var()))) {
        ggplot(get.var(), aes(x = get.var()[,1])) +
          geom_histogram(col = "white", fill = "#E69F00") + 
          xlab(names(get.var())[1])  +
          ylab("Count")
      } else{
        ggplot(get.var(), aes(x = get.var()[,1])) +
          geom_bar(col = "#E69F00", fill = "#E69F00") + 
          xlab(names(get.var())[1])  
      }
    })

    
    output$sum.single <- renderPrint({
      if(is.numeric(unlist(get.var()))) {
        summary(get.var())
      } else {
        table(get.var())
      }
    })
  
    output$bi.plot <- renderPlot({
      dat.tmp <- get.xs()
      types <- sum(is.numeric(dat.tmp[, 1]), is.numeric(dat.tmp[, 2]))
      
      # quantitative & quantitative plots ----
      if(types == 2){
        g0 <- ggplot(dat.tmp, aes(x = dat.tmp[, 1], y = dat.tmp[, 2])) +
          geom_point(size = 2, col = "#E69F00") +
          xlab(names(dat.tmp)[1]) +
          ylab(names(dat.tmp)[2])
        lims <- get_plot_limits(g0)
        
        if(input$regline){
          coefs <- coef(lm(dat.tmp[, 2] ~ dat.tmp[, 1]))
          g0 <- g0 + 
            geom_abline(intercept = coefs[1], slope = coefs[2])
        }
        
        print(g0)
      } else if(types == 0){
        ggplot(dat.tmp, aes(dat.tmp[, 1])) + 
          geom_bar(aes(fill = dat.tmp[, 2]), col = "black", position = 'dodge', alpha = .6) +
          theme(legend.title = element_blank()) +
          scale_fill_manual("", values = cbbPalette) +
          xlab(names(dat.tmp)[1]) +
          ylab("Count")
        # categorical & quantitative plots ----
      } else if(input$plot.type == "Boxplot"){
        if(is.numeric(dat.tmp[,1])){
          num = 1
          cat = 2
        } else {
          cat = 1
          num = 2
        }
        
        ggplot(dat.tmp, aes(y = dat.tmp[, num], x = dat.tmp[, cat])) +
          geom_boxplot() +
          xlab(names(dat.tmp)[cat]) +
          ylab(names(dat.tmp)[num])
      } else if(input$plot.type == "Dotplot"){
        
        if(is.numeric(dat.tmp[,1])){
          num = 1
          cat = 2
        } else {
          cat = 1
          num = 2
        }
        
        ggplot(dat.tmp, aes(x = dat.tmp[, num])) +
          geom_dotplot() + 
          facet_wrap(~ dat.tmp[, cat], scales = "free_y") +
          xlab(names(dat.tmp)[num])  +
          scale_y_continuous(NULL, breaks = NULL)
      } else if(input$plot.type == "Histogram"){
        
        if(is.numeric(dat.tmp[,1])){
          num = 1
          cat = 2
        } else {
          cat = 1
          num = 2
        }
        
        ggplot(dat.tmp, aes(x = dat.tmp[, num])) +
          geom_histogram(fill = "white", col = "black") + 
          facet_wrap(~ dat.tmp[, cat]) +
          xlab(names(dat.tmp)[num])  +
          scale_y_continuous(NULL, breaks = NULL)
      }
      
    })
    
    output$cor.out <- renderPrint({
      if(input$cor){
        dat.tmp <- get.xs()
        types <- sum(is.numeric(dat.tmp[, 1]), is.numeric(dat.tmp[, 2]))
        if(types == 2){
          cor.tmp <- cor(dat.tmp[, 1], dat.tmp[, 2], use = "pairwise.complete.obs")
          cat("The correlation coefficient is\nr =", round(cor.tmp, 3))
        }
      }
    })
    
    output$reg.line <- renderPrint({
      if(input$regline){
        dat.tmp <- get.xs()
        types <- sum(is.numeric(dat.tmp[, 1]), is.numeric(dat.tmp[, 2]))
        if(types == 2){
          coefs <- coef(lm(dat.tmp[, 2] ~ dat.tmp[, 1]))
          cat("The equation for the regression line is\na =", round(coefs[1], 3), "b =", round(coefs[2], 3))
        }
      }
    })
    
    output$sum.bi <- renderPrint({
      dat.tmp <- get.xs()
      if(is.numeric(dat.tmp[, 1]) & is.numeric(dat.tmp[, 2])){
        summary(dat.tmp)
      } else if(!is.numeric(dat.tmp[, 1]) & !is.numeric(dat.tmp[, 2])){
        table(dat.tmp)
      } else {
        if(is.numeric(dat.tmp[,1])){
          num = 1
          cat = 2
        } else {
          cat = 1
          num = 2
        }
        by(dat.tmp[num], dat.tmp[cat], summary)
          }
        })
    
    output$std.out <- renderPrint({
      dat.tmp <- get.xs()
      if(is.numeric(dat.tmp[, 1]) & is.numeric(dat.tmp[, 2])){
        #summary(dat.tmp)
      } else if(!is.numeric(dat.tmp[, 1]) & !is.numeric(dat.tmp[, 2])){
        #table(dat.tmp)
      } else {
        if(is.numeric(dat.tmp[,1])){
          num = 1
          cat = 2
        } else {
          cat = 1
          num = 2
        }
        grp <- by(dat.tmp[[num]], dat.tmp[[cat]], sd)
        cat("The standard deviations for each group are:\n")
        c(grp)
      }
    })
  }
    )
  )
  
  