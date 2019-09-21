if(!require(Lock5Data)){
  install.packages("Lock5Data")
  library(Lock5Data)
}
if(!require(tools)){
  install.packages("tools")
  library(tools)
}
library(ggplot2)
library(dplyr)


get_plot_limits <- function(plot) {
  gb = ggplot_build(plot)
  xmin = gb$layout$panel_params[[1]]$x.range[1]
  xmax = gb$layout$panel_params[[1]]$x.range[2]
  ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
}

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
                  tabPanel("Data Set",
                           fluidRow(
                             column(4,
                                    selectInput("data", "Select a data set from the Lock 5 textbook:", lock.data)),
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
                               textOutput("single.var.name"),
                               br(),
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
                                            "smooth", "Add a smoother?",value = FALSE))),
                           mainPanel(tabsetPanel(type = "tabs",
                                                 tabPanel("Plot",  plotOutput("bi.plot", height = "600px")),
                                                 tabPanel("Summary", verbatimTextOutput("bi.var.name"))))
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
        # create the table
        x.var <- as.numeric(names(table(get.var())))
        y.var <- c(table(get.var()))
        
        # expand based on the table
        x.var <- rep(x.var, y.var)
        y.var <- unlist(sapply(y.var, seq))
        tmp <- data.frame(x.var = x.var,
                          y.var = y.var)
        plot(y.var ~ x.var, tmp, pch = 16, col = "dodgerblue",
             ylab = "Count", xlab = names(get.var()),
             main = paste("Dotplot of the", names(get.var()), "variable."))
      } else{
        plot(get.var(), ylab = "Count", xlab = names(get.var()),
             main = paste("Barchart of the", names(get.var()), "variable."))
      }
    })
    
    output$single.var.name <- renderText({
      paste("Descriptive statistics for the ", names(get.var()), "variable.")
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
          geom_point() +
          xlab(names(dat.tmp)[1]) +
          ylab(names(dat.tmp)[2])
        if(input$regline){
          coefs <- coef(lm(dat.tmp[, 2] ~ dat.tmp[, 1]))
          g0 <- g0 + geom_abline(intercept = coefs[1], slope = coefs[2]) #+ 
        }
        if(input$smooth){
          g0 <- g0 + geom_smooth()
        }
        if(input$cor){
          cor <- cor(dat.tmp[, 1], dat.tmp[, 2], use = "pairwise.complete.obs")
          g0 <- g0 + geom_label(x = (max(dat.tmp[, 1]) + min(dat.tmp[, 1])) / 2, y = (max(dat.tmp[, 2]) + min(dat.tmp[, 2])) / 2, label = paste0("r =", round(cor, 2)),
                                label.size = 1)
        }
        print(g0)
      } else if(types == 0){
          ggplot(dat.tmp, aes(dat.tmp[, 1])) + 
            geom_bar(aes(fill = dat.tmp[, 2]), col = "black", position = 'dodge', alpha = .6) +
            theme(legend.title = element_blank()) + xlab(names(dat.tmp)[1])
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
    output$bi.var.name <- renderText({
          paste("Descriptive statistics for the ", input$plot.type)
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
  }
    )
  )
  
  