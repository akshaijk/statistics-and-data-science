library(shiny)
library(readxl)
library(DT)
library(shinythemes)
library(svDialogs)
library(ggplot2)
library(data.table)
library(dplyr)
library(tidyverse)
library(shinyjqui)
library(pastecs)
library(hms)
library(car)
library(FSA)

not_sel <- "Not Selected"

ui <- fluidPage(
    fileInput("xls_file", "Choose Excel File", accept = c(".xls", ".xlsx")),
    selectInput("num_var_1", "Numerical Variable", choices = c(not_sel)),
    selectInput("num_var_2", "Colour factor", choices = c(not_sel)),
    selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
    actionButton("run_button", "Run Analysis", icon = icon("play")),
    DT::dataTableOutput("txt2", width = "800px"),
    DT::dataTableOutput("txt3", width = "800px"),
    verbatimTextOutput("txt")
)

server <- function(input, output, session) {
    
    ##############################################################
    #################### open excel sheet as DF ##################
    ##############################################################
    
    donnees <- reactive({
        req(input$xls_file)
        inFile <- input$xls_file
        if (is.null(inFile))
            return(NULL)
        data <- read_excel(inFile$datapath)
        data
    })
    
    ##############################################################
    #################### Creates the order input rearranger ######
    ##############################################################
    
    observe({
        req(donnees())
        choices <- c(not_sel, colnames(donnees()))
        updateSelectInput(inputId = "num_var_1", choices = choices)
        updateSelectInput(inputId = "num_var_2", choices = choices)
        updateSelectInput(inputId = "fact_var", choices = choices)
    })
    
    ##############################################################
    #################### Compute Variance Parameters ##############
    ##############################################################
    
    lev <- eventReactive(input$run_button, {
        dat <- donnees()
        if (input$num_var_1 != not_sel && input$fact_var != not_sel) {
            leveneTest(
                y = dat[[input$num_var_1]],
                group = as.factor(dat[[input$fact_var]])
            )
        }
    })
    
    
    # generates Lev test seprated by the second factor variable (Date in the example)
    
    lev3 <- eventReactive(input$run_button, {
        dat <- donnees()
        if (input$num_var_1 != not_sel && input$fact_var != not_sel && input$num_var_2 != not_sel) {
            assign(x = paste("Stats_all_", input$fact_var, sep = ""), 
                   dat %>%
                       group_by(dat[[input$num_var_2]]) %>%
                       do(leveneTest(y = .[[input$num_var_1]], group = .[[input$fact_var]]))
            )
        }
    })
    
    ##############################################################
    #################### Generate output ##########################
    ##############################################################
    
    output$txt <- renderPrint({
        if (!is.null(lev3())) {
            print(lev3())
        }
    })
    
    output$txt2 <- DT::renderDataTable({
        if (!is.null(lev())) {
            DT::datatable(lev())
        }
    })
    
    output$txt3 <- DT::renderDataTable({
        if (!is.null(lev3())) {
            DT::datatable(lev3())
        }
    })
}

shinyApp(ui, server)
