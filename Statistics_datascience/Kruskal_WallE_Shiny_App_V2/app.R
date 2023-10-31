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
library(broom)
library(broom.mixed)
library(rstatix)

not_sel <- "Not Selected"

ui <- fluidPage(
    fileInput("xls_file", "Choose Excel File", accept = c(".xls", ".xlsx")),
    selectInput("num_var_1", "Numerical Variable", choices = c(not_sel)),
    selectInput("num_var_2", "Colour factor", choices = c(not_sel)),
    selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
    actionButton("run_button", "Run Analysis", icon = icon("play")),
    br(),  
    br(),
    br(),
    br(),
    br(),
    DT::dataTableOutput("txt1", width = "800px"),
    br(),
    br(),
    br(),
    br(),
    br(),
    
    verbatimTextOutput("txt2"),
    
    verbatimTextOutput("txt3"),
    
    DT::dataTableOutput("txt4", width = "800px")
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
        
        # Convert variables to numeric if they are characters
        num_var_cols <- input$num_var_1
        fact_var_col <- input$fact_var
        if (!is.null(num_var_cols) && num_var_cols != not_sel) {
            data <- data %>%
                mutate(across(all_of(num_var_cols), as.numeric))
        }
        if (!is.null(fact_var_col) && fact_var_col != not_sel) {
            data <- data %>%
                mutate({{fact_var_col}} := as.numeric({{fact_var_col}}))
        }
        
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
    
    ###############################################################
    #################### Compute kruskal ##############
    ###############################################################
    
    Kruskal_test_all <- eventReactive(input$run_button, {
        dat <- donnees()
        if (input$num_var_1 != not_sel && input$fact_var != not_sel) {
            kruskal.test(dat[[input$num_var_1]] ~ dat[[input$fact_var]], data = donnees())
        }
    })
    
    df_Kruskal_test_all <- eventReactive(input$run_button, {
        if (!is.null(Kruskal_test_all())) {
            broom::tidy(Kruskal_test_all())
        }
    })
    
    Dunn_test_all <- eventReactive(input$run_button, {
        if (input$num_var_1 != not_sel && input$fact_var != not_sel) {
            dat <- donnees()
            fact_var <- input$fact_var
            num_var <- input$num_var_1
            formula <- as.formula(paste0(num_var, " ~ ", fact_var))
            rstatix::dunn_test(data = dat, formula = formula, p.adjust.method = "holm")
        }
    })
    
    df_Dunn_test_all <- eventReactive(input$run_button, {
        if (!is.null(Dunn_test_all())) {
            broom::tidy(Dunn_test_all())
        }
    })
    
    ##############################################################
    #################### Generate output ##########################
    #############################################################
    
    output$txt1 <- DT::renderDataTable({
        if (!is.null(df_Kruskal_test_all())) {
            DT::datatable(df_Kruskal_test_all(), extensions = c("Buttons"), options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
        }
    })
    
    output$txt2 <- renderPrint({
        if (!is.null(Kruskal_test_all())) {
            print(Kruskal_test_all())
        }
    })
    
    output$txt3 <- renderPrint({
        if (!is.null(Dunn_test_all())) {
            print(Dunn_test_all())
        }
    })
    
    output$txt4 <- DT::renderDataTable({
        if (!is.null(df_Dunn_test_all())) {
            DT::datatable(df_Dunn_test_all(), extensions = c("Buttons"), options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
        }
    })
}

shinyApp(ui, server)

