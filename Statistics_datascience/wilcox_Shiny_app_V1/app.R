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
    
    #verbatimTextOutput("txt3"),
    
    #DT::dataTableOutput("txt4", width = "800px")
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
    
    ###############################################################
    #################### Compute kruskal ##############
    ###############################################################
    
    wilcox_test_all <- eventReactive(input$run_button, {
        dat <- donnees()
        if (input$num_var_1 != not_sel && input$fact_var != not_sel) {
            wilcox.test(dat[[input$num_var_1]] ~ dat[[input$fact_var]], data = donnees(), paired = F)
        }
    })
    
    df_wilcox_test_all <- eventReactive(input$run_button, {
        if (!is.null(wilcox_test_all())) {
            broom::tidy(wilcox_test_all())
        }
    })
    

    ##############################################################
    #################### Generate output ##########################
    #############################################################
    
    output$txt1 <- DT::renderDataTable({
        if (!is.null(df_wilcox_test_all())) {
            DT::datatable(df_wilcox_test_all(), extensions = c("Buttons"), options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
        }
    })
    
    output$txt2 <- renderPrint({
        if (!is.null(wilcox_test_all())) {
            print(wilcox_test_all())
        }
    })
    

}

shinyApp(ui, server)

