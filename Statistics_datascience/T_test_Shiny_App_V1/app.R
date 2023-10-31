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
    
#    DT::dataTableOutput("txt3", width = "800px"),
    
#    verbatimTextOutput("txt4")
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
    #################### Compute t test##############
    ###############################################################
    
    Ttest_all <- eventReactive(input$run_button, {
        dat <- donnees()
        if (input$num_var_1 != not_sel && input$fact_var != not_sel) {
            t.test(formula = dat[[input$num_var_1]] ~ dat[[input$fact_var]], data = donnees(), alternative = "two.sided", var.equal = T, paired = F)
        }
    })
    
    df_Ttest_all <- eventReactive(input$run_button, {
        if (!is.null(Ttest_all())) {
            broom::tidy(Ttest_all())
        }
    })
    
    
    
    
    
    
    
    
    ###################################################################################################################   
    ###################################################################################################################   
    
    
    date_Ttest <- eventReactive(input$run_button, {
        dat <- donnees()
        if (input$num_var_1 != not_sel && input$fact_var != not_sel && input$num_var_2 != not_sel) {
            dat %>%
                group_by(dat[[input$num_var_2]]) %>%
                do(aov2_mdl = summary(t.test(.[[input$num_var_1]]~.[[input$fact_var]], data = donnees(), alternative = "two.sided", var.equal = T, paired = F)))
        }
    })    
    
    
    
    
    tidy_date_Ttest <- eventReactive(input$run_button, {
        dat <- donnees()
        if (!is.null(date_Ttest())) {
            (tidy(date_Ttest))
        }
    })  
    

    
    ##############################################################
    #################### Generate output ##########################
    #############################################################
    
    
    
    output$txt1 <- DT::renderDataTable({
        if (!is.null(df_Ttest_all())) {
            DT::datatable(df_Ttest_all())
        }
    })
    
    output$txt2 <- renderPrint({
        if (!is.null(Ttest_all())) {
            print(Ttest_all())
        }
    })
    
    
    output$txt3 <- DT::renderDataTable({
        if (!is.null(tidy_date_Ttest ())) {
            DT::datatable(tidy_date_Ttest ())
        }
    })
    
    
    
    
#    output$txt4 <- renderPrint({
#        if (!is.null(date_Ttest())) {
#            print(date_Ttest())
#        }
#    })
    

    
    output$txt3 <- renderPrint({
        if (!is.null(tidy_date_Ttest())) {
            result_t <- tidy_date_Ttest()
            for (i in seq_along(result_t)) {
                cat("Group:", result_t$`dat[[input$num_var_2]]`[[i]], "\n")
                print(result_t$aov2_mdl[[i]])
            }
        }
    })
    
    
    
}

shinyApp(ui, server)
