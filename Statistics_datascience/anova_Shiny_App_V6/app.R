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
    DT::dataTableOutput("txt2", width = "800px"),
    verbatimTextOutput("txt3"),
    verbatimTextOutput("txt4"),
#    DT::dataTableOutput("txt5", width = "800px")
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
    #################### Compute Anova and Tukey ##############
    ###############################################################
    
    anova_all <- eventReactive(input$run_button, {
        dat <- donnees()
        if (input$num_var_1 != not_sel && input$fact_var != not_sel) {
            aov(formula = dat[[input$num_var_1]] ~ dat[[input$fact_var]], data = donnees())
        }
    })
    
    df_anova <- eventReactive(input$run_button, {
        if (!is.null(anova_all())) {
            broom::tidy(anova_all())
        }
    })
    
    Tuk <- eventReactive(input$run_button, {
        if (!is.null(anova_all())) {
            TukeyHSD(anova_all())
        }
    })
    
    df_Tuk <- eventReactive(input$run_button, {
        if (!is.null(Tuk())) {
            broom::tidy(Tuk())
        }
    })
    
    ###################################################################################################################   
    ###################################################################################################################   
    
    
    date_anova <- eventReactive(input$run_button, {
        dat <- donnees()
        if (input$num_var_1 != not_sel && input$fact_var != not_sel && input$num_var_2 != not_sel) {
                   dat %>%
                      group_by(dat[[input$num_var_2]]) %>%
                          do(aov_mdl = summary(aov(.[[input$num_var_1]]~.[[input$fact_var]], data = donnees())))
        }
    })    
    
    
    
    
    tidy_date_anova <- eventReactive(input$run_button, {
        dat <- donnees()
        if (!is.null(date_anova())) {
            tidy(date_anova())
        }
    })  
    
    

    ##############################################################
    #################### Generate output ##########################
    ##############################################################
    
    
    # txt3 = text
    output$txt3 <- renderPrint({
        if (!is.null(tidy_date_anova())) {
            result_t <- tidy_date_anova()
            for (i in seq_along(result_t)) {
                cat("Group:", result_t$`dat[[input$num_var_2]]`[[i]], "\n")
                print(result_t$aov_mdl[[i]])
            }
        }
    })
    
    


    
    # txt5 = text
    output$txt5 <- renderDataTable({
        if (!is.null(date_anova())) {
            result <- date_anova()
            datatable_list <- list()
            for (i in seq_along(result)) {
                group <- result$`dat[[input$num_var_2]]`[[i]]
                datatable <- DT::datatable(broom::tidy(result$aov_mdl[[i]]))
                datatable_list[[group]] <- datatable
            }
            datatable_list
        }
    })
    
    
    

    
    
########################################### working do not touch #####################################################    
        
    output$txt1 <- DT::renderDataTable({
        if (!is.null(df_anova())) {
            DT::datatable(df_anova(),  extensions = c("Buttons"), options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
        }
    })
    
    
    
    
    output$txt2 <- DT::renderDataTable({
        if (!is.null(df_Tuk())) {
            DT::datatable(df_Tuk(), extensions = c("Buttons"), options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
            
        }
    })
    
    
    output$txt3 <- renderPrint({
        if (!is.null(anova_all())) {
            print(anova_all())
        }
    })
    
    output$txt4 <- renderPrint({
        if (!is.null(Tuk())) {
            print(Tuk())
        }
    })
    

    
}

shinyApp(ui, server)
