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

not_sel <- "Not Selected"

ui <- fluidPage(
    fileInput("xls_file", "Choose Excel File", accept = c(".xls", ".xlsx")),
    selectInput("fact_var", "Factor Variable", choices = c(), selected = NULL, multiple = FALSE),
    selectInput("fact_var_2", "Additional Factor Variable 2", choices = c(), selected = NULL, multiple = FALSE),
#    selectInput("fact_var_3", "Additional Factor Variable 3", choices = c(), selected = NULL, multiple = FALSE),
    actionButton("run_button", "Run"),
    DT::dataTableOutput("txt2", width = "800px"),
    DT::dataTableOutput("txt3", width = "800px"),
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
        data
    })
    
    ##############################################################
    #################### Creates the order input rearranger ######
    ##############################################################
    
    observe({
        req(donnees())
        choices <- c(not_sel, colnames(donnees()))
        updateSelectInput(inputId = "fact_var", choices = choices)
        updateSelectInput(inputId = "fact_var_2", choices = choices)
        updateSelectInput(inputId = "fact_var_3", choices = choices)
    })
    
    fact_var <- eventReactive(input$run_button, {
        req(input$fact_var)
        input$fact_var
    })
    
    fact_var_2 <- eventReactive(input$run_button, {
        req(input$fact_var_2)
        input$fact_var_2
    })
    
    fact_var_3 <- eventReactive(input$run_button, {
        req(input$fact_var_3)
        input$fact_var_3
    })
    
    ########################################################################################
    #################### Compute Descriptive Statistics ####################################
    ########################################################################################
    
    des_stat <- eventReactive(input$run_button, {
        pastecs::stat.desc(donnees(), hist = TRUE,  norm = TRUE)
    })
    
    
    
    des_stat_s <- eventReactive(input$run_button, {
        fact_var_val <- input$fact_var
        if (fact_var_val != not_sel) {
            donnees() %>%
                group_by_at(fact_var_val) %>%
                summarise(across(everything(), ~ pastecs::stat.desc(.x, norm = TRUE)), .groups = "drop")
        } else {
            NULL
        }
    })
    
    
    des_stat_t <- eventReactive(input$run_button, {
        fact_var_val <- input$fact_var
        fact_var_val_2 <- input$fact_var_2
        if (!is.null(fact_var_val) && !is.null(fact_var_val_2)) {
            donnees() %>%
                group_by_at(c(fact_var_val, fact_var_val_2)) %>%
                summarise(across(everything(), ~ pastecs::stat.desc(.x, norm = TRUE)), .groups = "drop")
        } else {
            NULL
        }
    })
    
    #######################################################################################
    #################### Generate output ##################################################
    #######################################################################################
    
    output$txt2 <- DT::renderDataTable({
        df <- donnees()
        if (!is.null(df)) {
            DT::datatable(des_stat(), class = "compact", width = "800px",
                          rownames = c("number of values",
                                       "number of NULL values",
                                       "number of NA values",
                                       "minimum value",
                                       "maximum value",
                                       "difference b/w min and max",
                                       "sum of the values",
                                       "median",
                                       "arithmetic mean",
                                       "standard error of the mean",
                                       "95% CI of the mean",
                                       "variance",
                                       "standard deviation",
                                       "variation coefficient",
                                       "skewness",
                                       "skew.2SE",
                                       "kurtosis",
                                       "kurt.2SE",
                                       "Shapiro-Wilk Norm test stat (W)",
                                       "Shapiro-Wilk Norm test (P-value)"
                          )
            )
        }
    })
    
    output$txt3 <- DT::renderDataTable({
        df <- donnees()
        if (!is.null(df)) {
            if (!is.null(des_stat_s())) {
                DT::datatable(des_stat_s(), class = "compact", width = "800px",
                              rownames = c("number of values",
                                           "number of NULL values",
                                           "number of NA values",
                                           "minimum value",
                                           "maximum value",
                                           "difference b/w min and max",
                                           "sum of the values",
                                           "median",
                                           "arithmetic mean",
                                           "standard error of the mean",
                                           "95% CI of the mean",
                                           "variance",
                                           "standard deviation",
                                           "variation coefficient",
                                           "skewness",
                                           "skew.2SE",
                                           "kurtosis",
                                           "kurt.2SE",
                                           "Shapiro-Wilk Norm test stat (W)",
                                           "Shapiro-Wilk Norm test (P-value)"
                              )
                              
                              
                              
                              
                )
            }
        }
    })
    
    
    
    output$txt4 <- DT::renderDataTable({
        df <- donnees()
        if (!is.null(df)) {
            if (!is.null(des_stat_t())) {
                DT::datatable(des_stat_t(), class = "compact", width = "800px",
                              rownames = c("number of values",
                                           "number of NULL values",
                                           "number of NA values",
                                           "minimum value",
                                           "maximum value",
                                           "difference b/w min and max",
                                           "sum of the values",
                                           "median",
                                           "arithmetic mean",
                                           "standard error of the mean",
                                           "95% CI of the mean",
                                           "variance",
                                           "standard deviation",
                                           "variation coefficient",
                                           "skewness",
                                           "skew.2SE",
                                           "kurtosis",
                                           "kurt.2SE",
                                           "Shapiro-Wilk Norm test stat (W)",
                                           "Shapiro-Wilk Norm test (P-value)"
                              )
                              
                )
            }
        }
    })
    
    
}

shinyApp(ui, server)
