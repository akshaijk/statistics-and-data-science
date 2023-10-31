library(tidyverse)
library(shiny)
library(shinyjqui)
library(readxl)
library(DT)
library(shinythemes)
library(svDialogs)
library(ggplot2)
library(data.table)
library(dplyr)

not_sel <- "Not Selected"

ui <- fluidPage(
  fileInput("xls_file", "Choose Excel File", accept = c(".xls", ".xlsx")),
  selectInput("num_var_1", "Numerical Variable", choices = c(not_sel)),
  selectInput("num_var_2", "Colour factor", choices = c(not_sel)),
  selectInput("fact_var", "Factor Variable", choices = c(not_sel)),
  orderInput(inputId = "levels", label = "Factor level order", items = c()),
  actionButton("run_button", "Run Analysis", icon = icon("play")),
  numericInput("y_min", "Y-axis Minimum", value = NULL, step = 1),
  numericInput("y_max", "Y-axis Maximum", value = NULL, step = 1),
  selectInput("output_format", "Output Format:", choices = c("png", "tiff", "pdf", "svg")),
  sliderInput("height", "Plot Height", min = 100, max = 5000, value = 500),
  sliderInput("width", "Plot Width", min = 100, max = 5000, value = 1000),
  plotOutput(outputId = "plot", brush = "plot_brush"),tableOutput("data"),
  downloadButton("down", "Download Dot Plot")
)

server <- function(input, output, session) {
  
  ######################################################################################################################################
  ########################## Reads the excel sheet as donnees ##########################################################################
  #####################################################################################################################################
  
  donnees <- reactive({
    req(input$xls_file)
    inFile <- input$xls_file
    if (is.null(inFile))
      return(NULL)
    data <- read_excel(inFile$datapath)
    
    Condition <- data$fact_var
    
    mutate(data, Condition = Condition %>% factor(levels = input$levels))
  })
  
  ##########################################################################################################################################  
  ####################updates the select button based on excel sheet####################################################################  
  #####################################################################################################################################  
  
  observe({
    req(donnees())
    choices <- c(not_sel, colnames(donnees()))
    updateSelectInput(inputId = "num_var_1", choices = choices)
    updateSelectInput(inputId = "num_var_2", choices = choices)
    updateSelectInput(inputId = "fact_var", choices = choices)
  })
  
  num_var_1 <- eventReactive(input$run_button, {
    req(input$num_var_1)
    input$num_var_1
  })
  
  num_var_2 <- eventReactive(input$run_button, {
    req(input$num_var_2)
    input$num_var_2
  })
  
  fact_var <- eventReactive(input$run_button, {
    req(input$fact_var)
    input$fact_var
  })
  
  
  pheight <-reactive({input$height})
  
  
  pwidth <- reactive({input$width})
  
  
  
  
  ######################################################################################################################################
  ############################ Creates the order input rearranger from Excel sheet##################################################### 
  #########################################################################################################################################
  
  factor_list <- eventReactive(input$run_button, {
    if (!is.null(input$xls_file) && !is.null(input$fact_var)) {
      file <- input$xls_file$datapath
      df <- read_excel(file)
      column <- input$fact_var
      factors <- unique(df[[column]])
      return(factors)
    }
  })
  
  output$factor_list <- renderPrint({
    if (!is.null(factor_list())) {
      factor_list()
    }
  })
  
  observeEvent(input$run_button,{
    choices <- c(factor_list())
    updateOrderInput(session, inputId = "levels", items = choices)
  })
  
  ##########################################################################################################################################  
  ################# plots the Graph ###################################################################################################
  ##########################################################################################################################################  
  
  plot1 <- reactive({
    ggplot(donnees(), aes_string(x = fact_var(), y = num_var_1())) +
      geom_dotplot(binaxis = 'y', stackdir = "center") +
      coord_cartesian(ylim = c(input$y_min, input$y_max))
  })
  
  output$plot <- renderPlot({ plot1() }, height = pheight , width = pwidth)
  
  output$data <- renderTable({
    df <- donnees()
    if (!is.null(df)) {
      brushedPoints(df, input$plot_brush)
    }
  })
  
  ##################### Download the dot plot #######################################################################
  #################################################################################################################  
  
  output$down <- downloadHandler(
    filename = function() {
      paste("dot_plot", input$output_format, sep = ".")
    },
    content = function(file) {
      if (input$output_format == "png")
        ggsave(file, plot = plot1(), width = input$width, height = input$height, dpi = 96, units = "px", device = "png")
      else if (input$output_format == "tiff")
        ggsave(file, plot = plot1(), width = input$width, height = input$height, dpi = 96, units = "px", device = "tiff")
      else if (input$output_format == "pdf")
        ggsave(file, plot = plot1(), width = input$width, height = input$height, dpi = 96, units = "px", device = "pdf")
      else if (input$output_format == "svg")
        ggsave(file, plot = plot1(), width = input$width, height = input$height, dpi = 96, units = "px", device = "svg")
    }
  )
}

shinyApp(ui, server)
