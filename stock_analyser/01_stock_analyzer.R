# Business Analytics with Data Science and Machine Learning ----
# Building Business Data Products ----
# STOCK ANALYZER APP - LAYOUT -----

# APPLICATION DESCRIPTION ----
# - Create a basic layout in shiny showing the stock dropdown, interactive plot 
#   and commentary.


# LIBRARIES ----
library(shiny)
library(shinyWidgets)

library(plotly)
library(tidyverse)

library(rvest)
library(glue)

library(quantmod)

source(file = "00_scripts/stock_analysis_functions.R")


# UI ----
ui <- fluidPage(
  title = "Stock Analyzer",
  
  # 1.0 HEADER ----
  
  div(
    h1("Stock Analzer"),
    p("Press 'Analyze' to remove Error Message!"),
  ),

  # 2.0 APPLICATION UI -----
  div(
    column(
      width = 4,
      wellPanel(
        pickerInput(inputId  = "index_selection",
                    label    = "Stock Index",
                    choices  = c("DOW", "DAX", "SP500", "NASDAQ"),
                    multiple = FALSE,
                    selected = NULL,
                    options  = list("actionsBox" = FALSE,
                                   "liveSearch"  = TRUE,
                                   "size"        = 10),
        ),
        uiOutput("indices"),
        dateRangeInput(inputId = "date_range", 
                       label   = "Date Range", 
                       start   = today() - days(180), 
                       end     = today()),
        actionButton(inputId = "analyze",
                     label   = "Analyze",
                     icon    = icon("download")),
        # textOutput(outputId = "selected_symbol")
        hr(),
        sliderInput(inputId = "mavg_short", 
                    label   = "Short Moving Average", 
                    min     = 5, 
                    max     = 40, 
                    value   = 20),
        sliderInput(inputId = "mavg_long", 
                    label   = "Long Moving Average", 
                    min     = 50, 
                    max     = 120, 
                    value   = 50),
      )
    ), 
    column(
      width = 8,
      div(h4(textOutput(outputId = "plot_header"))),
      # div(verbatimTextOutput(outputId = "stock_data")),
      div(
        plotlyOutput(outputId = "plotly_plot")
      )
    )
  ),

  # 3.0 ANALYST COMMENTARY ----
  div(
    column(
      width = 12,
      textOutput(outputId = "analyst_commentary")
    )
  )
)
  
# SERVER ----
server <- function(input, output, session) {
  
  # Create stock list ----    
  stock_list_tbl <- eventReactive(input$index_selection, {
    get_stock_list(input$index_selection)
    
  },
  ignoreNULL = FALSE,
  )
  
  output$indices <- renderUI({
    choices = stock_list_tbl() %>% purrr::pluck("label")
    pickerInput(inputId  = "stock_selection",
                label    = "Stocks",
                choices  = choices,
                multiple = FALSE,
                selected = NULL,
                options  = list("actionsBox" = FALSE,
                                "liveSearch" = TRUE,
                                "size"       = 10),
    )
  })
  
  # Stock Symbol ----
  stock_symbol <- eventReactive(
    input$analyze, {
      input$stock_selection
    },
    ignoreNULL = FALSE,
  )
  
  output$selected_symbol <- renderText({stock_symbol()})
  output$plot_header     <- renderText({stock_symbol()})
  
  # Get Stock Data ----
  stock_data_tbl <- reactive({
    
    stock_symbol() %>% 
      get_symbol_from_user_input()%>% 
      get_stock_data(from = input$date_range[1], 
                     to   = input$date_range[2],
                     mavg_short = input$mavg_short,
                     mavg_long  = input$mavg_long)
    
  })
  
  output$stock_data  <- renderPrint({stock_data_tbl()})
  output$plotly_plot <- renderPlotly({stock_data_tbl() %>% plot_stock_data()})
  output$analyst_commentary <- renderPrint({
    stock_data_tbl() %>% generate_commentary(user_input = stock_symbol() %>% 
                                               get_symbol_from_user_input())
  })
  
  
  
}

# RUN APP ----
shinyApp(ui = ui, server = server)

