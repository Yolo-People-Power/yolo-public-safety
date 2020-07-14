library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)
library(DT)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Davis arrest log, 6/2015-6/2020"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date", "Date range",
                     min = as.Date("2015-06-02", "%Y-%m-%d"),
                     max = as.Date("2020-06-16", "%Y-%m-%d"),
                     value = c(as.Date("2015-06-02", "%Y-%m-%d"), 
                               as.Date("2020-06-16", "%Y-%m-%d")),
                     timeFormat = "%Y-%m-%d",
                  dragRange = TRUE),
                 checkboxGroupInput("severity", "Severity", 
                                    choices = 
                                      levels(davis_daily_log$severity),
                                    selected = "Felony"),
                 checkboxGroupInput("race", "Race", 
                                    choices = levels(davis_daily_log$race),
                                    selected = "White"),
                 checkboxGroupInput("category", "Category", 
                                    choices = levels(davis_daily_log$category),
                                    selected = "Alcohol-related incidents"),
    ),
    mainPanel(
      plotOutput("results_plot"),
      br(),
      br(),
      actionButton("toggle", "Hide/show table"),
      tableOutput("values")
      # br(),
      # br(),
      # DT::dataTableOutput("downloadData", "Download")
    )
  ))
  
server <- function(input,output) {
  
  selectedValues <- reactive({
    dateView <- seq(input$date[1], input$date[2], by = 1)
    # data.frame(
    #   Name = c("Date", "Severity", "Race", "Category"),
    #   Value = as.character(c(paste(input$date, collapse = " "),
    #                          input$severity,
    #                          input$race,
    #                          input$category)),
    #   stringsAsFactors = FALSE)
    # ) 
    davis_daily_log %>%
      filter(date %in% dateView &
               severity %in% input$severity &
               race %in% input$race &
               category %in% input$category)
  })
  
  output$results_plot <- renderPlot({
    ggplot(selectedValues()) +
            geom_col(aes(x=date, y=n,
                         color = race,
                         fill = race)) 
      # + geom_smooth(aes(x=date, y =n, color = race, 
      #                 linetype = severity), se = FALSE) 
  

  })
  
  observeEvent(input$toggle, {
    toggle("results_table")
  })
  
  # output$downloadData <- downloadHandler (
  #   filename = function() {
  #     paste("Selected_results", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(totals_filtered(), file, row.names = FALSE)
  #   }
  # )
  
}


shinyApp(ui, server)