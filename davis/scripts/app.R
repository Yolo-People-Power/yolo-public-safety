library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)
library(DT)
library(shinythemes)
library(shinyWidgets)
library(lubridate)

# setwd("/Users/bapu/Projects/watershed/action/public-safety/yolo/analysis/davis/")
# calls_desc <- read.csv("./data/generated/service-calls.csv")

ui <- fluidPage(
  titlePanel("Davis calls for service"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("date", "Date range",
                     min = as.Date("2015-01-01", "%Y-%m-%d"),
                     max = as.Date("2019-12-31", "%Y-%m-%d"),
                     value = c(as.Date("2015-01-01", "%Y-%m-%d"),
                               as.Date("2019-12-31", "%Y-%m-%d")),
                     timeFormat = "%Y-%m-%d",
                  dragRange = TRUE),
                 pickerInput("broad_cat", 
                             "Broad Category", 
                             choices = levels(calls_desc$broad_cat),
                             selected = c("Accidents/injuries",
                                          "Alcohol/drugs"),
                             options = list(
                               `actions-box` = TRUE,
                               size = 10,
                               `selected-text-format` = "count > 3"
                               ),
                               multiple = TRUE
                             )
                 # checkboxGroupInput("race", "Race", 
                 #                    choices = levels(davis_daily_log$race),
                 #                    selected = "White"),
                 # checkboxGroupInput("category", "Category", 
                 #                    choices = levels(davis_daily_log$category),
                 #                    selected = "Alcohol-related incidents"),
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
    ))
  )
  
server <- function(input,output) {
  #dateView <- seq(input$date[1], input$date[2], by = 1)
  # selectedValues <- reactive({
  #   dateView <- seq(input$date[1], input$date[2], by = 1)
  #   # data.frame(
  #   #   Name = c("Date", "Severity", "Race", "Category"),
  #   #   Value = as.character(c(paste(input$date, collapse = " "),
  #   #                          input$broad_cat,
  #   #                          # ,
  #   #                          # input$race,
  #   #                          # input$category)),
  #   #   stringsAsFactors = FALSE)
  #   # ))
  #   calls_desc %>%
  #     filter(
  #       date %in% dateView &
  #              broad_cat %in% input$broad_cat
  # #            # &
  # #            #   race %in% input$race &
  # #            #   category %in% input$category)
  # )})
  
  output$results_plot <- renderPlot({
    calls_desc %>%
      filter(broad_cat == input$broad_cat,
             date == as.Date(date) %in% 
               seq(ymd(as.Date(input$date[1])), 
                                   ymd(as.Date(input$date[2])), by = 1)) %>%
    ggplot(calls_desc, aes(broad_cat)) +
            geom_bar(stat = "count")
      # + geom_smooth(aes(x=date, y =n, color = race, 
      #                 linetype = severity), se = FALSE) 

  })
    

  # observeEvent(input$toggle, {
  #   toggle("results_table")
  # })
  
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