  library(shiny)
  library(tidyverse)
  library(shinyjs)
  library(DT)
  library(shinythemes)
  library(shinyWidgets)
  library(lubridate)
  
  # READ DATA----
    # Calls
  calls_shiny <- read.csv("calls_sum.csv", colClasses = c(
    "character","factor","factor","integer"))
  calls_shiny$date <- as.Date(calls_shiny$date)
    # Arrests
  arrests <- read.csv("arrests.csv", colClasses = c(
                        "Date", "factor", "factor", "integer", 
                        "factor", "factor", "factor"),
                      na.strings = "")
      # Set severity order
  arrests <- arrests %>% arrange(severity) %>%
    mutate(severity = factor(severity, levels = 
                               c("Felony [3]", "Felony [2], Misdemeanor [1]",
                                 "Felony [2]", "Felony [1], Misdemeanor [2]",
                                 "Felony [1], Misdemeanor [1]", "Felony [1]",            
                                 "Misdemeanor [3]", "Misdemeanor [2]",
                                 "Misdemeanor [1]", "Unclear/Unknown")))
    # Charges
  charges <- read.csv("charges.csv", colClasses = c(
    "factor", "Date", "factor", "factor", "integer", 
    "factor", "factor", "factor" ))
    
  # USER INTERFACE----
  ui <- fluidPage(
  # Sidebar----
    useShinyjs(),       # For table toggle button
    theme = shinytheme("cosmo"),
    titlePanel("Davis Public Safety Data"),
    sidebarLayout(
      sidebarPanel(
        div(tags$b(radioButtons("model", "Dataset",
                     choices = c("Service calls", 
                                 "Arrests",
                                 "Charges")), 
            style = "font-size: 150%")),
        br(),
        # Service calls
        conditionalPanel("input.model == 'Service calls'",
          sliderInput("dateView", "Date range",
                      min = as.Date("2015-01-01"),
                      max = as.Date("2019-12-31"),
                      value = c(as.Date("2015-01-01"), 
                                as.Date("2019-12-31")),
                      dragRange = TRUE),
          radioButtons("stat", "Statistic",
                       choices = c("Count", "Percentage"),
                       selected = "Count"),
          checkboxGroupInput("disp_desc", 
                             "Outcome (Disposition)", 
                             choices = levels(calls_shiny$disp_desc),
                             selected = levels(calls_shiny$disp_desc)),
          actionButton("selectall", "Select/Deselect All")
      ),
        # Arrests
        conditionalPanel("input.model == 'Arrests'",
                         sliderInput("dateView_arrests", "Date range",
                                     min = as.Date("2015-06-02"),
                                     max = as.Date("2020-06-16"),
                                     value = c(as.Date("2015-06-02"),
                                               as.Date("2020-06-16")),
                                     dragRange = T),
                         radioButtons("stat_arrests", "Statistic",
                                      choices = c("Count", "Percentage"),
                                      selected = "Count"),
                         sliderInput("ageView_arrests", "Age",
                                     min = 18, max = 73,
                                     value = c(18,73),
                                     dragRange = T),
                         checkboxGroupInput("race_arrests", "Race",
                                            choices = levels(arrests$race),
                                            selected = levels(arrests$race)),
                         actionButton("selectall_arrests_race", 
                                      "Select/Deselect All"),
                         br(),
                         br(),
                         checkboxGroupInput("sev_arrests", 
                                            "Severity [# of charges]",
                                            choices = levels(arrests$severity),
                                            selected = levels(arrests$severity)),
                         actionButton("selectall_arrests_sev", 
                                      "Select/Deselect All")
                         ),
      # Charges
      conditionalPanel("input.model == 'Charges'",
                       sliderInput("dateView_charges", "Date range",
                                   min = as.Date("2015-06-02"),
                                   max = as.Date("2020-06-16"),
                                   value = c(as.Date("2015-06-02"),
                                             as.Date("2020-06-16")),
                                   dragRange = T),
                       radioButtons("stat_charges", "Statistic",
                                    choices = c("Count", "Percentage"),
                                    selected = "Count"),
                       sliderInput("ageView_charges", "Age",
                                   min = 18, max = 73,
                                   value = c(18,73),
                                   dragRange = T),
                       checkboxGroupInput("race_charges", "Race",
                                          choices = levels(charges$race),
                                          selected = levels(charges$race)),
                       actionButton("selectall_charges_race", 
                                    "Select/Deselect All"),
                       br(),
                       br(),
                       checkboxGroupInput("sev_charges", 
                                          "Severity",
                                          choices = levels(charges$severity_1),
                                          selected = levels(
                                            charges$severity_1)),
                       actionButton("selectall_charges_sev", 
                                    "Select/Deselect All"),
                       br(),
                       br(),
                       checkboxGroupInput("cat_charges",
                                          "Category",
                                          choices = levels(charges$category),
                                          selected = levels(charges$category)),
                       actionButton("selectall_charges_cat",
                                    "Select/Deselect All")
      )
      ),
      
      # Main panel----
      mainPanel(
        conditionalPanel(
          #Service calls
          "input.model == 'Service calls'",
        plotOutput("results_plot", height = 700),
        br(),
        br(),
        actionButton("toggle1", "Hide/show table"),
        downloadButton("downloadData", "Download table"),
        br(),
        br(),
        div(DT::dataTableOutput("calls_table"), 
            style = "font-size: 75%; height: 50%")
      ),
      conditionalPanel(
        # Arrests
        "input.model == 'Arrests'",
        tabsetPanel(type = "tabs",
                    tabPanel("Arrests by race", 
                             plotOutput("arrests_plot_race", height = 700),
                             br(), br(),
                             actionButton("toggle2", "Hide/show table"),
                             downloadButton("downloadData_arrests_race", 
                                            "Download table"),
                             br(),br(),
                             div(DT::dataTableOutput("arrests_race_table"), 
                                 style = "font-size: 75%; height: 50%")),
                    tabPanel("Arrests by severity",
                             plotOutput("arrests_plot_sev", height = 700),
                             br(), br(),
                             actionButton("toggle3", "Hide/show table"),
                             downloadButton("downloadData_arrests_sev",
                                            "Download table"),
                             br(),br(),
                             div(DT::dataTableOutput("arrests_sev_table"),
                                 style = "font-size: 75%; height: 50%")
                             ),
                    tabPanel("Arrests by date",
                             plotOutput("arrests_plot_date", height = 700),
                             br(), br(),
                             actionButton("toggle4", "Hide/show table"),
                             downloadButton("downloadData_arrests_date",
                                            "Download table"),
                             br(), br(),
                             div(DT::dataTableOutput("arrests_date_table"),
                                 stype = "font-size: 75%; height: 50%")
                             )
                    )
        ),
      conditionalPanel(
        # Charges
        "input.model == 'Charges'",
        tabsetPanel(type = "tabs",
                    tabPanel("Charges by race", 
                             plotOutput("charges_plot_race", height = 700),
                             br(), br(),
                             actionButton("toggle5", "Hide/show table"),
                             downloadButton("downloadData_charges_race", 
                                            "Download table"),
                             br(),br(),
                             div(DT::dataTableOutput("charges_race_table"), 
                                 style = "font-size: 75%; height: 50%")),
                    tabPanel("Charges by category",
                             plotOutput("charges_plot_cat", height = 700),
                             br(), br(),
                             actionButton("toggle6", "Hide/show table"),
                             downloadButton("downloadData_charges_sev",
                                            "Download table"),
                             br(),br(),
                             div(DT::dataTableOutput("charges_cat_table"),
                                 style = "font-size: 75%; height: 50%")
                    ),
                    tabPanel("Charges by date",
                             plotOutput("charges_plot_date", height = 700),
                             br(), br(),
                             actionButton("toggle7", "Hide/show table"),
                             downloadButton("downloadData_charges_date",
                                            "Download table"),
                             br(), br(),
                             div(DT::dataTableOutput("charges_date_table"),
                                 stype = "font-size: 75%; height: 50%")
                    )
        )
      )
    )
    )
  )
  
  
  # SERVER----
    
  server <- function(input, output, session) {
    
    # Observes----
      # Service calls
    observe({
      if(input$selectall > 0) {
       if (input$selectall %% 2 == 0) {
        updateCheckboxGroupInput(session = session, 
                                 "disp_desc", "Outcome (Disposition)",
                                 choices = levels(calls_shiny$disp_desc),
                                 selected = c(levels(calls_shiny$disp_desc)))
    
      } else {
        updateCheckboxGroupInput(session = session, 
                                 "disp_desc", "Outcome (Disposition)",
                                 choices = levels(calls_shiny$disp_desc),
                                 selected = c())
      }}
    })
    
      # Arrests
        # Race
    observe({
      if(input$selectall_arrests_race > 0) {
        if (input$selectall_arrests_race %% 2 == 0) {
          updateCheckboxGroupInput(session = session, 
                                   "race_arrests", "Race",
                                   choices = levels(arrests$race),
                                   selected = c(levels(arrests$race)))
          
        } else {
          updateCheckboxGroupInput(session = session, 
                                   "race_arrests", "Race",
                                   choices = levels(arrests$race),
                                   selected = c())
        }}
    })
        # Severity
    observe({
      if(input$selectall_arrests_sev > 0) {
        if (input$selectall_arrests_sev %% 2 == 0) {
          updateCheckboxGroupInput(session = session, 
                                   "sev_arrests", "Severity [# of charges]",
                                   choices = levels(arrests$severity),
                                   selected = c(levels(arrests$severity)))
          
        } else {
          updateCheckboxGroupInput(session = session, 
                                   "sev_arrests", "Severity [# of charges]",
                                   choices = levels(arrests$severity),
                                   selected = c())
        }}
    })
    
      # Charges
    # Race
    observe({
      if(input$selectall_charges_race > 0) {
        if (input$selectall_charges_race %% 2 == 0) {
          updateCheckboxGroupInput(session = session, 
                                   "race_charges", "Race",
                                   choices = levels(charges$race),
                                   selected = c(levels(charges$race)))
          
        } else {
          updateCheckboxGroupInput(session = session, 
                                   "race_charges", "Race",
                                   choices = levels(charges$race),
                                   selected = c())
        }}
    })
    # Severity
    observe({
      if(input$selectall_charges_sev > 0) {
        if (input$selectall_charges_sev %% 2 == 0) {
          updateCheckboxGroupInput(session = session, 
                                   "sev_charges", "Severity",
                                   choices = levels(charges$severity),
                                   selected = c(levels(charges$severity)))
          
        } else {
          updateCheckboxGroupInput(session = session, 
                                   "sev_charges", "Severity",
                                   choices = levels(charges$severity),
                                   selected = c())
        }}
    })
    
    # Category
    observe({
      if(input$selectall_charges_cat > 0) {
        if (input$selectall_charges_cat %% 2 == 0) {
          updateCheckboxGroupInput(session = session, 
                                   "cat_charges", "category",
                                   choices = levels(charges$category),
                                   selected = c(levels(charges$category)))
          
        } else {
          updateCheckboxGroupInput(session = session, 
                                   "cat_charges", "category",
                                   choices = levels(charges$category),
                                   selected = c())
        }}
    })
    
    # Reactives----
    
    calls_filtered <- reactive({
    dateView <- seq(as.Date(input$dateView[1]),
                    as.Date(input$dateView[2]),
                    by = "days")
      calls_shiny %>% 
        filter(as.Date(date) %in% dateView &
                 disp_desc %in% input$disp_desc)
    })
    
    arrests_filtered <- reactive({
      dateView_arrests <- seq(as.Date(input$dateView_arrests[1]),
                      as.Date(input$dateView_arrests[2]),
                      by = "days")
      ageView_arrests <- seq(input$ageView_arrests[1], 
                             input$ageView_arrests[2])
      arrests %>%
        filter(as.Date(date) %in% dateView_arrests &
                 age %in% ageView_arrests &
                 race %in% input$race_arrests &
                 severity %in% input$sev_arrests)
    })

    charges_filtered <- reactive({
      dateView_charges <- seq(as.Date(input$dateView_charges[1]),
                              as.Date(input$dateView_charges[2]),
                              by = "days")
      ageView_charges <- seq(input$ageView_charges[1], 
                             input$ageView_charges[2])
      charges %>%
        filter(as.Date(date) %in% dateView_charges &
                 age %in% ageView_charges &
                 race %in% input$race_charges &
                 severity_1 %in% input$sev_charges &
                 category %in% input$cat_charges)
    })
    
    # Output tables-----
    
      # Calls
    
    observeEvent(input$toggle1, {
      toggle("calls_table")
    })
    
    output$calls_table <- DT::renderDataTable({
      calls_filtered()
    }, colnames = c("Date", "Category", "Outcome", "Number of Calls"))
    
      # Arrests
    
    observeEvent(input$toggle2, {
      toggle("arrests_race_table")
    })
    
    observeEvent(input$toggle3, {
      toggle("arrests_sev_table")
    })
    
    observeEvent(input$toggle4, {
      toggle("arrests_date_table")
    })
    
    output$arrests_race_table <- DT::renderDataTable({
      arrests_filtered()
    }, colnames = c("Arrest ID", "Date", "Sex", "Race", "Age", "Severity", 
                    "Section Codes", "Charges"))
    
    output$arrests_sev_table <- DT::renderDataTable({
      arrests_filtered()
    }, colnames = c("Arrest ID", "Date", "Sex", "Race", "Age", "Severity", 
                    "Section Codes", "Charges"))
    
    output$arrests_date_table <- DT::renderDataTable({
      arrests_filtered()
    }, colnames = c("Arrest ID", "Date", "Sex", "Race", "Age", "Severity", 
                    "Section Codes", "Charges"))
    
    # Charges
    
    observeEvent(input$toggle5, {
      toggle("charges_race_table")
    })
    
    observeEvent(input$toggle6, {
      toggle("charges_cat_table")
    })
    
    observeEvent(input$toggle7, {
      toggle("charges_date_table")
    })
    
  
    output$charges_race_table <- DT::renderDataTable({
      charges_filtered()
    }, colnames = c("Individual ID", "Date", "Sex", "Race", "Age", 
                  "Section Codes", "Charges", 
                  "Severity", "Category"))
    
    output$charges_cat_table <- DT::renderDataTable({
      charges_filtered()
    }, colnames = c("Individual ID", "Date", "Sex", "Race", "Age", 
                    "Section Codes", "Charges", 
                    "Severity", "Category"))
    
    output$charges_date_table <- DT::renderDataTable({
      charges_filtered()
    }, colnames = c("Individual ID", "Date", "Sex", "Race", "Age", 
                    "Section Codes", "Charges", 
                    "Severity", "Category"))


      # Arrests

    # Output plots----
    
      # Service calls
    output$results_plot <- renderPlot({
      if (input$stat == "Count") {
      ggplot(calls_filtered()) +
        geom_bar(aes(x = broad_cat,
                     y = n,
                     fill = broad_cat,
                     color = broad_cat),
                 stat = "identity") +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          stat_summary(
            aes(x = broad_cat,
                y = n,
                label = stat(y)), 
            fun = "sum", geom = "text", vjust = -0.5, size = 4.5) +
          theme(panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1, 
                                         size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17),
                legend.position = "none") +
        ylab("# of calls") + xlab("Call category") 
     } else {
       ggplot(calls_filtered()) +
         geom_bar(aes(x = broad_cat,
                      y = n*100/sum(n),
                      fill = broad_cat,
                      color = broad_cat),
                  stat = "identity") +
         stat_summary(
           aes(x = broad_cat,
               y = n*100/sum(n),
               label = paste(stat(round(y, digits = 1)), "%", sep ="")), 
           fun = "sum", geom = "text", vjust = -0.5, size = 4.5) +
         scale_color_viridis_d() +
         scale_fill_viridis_d() +
         theme(panel.grid.major = element_blank(), 
               panel.grid.minor = element_blank(),
               panel.background = element_rect(fill = "white"),
               axis.ticks.x = element_blank(),
               axis.text.x = element_text(angle = 90, hjust = 1, 
                                          size = 15),
               axis.text.y = element_text(size = 13),
               axis.title = element_text(size = 17),
               legend.position = "none") +
         ylab("% of calls") + xlab("Call category") 
     }
      
    })
    
    # Arrests
      # By race
    output$arrests_plot_race <- renderPlot({
      if (input$stat_arrests == "Count") {
    ggplot(arrests_filtered()) +
        geom_bar(aes(x = race,
                   fill = race,
                   color = race),
                 stat = "count") +
        scale_color_viridis_d() +
        scale_fill_viridis_d() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1,
                                         size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17),
                legend.position = "none") +
        ylab("# of arrests") + xlab("Race") +
        geom_text(stat = "count",
                  aes(x = race, label = ..count..), 
                  vjust = -0.5, size = 4.5)
      } else {
        ggplot(arrests_filtered()) +
          geom_bar(aes(x = race,
                       y = (..count..)*100/sum(..count..),
                       fill = race,
                       color = race),
                   stat = "count") +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1,
                                           size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17),
                legend.position = "none") +
          ylab("% of arrests") + xlab("Race") +
          geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                        y = (..count..)*100/sum(..count..),
                        x = race),
                    stat = "count",
                    vjust = -.5, size = 4.5)
      }
    })
    
    # By severity
    output$arrests_plot_sev <- renderPlot({
      if (input$stat_arrests == "Count") {
        ggplot(arrests_filtered()) +
          geom_bar(aes(x = severity,
                       fill = severity,
                       color = severity),
                   stat = "count") +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1,
                                           size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17),
                legend.position = "none") +
          ylab("# of arrests")  + xlab("") +
          geom_text(stat = "count",
                    aes(x = severity, label = ..count..), 
                    vjust = -0.5, size = 4.5)
      } else {
        ggplot(arrests_filtered()) +
          geom_bar(aes(x = severity,
                       y = (..count..)*100/sum(..count..),
                       fill = severity,
                       color = severity),
                   stat = "count") +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1,
                                           size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17, vjust = -.5),
                legend.position = "none") +
          ylab("% of arrests") + xlab("") +
          geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                        y = (..count..)*100/sum(..count..),
                        x = severity),
                    stat = "count",
                    vjust = -.5, size = 4.5)
      }
    })
    
    # By date
    output$arrests_plot_date <- renderPlot({
      if (input$stat_arrests == "Count") {
        ggplot(arrests_filtered()) +
          geom_bar(aes(x = date,
                       y = (..count..)),
                   stat = "bin",
                   color = "#404788FF", 
                   fill = "#453781FF",
                   binwidth = 31) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.text.x = element_text(angle = 90,
                                           size = 14, vjust = .5,
                                           margin = margin (r = 0)),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17),
                legend.position = "none") +
          ylab("# of arrests/month")  + xlab("") +
          scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y",
                       expand = c(0,0)) +
          scale_y_continuous(expand = c(0.01,0))
        
      } else {
        ggplot(arrests_filtered()) +
          geom_bar(aes(x = date,
                       y = (..count..)*100/sum(..count..)),
                   stat = "bin",
                   color = "#404788FF", 
                   fill = "#453781FF",
                   binwidth = 31) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90,
                                           size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17, vjust = -.5),
                legend.position = "none") +
          ylab("% of all arrests in dataset, by month") + xlab("") +
          scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y",
                       expand = c(0,0)) +
          scale_y_continuous(expand=c(0.01,0))
      }
    })
    
    # Charges
    
      # By race
    output$charges_plot_race <- renderPlot({
      if (input$stat_charges == "Count") {
        ggplot(charges_filtered()) +
          geom_bar(aes(x = race,
                       fill = race,
                       color = race),
                   stat = "count") +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1,
                                           size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17),
                legend.position = "none") +
          ylab("# of charges") + xlab("Race") +
          geom_text(stat = "count",
                    aes(x = race, label = ..count..), 
                    vjust = -0.5, size = 4.5)
      } else {
        ggplot(charges_filtered()) +
          geom_bar(aes(x = race,
                       y = (..count..)*100/sum(..count..),
                       fill = race,
                       color = race),
                   stat = "count") +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1,
                                           size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17),
                legend.position = "none") +
          ylab("% of charges") + xlab("Race") +
          geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                        y = (..count..)*100/sum(..count..),
                        x = race),
                    stat = "count",
                    vjust = -.5, size = 4.5)
      }
    })
    
    # By category
    output$charges_plot_cat <- renderPlot({
      if (input$stat_charges == "Count") {
        ggplot(charges_filtered()) +
          geom_bar(aes(x = category,
                       fill = category,
                       color = category),
                   stat = "count") +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1,
                                           size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17),
                legend.position = "none") +
          ylab("# of charges")  + xlab("") +
          geom_text(stat = "count",
                    aes(x = category, label = ..count..), 
                    vjust = -0.5, size = 4.5)
      } else {
        ggplot(charges_filtered()) +
          geom_bar(aes(x = category,
                       y = (..count..)*100/sum(..count..),
                       fill = category,
                       color = category),
                   stat = "count") +
          scale_color_viridis_d() +
          scale_fill_viridis_d() +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90, hjust = 1,
                                           size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17, vjust = -.5),
                legend.position = "none") +
          ylab("% of charges") + xlab("") +
          geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                        y = (..count..)*100/sum(..count..),
                        x = category),
                    stat = "count",
                    vjust = -.5, size = 4.5)
      }
    })
    
    # By date
    output$charges_plot_date <- renderPlot({
      if (input$stat_charges == "Count") {
        ggplot(charges_filtered()) +
          geom_bar(aes(x = date,
                       y = (..count..)),
                   stat = "bin",
                   fill = "#20A387FF", 
                   color = "#1F968BFF",
                   binwidth = 31) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.text.x = element_text(angle = 90,
                                           size = 14, vjust = .5,
                                           margin = margin (r = 0)),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17),
                legend.position = "none") +
          ylab("# of charges/month")  + xlab("") +
          scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y",
                       expand = c(0,0)) +
          scale_y_continuous(expand = c(0.01,0))
        
      } else {
        ggplot(charges_filtered()) +
          geom_bar(aes(x = date,
                       y = (..count..)*100/sum(..count..)),
                   stat = "bin",
                   fill = "#20A387FF", 
                   color = "#1F968BFF",
                   binwidth = 31) +
          theme(panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                panel.background = element_rect(fill = "white"),
                axis.ticks.x = element_blank(),
                axis.text.x = element_text(angle = 90,
                                           size = 14, vjust = .5),
                axis.text.y = element_text(size = 13),
                axis.title = element_text(size = 17, vjust = -.5),
                legend.position = "none") +
          ylab("% of all charges in dataset, by month") + xlab("") +
          scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y",
                       expand = c(0,0)) +
          scale_y_continuous(expand=c(0.01,0))
      }
    })
    
    #Downloads----
    
      # Calls
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(calls_filtered(), file)
      }
    )
    
      # Arrests
        # Race
    output$downloadData_arrests_race <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(arrests_filtered(), file)
      }
    )
    
        # Severity
    output$downloadData_arrests_sev <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(arrests_filtered(), file)
      }
    )
    
        # Date
    output$downloadData_arrests_date <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(arrests_filtered(), file)
      }
    )
    
    # Charges
      # Race
    output$downloadData_charges_race <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(charges_filtered(), file)
      }
    )
    
      # Category
    output$downloadData_charges_cat <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(charges_filtered(), file)
      }
    )
    
    # Date
    output$downloadData_charges_date <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(charges_filtered(), file)
      }
    )
    
  }
      
  shinyApp(ui, server)