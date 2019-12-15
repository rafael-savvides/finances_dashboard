library(shiny)
library(plotly)
require(shinythemes)
source("R/load_data.R")
source("R/plots.R")
source("R/tables.R")
source("R/utils.R")
data = read_all()
categories = read_categories()
data = add_category_column(data, categories)

time_periods = expand.grid(mon = month.abb[unique(month(data$date))], yr = unique(year(data$date))) %>%
  mutate(mon_yr = paste(mon, yr)) %>%
  pull(mon_yr)

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage("Finances",
             tabPanel("Daily",
                      verticalLayout(
                        plotOutput("plot_ts"),
                        hr(),
                        fluidRow(
                          column(width=4,
                                 checkboxInput("show_categ", "Show categories", FALSE),
                                 checkboxGroupInput("categ", inline=T,
                                                    label = "Choose categories:",
                                                    choiceNames = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None"),
                                                    choiceValues = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None"),
                                                    selected = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None")
                                 )
                          ),
                          column(width=2,
                                 dateInput('date_start',
                                           label = 'From',
                                           value = max(data$date) - 7
                                 )),
                          column(width=2,
                                 dateInput('date_end',
                                           label = 'To',
                                           value = max(data$date)
                                 ))
                        ),
                        hr(),
                        navlistPanel(widths = c(2, 10),
                          "",
                          tabPanel("Show top purchases",
                                   sliderInput("n_top", "n", min=1, max=100, value=10),
                                   tableOutput("table_top")
                          ),
                          tabPanel("Search",
                                   fluidRow(
                                     textInput("text_search", "", placeholder = "Enter a search term"),
                                     h3(textOutput("text_search_result"))
                                     ),
                                   tableOutput("table_search")
                          )
                        )
                      )
             ),
             tabPanel("Monthly",
                      verticalLayout(
                        plotOutput("plot_monthly"),
                        hr(),
                        fixedRow(
                          column(width=12,
                                 checkboxInput("show_categ_monthly", "Show categories", FALSE),
                                 checkboxGroupInput("categ_monthly", inline=T,
                                                    label = "Choose categories:",
                                                    choiceNames = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None"),
                                                    choiceValues = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None"),
                                                    selected = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None")
                                 )
                          ),
                          hr(),
                          tableOutput("table_monthly")
                        )
                      )
             ),
             tabPanel("Weekly",
                      verticalLayout(
                        plotOutput("plot_weekly"),
                        hr(),
                        fluidRow(
                          column(width=12,
                                 checkboxInput("show_categ_weekly", "Show categories", FALSE),
                                 checkboxGroupInput("categ_weekly", inline=T,
                                                    label = "Choose categories:",
                                                    choiceNames = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None"),
                                                    choiceValues = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None"),
                                                    selected = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None")
                                 )
                          )
                        )
                      )
             )
  )
)


server <- function(input, output, session) {
  output$plot_ts <- renderPlot({
    data %>%
      dplyr::filter(between(date, input$date_start, input$date_end)) %>%
      filter(category %in% input$categ) %>%
      plot_daily_expenses(show_categories = input$show_categ)
  })

  output$table_top <- renderTable({
    table_top_purchases(data, input$date_start, input$date_end, input$categ, n_top = input$n_top)
  })

  output$table_search <- renderTable({
    table_search(search_term = input$text_search,
                 data = data,
                 date_start = input$date_start,
                 date_end = input$date_end,
                 categ = input$categ)
  })

  output$plot_monthly <- renderPlot({
    data %>%
      filter(category %in% input$categ_monthly) %>%
      plot_month_year(show_categ = input$show_categ_monthly) #TODO Fix this. Faceting to multiple rows doesn't work.
  })

  output$table_monthly <- renderTable({
    data %>%
      filter(category %in% input$categ_monthly) %>%
      table_aggregate(group_categ = input$show_categ_monthly)
  })

  output$plot_weekly <- renderPlot({
    data %>%
      filter(category %in% input$categ_weekly) %>%
      plot_weekdays
  })

  output$text_search_result <- renderText({
    res = table_search(search_term = input$text_search,
                       data = data,
                       date_start = input$date_start,
                       date_end = input$date_end,
                       categ = input$categ) %>%
      summarise(total = sum(amount)) %>%
      pull(total)

    paste0("Total spending: ", res)
  })
}

shinyApp(ui, server)
