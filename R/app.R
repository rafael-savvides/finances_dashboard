options(dplyr.summarise.inform=FALSE)
suppressPackageStartupMessages({
  library(shiny)
  library(plotly)
  require(shinythemes)
  source("load_data.R")
  source("tables.R")
  source("utils.R")
  source("plots.R")
})
bank = read_all(readLines("../data/dir_bank.txt"))
categories = read_categories("../data/categories.json")
bank = add_category_column(bank, categories)

category_names = as.list(unique(bank$category))
names(category_names) = replace_na_category(category_names)
time_periods = expand.grid(
  mon = month.abb[unique(month(bank$date))],
  yr = unique(year(bank$date))) %>%
  mutate(mon_yr = paste(mon, yr)) %>%
  pull(mon_yr)

ui <- fluidPage(theme = shinytheme("flatly"),
  navbarPage(
    "Finances",
    tabPanel(
      "Daily",
      verticalLayout(
        plotlyOutput("plot_ts"),
        hr(),
        fluidRow(
          column(width=4,
                 checkboxInput("show_categ", "Show categories", FALSE),
                 checkboxGroupInput(
                   "categ", inline=TRUE,
                   label = "Choose categories:",
                   choices = category_names,
                   selected = category_names
                 )
          ),
          column(width=2,
                 dateInput('date_start',
                           label = 'From',
                           value = max(bank$date) - 7
                 )),
          column(width=2,
                 dateInput('date_end',
                           label = 'To',
                           value = max(bank$date)
                 ))
        ),
        hr(),
        textOutput("table_summary"),
        hr(),
        dataTableOutput("table_expenses")
      )
    ),
    tabPanel(
      "Compare months per year",
      verticalLayout(
        plotOutput("plot_monthly"),
        hr(),
        fixedRow(
          column(width=12,
                 checkboxInput("show_categ_monthly", "Show categories", FALSE),
                 checkboxGroupInput("categ_monthly", inline=T,
                                    label = "Choose categories:",
                                    choices = category_names,
                                    selected = category_names
                 )
          ),
          hr(),
          tableOutput("table_monthly")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  output$plot_ts <- renderPlotly({
    bank %>%
      filter(between(date, input$date_start, input$date_end)) %>%
      filter(category %in% input$categ) %>%
      mutate(category = replace_na_category(category)) %>%
      plot_daily_expenses(show_categories = input$show_categ) %>%
      ggplotly()
  })

  output$table_expenses <- renderDataTable({
    table_expenses(
      data = bank,
      date_start = input$date_start,
      date_end = input$date_end,
      categ = input$categ) %>%
      mutate(category = replace_na_category(category))
  })

  output$table_summary = renderPrint({
    table_summary(
      data = bank,
      date_start = input$date_start,
      date_end = input$date_end,
      categ = input$categ)
  })

  output$plot_monthly <- renderPlot({
    bank %>%
      filter(category %in% input$categ_monthly) %>%
      plot_month_year()
  })

  output$table_monthly <- renderTable({
    bank %>%
      filter(category %in% input$categ_monthly) %>%
      mutate(category = replace_na_category(category)) %>%
      table_aggregate(group_categ = input$show_categ_monthly)
  }, digits=0)
}

shinyApp(ui, server)
