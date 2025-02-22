options(dplyr.summarise.inform = FALSE)
{
  library(shiny)
  library(plotly)
  require(shinythemes)
  source("load_data.R")
  source("tables.R")
  source("utils.R")
  source("plots.R")
} |>
  suppressPackageStartupMessages() |>
  suppressWarnings()

path_to_data = getShinyOption("path_to_data", default = "")
path_to_categories = getShinyOption("path_to_categories", default = "")

bank = read_data(path_to_data, required_columns = c("date", "amount", "payer", "receiver"))

categories = if (file.exists(path_to_categories)) read_categories(path_to_categories) else list()
bank = add_category_column(bank, categories)
category_names = as.list(unique(bank$category))
names(category_names) = category_names

fill_palette = make_fill_palette(category_names = category_names, years = unique(year(bank$date)))

ui <- fluidPage(
  theme = shinytheme("flatly"),
  navbarPage(
    "Finances",
    tabPanel(
      "Daily",
      verticalLayout(
        plotlyOutput("plot_ts"),
        hr(),
        fluidRow(
          column(
            width = 4,
            checkboxInput("show_categ", "Show categories", FALSE),
            checkboxGroupInput(
              "categ",
              inline = TRUE,
              label = "Choose categories:",
              choices = category_names,
              selected = category_names
            )
          ),
          column(
            width = 2,
            dateInput("date_start",
              label = "From",
              value = max(bank$date) - 7
            )
          ),
          column(
            width = 2,
            dateInput("date_end",
              label = "To",
              value = max(bank$date)
            )
          )
        ),
        hr(),
        verbatimTextOutput("print_summary"),
        hr(),
        dataTableOutput("table_expenses")
      )
    ),
    tabPanel(
      "Compare months per year",
      verticalLayout(
        plotlyOutput("plot_monthly"),
        hr(),
        fixedRow(
          column(
            width = 12,
            checkboxInput("show_categ_monthly", "Show categories", FALSE),
            checkboxGroupInput("categ_monthly",
              inline = TRUE,
              label = "Choose categories:",
              choices = category_names,
              selected = category_names
            )
          ),
          hr(),
          dataTableOutput("table_monthly")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  output$plot_ts <- renderPlotly({
    fig = bank |>
      filter(
        between(date, input$date_start, input$date_end),
        category %in% input$categ
      ) |>
      plot_daily_expenses(show_categories = input$show_categ) +
      scale_fill_manual(values = fill_palette)
    ggplotly(fig)
  })

  output$table_expenses <- renderDataTable({
    table_expenses(
      data = bank,
      date_start = input$date_start,
      date_end = input$date_end,
      categ = input$categ
    )
  })

  output$print_summary = renderPrint({
    print_summary(
      data = bank,
      date_start = input$date_start,
      date_end = input$date_end,
      categ = input$categ
    ) |>
      cat()
  })

  output$plot_monthly <- renderPlotly({
    fig = bank |>
      filter(category %in% input$categ_monthly) |>
      plot_month_year() +
      scale_fill_manual(values = fill_palette)
    ggplotly(fig)
  })

  output$table_monthly <- renderDataTable(
    {
      bank |>
        filter(category %in% input$categ_monthly) |>
        table_aggregate(group_categ = input$show_categ_monthly)
    }
  )
}

shinyApp(ui, server)
