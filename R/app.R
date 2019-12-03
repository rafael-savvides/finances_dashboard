library(shiny)

source("R/load_data.R")
source("R/plots.R")
data = read_spankki_all()
categories = read_categories()
data = add_category_column(data, categories)
time_periods = expand.grid(mon = month.abb[unique(month(data$date))], yr = unique(year(data$date))) %>%
  mutate(mon_yr = paste(mon, yr)) %>%
  pull(mon_yr)

ui <- fluidPage(
  verticalLayout(
    titlePanel("Finances"),
    plotOutput("plot1"),
    fluidRow(
      column(width=4,
             dateInput('date_start',
                       label = 'Start: yyyy-mm-dd',
                       value = max(data$date) - 7
             )),
      column(width=4,
             dateInput('date_end',
                       label = 'End: yyyy-mm-dd',
                       value = max(data$date)
             )),
      column(width=4,
             selectInput('date_period', "Select time period",
                         choices = time_periods #TODO Make this change date_start and date_end. https://stackoverflow.com/questions/38884084/shiny-r-reset-other-input-values-when-selectinput-value-changes
             )),
    ),
    navlistPanel(
      "Extra",
      tabPanel("Compare categories",
               checkboxInput("show_categ", "Show categories", FALSE),
               checkboxGroupInput("categ",
                                  label = "Choose categories:",
                                  choiceNames = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None"),
                                  choiceValues = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", ""),
                                  selected = list("Supermarket", "Food", "Out", "Service", "Product", "Other", "Rent", "None")
               )
      ),
      tabPanel("Aggregate",
               fluidRow(selectInput("aggregate", "Choose aggregation", choices = list("Year", "Month")),
                        selectInput("aggregate2", "Choose aggregation2", choices = list("Year", "Month")))
      ),

      tabPanel("Compare years, months",
               checkboxGroupInput("compare_years", "Choose year or month",
                                  choices = list("Year", "Month", "Week"))
      ),

      tabPanel("Show top10 purchases",
              tableOutput("table1")
      )
    )
  )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    data %>%
      dplyr::filter(between(date, input$date_start, input$date_end)) %>%
      filter(category %in% input$categ) %>%
      plot_daily_expenses(show_categories = input$show_categ)
  })

  output$table1 <- renderTable({
    data %>%
      filter_expenses %>%
      select(date, amount, receiver, message, category) %>%
      dplyr::filter(between(date, input$date_start, input$date_end)) %>%
      top_n(n=10, amount) %>%
      arrange(-amount) %>%
      mutate(date = format(date,'%Y-%m-%d'))
  })
}

shinyApp(ui, server)
