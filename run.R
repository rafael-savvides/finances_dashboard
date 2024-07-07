library(shiny) |>
    suppressPackageStartupMessages() |>
    suppressWarnings()

args = commandArgs(trailingOnly = TRUE)
path_to_data = args[1] # csv or rds
path_to_categories = args[2] # json {category: [terms]}

shinyOptions(
    path_to_data = path_to_data,
    path_to_categories = path_to_categories
)

runApp("R/app.R", launch.browser = TRUE)
