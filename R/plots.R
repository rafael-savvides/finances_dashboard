{
  library(dplyr)
  library(stringr)
  library(purrr)
  library(lubridate)
  require(ggplot2)
  library(tidyr)
} |>
  suppressPackageStartupMessages() |>
  suppressWarnings()
theme_set(theme_light())

source("utils.R")

#' Time series plot of daily expenses
#'
#' Notes: Internal payments (between my accounts) are omitted.
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_daily_expenses <- function(data, show_categories = FALSE) {
  fig = data |>
    keep_only_expenses() |>
    gg_time_series() +
    scale_x_date()
  if (show_categories) {
    fig + geom_col(aes(date, amount, fill = category), position = "stack") +
      theme(legend.position = "bottom")
  } else {
    fig + geom_col(aes(date, amount))
  }
  # TODO have consistent colors for categories. Order them according to amount.
}

#' Bar plot of expenses aggregated over month and year
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_month_year <- function(data) {
  months_years = expand.grid(
    year = unique(year(data$date)),
    month = unique(month(data$date)),
    is_expense = c(TRUE, FALSE)
  ) |>
    arrange(year, month)


  df = data |>
    as_tibble() |>
    mutate(is_expense = amount < 0,
           amount = abs(amount)) |>
    filter(tolower(payer) != tolower(receiver)) |>
    group_by(year = year(date), month = month(date), is_expense) |>
    summarise(total = sum(amount)) |>
    # Fill all combinations of year-month.
    right_join(months_years, by=c("year", "month", "is_expense")) |>
    mutate(total = replace_na(total, 0)) |>
    arrange(year, month)

  plt = df  |>
    gg_time_series() +
    geom_col(
      aes(month, total, fill = year, group = year),
      width = 0.1,
      color = NA,
      position = position_dodge(width = 1),
      data = df |>
        filter(!is_expense) |>
        mutate(year = as.factor(year))
    ) +
    geom_col(
      aes(month, total, fill = year),
      position = position_dodge(width = 1),
      data = df |>
        filter(is_expense) |>
        mutate(year = as.factor(year))
    )+
    geom_point(
      aes(month, total, group = year),
      size = 0.5,
      shape = 3,
      alpha = 0.5,
      position = position_dodge(width = 1),
      data = df |>
        filter(!is_expense) |>
        mutate(year = as.factor(year))
    ) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom"
    ) +
    labs(fill = "Year", title="Expenses (thick) and income (thin bars)")
  plt
}

#' Template for time series plots
#'
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
gg_time_series <- function(data) {
  # TODO Change default color to match shiny theme (dark blue instead of grey).
  ggplot(data) +
    theme(rect = element_blank()) +
    scale_y_continuous(labels = scales::dollar_format(suffix = "eur", prefix = "")) +
    labs(x = "", y = "", fill = "")
}
