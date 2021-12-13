source("utils.R")
require(ggplot2)
theme_set(theme_light())

clr_fill = make_fill_palette(if (exists("bank")) bank else NULL)

#' Template for time series plots
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
gg_time_series <- function(data) {
  #TODO Change default color to match shiny theme (dark blue instead of grey).
  ggplot(data) +
    theme(rect = element_blank()) +
    scale_y_continuous(labels = scales::dollar_format(suffix = "eur", prefix = "")) +
    scale_fill_manual(values = clr_fill) +
    labs(x="", y="", fill="")
}

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
  fig = data %>%
    keep_only_expenses() %>%
    gg_time_series() +
    scale_x_date()
  if (show_categories) {
    fig + geom_col(aes(date, amount, fill=category), position="stack") +
      theme(legend.position = "bottom")
  } else {
    fig + geom_col(aes(date, amount))
  }
  #TODO have consistent colors for categories. Order them according to amount.
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_month_year <- function(data, show_categ=FALSE) {
  plt = data %>%
    keep_only_expenses() %>%
    group_by(year = year(date), month = month(date), category) %>%
    summarise(total = sum(amount)) %>%
    gg_time_series() +
    geom_col(aes(month, total, fill = as.factor(year)), position = "dodge") +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.x = element_blank(), legend.position = "bottom") +
    labs(fill = "Year")
  if (show_categ)
    plt + facet_wrap(category~.)
  plt
}

