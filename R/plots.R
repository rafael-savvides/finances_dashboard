require(ggplot2)
theme_set(theme_light())

#' Template for time series plots
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_time_series <- function(data) {
  ggplot(data) +
    theme(rect = element_blank()) +
    scale_x_date() +
    scale_y_continuous(labels = scales::dollar_format(suffix = "eur", prefix = "")) +
    xlab("") + ylab("")
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
plot_daily_expenses <- function(data, show_categories = F) {
  plt = data %>%
    filter_expenses %>%
    plot_time_series()
  if (show_categories) plt + geom_col(aes(date, amount, fill=category)) else plt + geom_col(aes(date, amount))
  #TODO have consistent colors for categories. Order them according to amount.
}
