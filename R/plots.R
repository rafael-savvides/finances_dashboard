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
  if (show_categories) {
    plt + geom_col(aes(date, amount, fill=category))
  } else {
    plt + geom_col(aes(date, amount))
  }
  #TODO have consistent colors for categories. Order them according to amount.
}

plot_weekends <- function(data, ...) {
  #TODO plot only weekends, without gaps between them
  data %>%
    filter(weekdays(date) %in% c("Saturday", "Sunday")) %>%
    plot_daily_expenses(...)
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_month_year <- function(data) {
  #TODO replace with table_monthly
  data %>%
    filter_expenses %>%
    group_by(year = year(date), month = month(date)) %>%
    summarise(total = sum(amount)) %>%
    plot_time_series() +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(fill = "Year") +
    geom_col(aes(month, total, fill = as.factor(year)), position = "dodge")
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_week_year <- function(data) {
  data %>%
    filter_expenses %>%
    group_by(week = week(date), year = year(date)) %>%
    summarise(total = sum(amount)) %>%
    plot_time_series() +
    scale_x_continuous(breaks = pretty(1:53)) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank()) +
    labs(fill = "Year", x = "Week") +
    geom_col(aes(week, total, fill = as.factor(year)), position = "dodge")
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
plot_weekdays <- function(data) {
  #TODO change color palette. Encode season info (e.g. summer yellowish, winter blueish).
  data %>%
    filter_expenses %>%
    group_by(year = year(date), month = month(date), weekday = weekdays(date)) %>%
    summarise(total = sum(amount)) %>%
    ungroup %>%
    mutate(month = fct_reorder(month.abb[month], month),
           weekday = factor(weekday, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
    plot_time_series() +
    scale_x_discrete() +
    scale_fill_ordinal(begin = 1, end=0) +
    geom_col(aes(weekday, total, fill = month)) +
    facet_wrap(year~., nrow=1)
}
