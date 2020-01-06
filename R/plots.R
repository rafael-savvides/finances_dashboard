require(ggplot2)
theme_set(theme_light())

clr_categ = setNames(RColorBrewer::brewer.pal(n = min(9, length(categories)), "Set1"), names(categories))
clr_month = setNames(colorRampPalette(RColorBrewer::brewer.pal(n = 9, "Blues")[3:9])(12), month.abb)
clr_year = setNames(colorRampPalette(RColorBrewer::brewer.pal(n = 9, "Blues")[3:9])(10), 2017:2026)
clr_fill = c(clr_categ, clr_month, clr_year)

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
    scale_fill_manual(values = clr_fill) +
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
    plt + geom_col(aes(date, amount, fill=category), position="dodge")
  } else {
    plt + geom_col(aes(date, amount))
  }
  plt
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
plot_month_year <- function(data, show_categ=F) {
  #TODO replace with table_monthly
  plt = data %>%
    filter_expenses %>%
    group_by(year = year(date), month = month(date), category) %>%
    summarise(total = sum(amount)) %>%
    plot_time_series() +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(fill = "Year") +
    geom_col(aes(month, total, fill = as.factor(year)), position = "dodge")
  if (show_categ)
    plt + facet_wrap(category~.)
  plt
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
    #scale_fill_ordinal(begin = 1, end=0) +
    geom_col(aes(weekday, total, fill = month), position="dodge") +
    facet_wrap(year~., nrow=1)
}
