#' Table of expenses
#'
#' For a given time period (date_start to date_end) and for given categories
#'
#' @param data
#' @param date_start
#' @param date_end
#' @param categ
#'
#' @return
#' @export
#'
#' @examples
table_expenses <- function(data, date_start, date_end, categ) {
  data %>%
    keep_only_expenses() %>%
    select(date, amount, receiver, message, category) %>%
    filter(between(date, date_start, date_end)) %>%
    filter(category %in% categ)
}

#' Summary table
#'
#' For year and month show income, expense, difference, running total.
#' If group_categ=T then the above are subdivided also by category.
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
table_aggregate <- function(data, group_categ = FALSE) {
  dat = data %>%
    mutate(income = if_else(is_income, amount, 0),
           expenses = if_else(is_income, 0, -amount)) %>%
    group_by(year = year(date), month = month(date))
  if (group_categ)
    dat = dat %>% group_by(category, .add=TRUE)
  dat = dat %>%
    summarise(income = sum(income),
              expenses = sum(expenses),
              .groups="drop") %>%
    mutate(month = month.abb[month],
           year = as.integer(year),
           diff = income - expenses,
           running_total = cumsum(diff))
  if (group_categ) {
    dat %>%
      select(year, month, category, expenses)
  } else {
    dat
  }
}
