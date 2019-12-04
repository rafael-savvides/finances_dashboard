#' Table of top n purchases
#'
#' Returns a data frame of top purchases in a given time frame and in given categories
#'
#' @param data
#' @param date_start date
#' @param date_end date
#' @param categ character vector
#' @param n_top integer
#'
#' @return data frame
table_top_purchases <- function(data, date_start, date_end, categ, n_top) {
  data %>%
    filter_expenses %>%
    select(date, amount, receiver, message, category) %>%
    filter(between(date, date_start, date_end)) %>%
    filter(category %in% categ) %>%
    top_n(n = n_top, amount) %>%
    arrange(-amount) %>%
    mutate(date = format(date,'%Y-%m-%d'))
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
table_aggregate <- function(data, group_categ = F) {
  dat = data %>%
    mutate(income = if_else(is_income, amount, 0),
           expenses = if_else(is_income, 0, -amount)) %>%
    group_by(year = year(date), month = month(date))
  if (group_categ)
    dat = dat %>% group_by(category, add=T)
  dat = dat %>%
    summarise(income = sum(income),
              expenses = sum(expenses)) %>%
    ungroup %>%
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
