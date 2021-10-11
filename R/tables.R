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

#' Table of top n purchases
#'
#' Returns a data frame of top purchases in a given time frame and in given categories
#'
#' @param n_top integer
#' @param ... see [table_expenses]
#' @return data frame
#' @md
table_top_purchases <- function(..., n_top) {
  table_expenses(...) %>%
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

#' Search results
#'
#' @param search_term
#' @param ... see [table_expenses]
#'
#' @return
#' @export
#'
#' @md
table_search <- function(..., search_term) {
  parse_search_term <- function(s) {
    s = unlist(str_split(s, " ?\\| ?"))
    if (length(s) > 1)
      s = s[s != ""]
    s
  }
  search_term = parse_search_term(search_term)
  table_expenses(...) %>%
    filter(str_detect_vec(
      str_to_lower(receiver),
      str_to_lower(search_term)) | str_detect_vec(str_to_lower(message),
                                                  str_to_lower(search_term))) %>%
    mutate(date = format(date,'%Y-%m-%d'))
}
