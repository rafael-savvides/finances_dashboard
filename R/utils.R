#' Wrapper for str_detect
#' Returns logical vector of elements of s that contain an element from patterns.
#' @param patterns char vector. patterns to be found in s
#' @param s char vector
#' @return logical vector length(s). TRUE if any element of patterns is detected in an element of s
str_detect_vec <- function(s, patterns) {
  patterns_collapsed <- paste(patterns, collapse = "|")
  i <- str_detect(s, patterns_collapsed)
  if (length(patterns) == 0) i <- rep(FALSE, times=length(s))
  i
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
summarise_month <- function(data) {
  data %>%
    group_by(month = month(date_paid)) %>%
    summarise(total = sum(amount))
}

#' Title
#'
#' @param data
#' @param Year
#'
#' @return
#' @export
#'
#' @examples
filter_year <- function(data, Year) {
  data %>% filter(year(date) == Year)
}

#' Add a category column to data
#'
#' @param data data frame
#' @param categories list of character vectors
#'
#' @return
add_category_column <- function(data, categories) {
  data %>%
    mutate(message_ = if_else(str_detect(message, "[a-zA-Z]"), message, ""),
           receiver_message = paste(receiver, message_),
           category = which_category(str_to_lower(receiver_message), categories)) %>%
    select(-message_, receiver_message)
}


#' Category of s
#'
#' @param s character vector
#' @param categories list of character vectors
#'
#' @return names(categories) that are contained in s
which_category <- function(s, categories) {
  res = character(length(s))
  for (i in seq_along(res)) {
    tmp = names(categories)[vapply(categories, function(vec) any(str_detect(string = s[i], vec)), logical(1))]
    if (length(tmp) < 1) tmp = "None"
    res[i] = tmp
  }
  res
}

#' Filter expenses
#'
#' @param data
#'
#' @return
#' @export
filter_expenses <- function(data) {
  data %>%
    filter(is_income == FALSE,
           is_internal_payment == FALSE) %>%
    mutate(amount = -amount)
}
