{
  library(dplyr)
  library(stringr)
  library(purrr)
  library(lubridate)
} |>
  suppressPackageStartupMessages() |>
  suppressWarnings()

DEFAULT_CATEGORY = "None"

#' Add a category column to data
#'
#' Searches for categories in data["receiver"] and, if it exists, data["message"].
#'
#' @param data data frame
#' @param categories list of character vectors
#'
#' @return
add_category_column <- function(data, categories) {
  if ("message" %in% colnames(data)) {
    message = if_else(str_detect(data[["message"]], "[a-zA-Z]"), data[["message"]], "")
  } else {
    message = ""
  }
  data["category"] = which_category(
    str_to_lower(paste(data[["receiver"]], message)), categories
  )
  data
}

#' Assign categories to strings
#'
#' If x[i] contains a term from `categories[j]`, then it belongs to category `names(categories)[j]`.
#'
#' @param x character vector
#' @param categories list of character vectors.
#'
#' @return category names for elements of x
which_category <- function(x, categories, default = DEFAULT_CATEGORY) {
  categ_x = character(length(x))
  for (i in seq_along(x)) {
    is_in_xi = vapply(categories, \(terms) any(str_detect(x[i], terms) & terms != ""), logical(1))
    categ_xi = names(categories)[is_in_xi]
    if (length(categ_xi) == 0) {
      categ_x[i] = default
    } else if (length(categ_xi) == 1) {
      categ_x[i] = categ_xi
    } else if (length(categ_xi) > 1) {
      # When there is more than one category, keep the first one.
      categ_x[i] = categ_xi[1]
    }
  }
  categ_x
}

#' Keep only external expenses
#'
#' @param data
#'
#' @return
#' @export
keep_only_expenses <- function(data) {
  data |>
    filter(
      amount < 0,
      tolower(payer) != tolower(receiver)
    ) |>
    mutate(amount = -amount)
}

#' Make fill color palette for categories and years
make_fill_palette = function(category_names, years) {
  clr_categ = RColorBrewer::brewer.pal(n = min(9, length(category_names)), "Set1")
  names(clr_categ) = category_names[1:length(clr_categ)]
  clr_categ[DEFAULT_CATEGORY] = "grey"

  clr_year = colorRampPalette(RColorBrewer::brewer.pal(n = 9, "Blues")[3:9])(length(years))
  names(clr_year) = as.character(years)

  c(clr_categ, clr_year)
}
