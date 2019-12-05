library(tidyverse)
library(lubridate)

#' Read all spankki csv files
#'
#' @param dir_file
#'
#' @return
#' @export
#'
#' @examples
read_spankki_all <- function(dir_file = readLines("data/dir_bank.txt")) {
  file_list = list.files(dir_file, pattern = "spankki.+\\.csv")
  map_df(file_list, read_data)
}

#' Title
#'
#' @param dir_file
#'
#' @return
#' @export
#'
#' @examples
read_all <- function(dir_file = readLines("data/dir_bank.txt")) {
  file_list = list.files(dir_file)
  map_df(file_list, read_data) %>%
    arrange(date) %>%
    mutate(is_income = amount > 0,
           is_internal_payment = str_to_lower(payer) %in% c("rafael savvides", "savvides rafael") & str_to_lower(receiver) %in% c("rafael savvides", "savvides rafael"))
}

#' Read csv into a data frame
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
read_data <- function(filename, dir_file = readLines("data/dir_bank.txt")) {
  #filename = "spankki_20180101_20181231.csv"
  filename = paste0(dir_file, "\\", filename)
  if (grepl("spankki", filename)) {
    data = read_spankki(filename)
  } else if (grepl("danske", filename)) {
    data = read_danske(filename)
  }
  data
}


#' Read and preprocess S-pankki csv to data frame
#'
#' @param filename location of csv file
#'
#' @return data frame
#' @export
read_spankki <- function(filename) {
  make_numeric <- function(s) {
    x <- gsub(",", ".", s) # Commas to periods
    x <- gsub(" ", "", x) # Remove spaces (thousands delimiter). Not present in csv anymore.
    as.numeric(x)
  }
  day_month_year2date <- function(x) as.Date(as.character(x), tryFormats = c("%d.%m.%Y"))
  book <- read.csv(file = filename,
                   sep=";",
                   encoding = "UTF-8",
                   col.names = c("date_reported", "date_paid", "amount", "type", "payer", "receiver", "receiver_iban", "receiver_bic",
                                 "viite_number", "message", "archive_number"),
                   colClasses = c("character", "character", "character", "character", "character", "character", "character", "character",
                                  "character", "character", "character"))

  book %>%
    mutate(amount = make_numeric(amount),
           date_reported = day_month_year2date(date_reported),
           date = day_month_year2date(date_paid)) %>%
    dplyr::select(date, amount, type, payer, receiver, message)
}

#' Read Danske bank data
#'
#' @param filename
#'
#' @return
#' @export
#'
#' @examples
read_danske <- function(filename) {
  day_month_year2date <- function(x) as.Date(as.character(x), tryFormats = c("%d.%m.%Y"))
  read.csv2(filename,
            col.names = c("date", "receiver_payer", "amount", "balance", "status", "check"),
            stringsAsFactors = FALSE) %>%
  mutate(payer = if_else(amount > 0, as.character(receiver_payer), "Rafael Savvides"),
         receiver = if_else(amount < 0, as.character(receiver_payer), "Rafael Savvides"),
         date = day_month_year2date(date)) %>%
    select(date, receiver, payer, amount) %>%
    mutate(receiver = str_replace(receiver, " +\\)\\)\\)\\)", ""))
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
add_category_column <- function(data, categories = read_categories()) {
  data %>%
    mutate(message_ = if_else(str_detect(message, "[a-zA-Z]"), message, ""),
           receiver_message = paste(receiver, message_),
           category = which_category(str_to_lower(receiver_message), categories)) %>%
    select(-message_, receiver_message)
}

#' Read categories.rds
#'
#' @param filename
#'
#' @return list of character vectors
read_categories <- function(filename = "data/categories.rds") {
  readRDS(file = filename)
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
