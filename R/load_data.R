library(dplyr)
library(stringr)
library(purrr)
library(lubridate)
source("utils.R")

#' Read all csv files in a directory
#'
#' @param dir_bank
#'
#' @return
#' @export
#'
#' @examples
read_all <- function(dir_bank) {
  stopifnot(dir.exists(dir_bank))
  read_data <- function(filename) {
    if (grepl("spankki", filename)) {
      read_spankki(filename)
    } else if (grepl("danske", filename)) {
      read_danske(filename)
    }
  }
  file_list = list.files(dir_bank, full.names = TRUE, pattern = "*.csv$")
  map_df(file_list, read_data) %>%
    arrange(date) %>%
    mutate(is_income = amount > 0,
           is_internal_payment = str_to_lower(payer) %in% c("rafael savvides", "savvides rafael") & str_to_lower(receiver) %in% c("rafael savvides", "savvides rafael"))
}

#' Read and preprocess S-pankki csv to data frame
#'
#' @param filename csv file
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
    mutate(receiver = str_replace(receiver, " +\\)\\)\\)\\)", ""),
           amount = as.numeric(amount))
}


#' Read categories
#'
#' @param filename
#'
#' @return list of character vectors
read_categories <- function(filename) {
  readRDS(filename)
}

