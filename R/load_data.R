library(jsonlite)


#' Read dataframe
#'
#' @param filename path to csv or rds file
read_data = function(filename, required_columns = c("date", "amount", "payer", "receiver")) {
  if (endsWith(filename, ".csv")) {
    data = read.csv(filename)
  } else if (endsWith(filename, ".rds")) {
    data = readRDS(filename)
  } else {
    stop(sprintf("filename should be a .csv or .rds, but was %s", filename))
  }
  stopifnot(required_columns %in% colnames(data))
  data["date"] = as.Date(as.character(data[["date"]]), tryFormats = c("%Y-%m-%d"))
  if (!"message" %in% colnames(data)) {
    data["message"] = ""
  }
  data
}

#' Read categories
#'
#' @param filename json file
#'
#' @return list of character vectors
read_categories <- function(filename) {
  x = read_json(filename)
  x = lapply(x, unlist)
  x[sapply(x, is.null)] = ""
  x
}
