#' Interactively update categories.rds
#'
#'
#' @return
#' @export
#'
#' @examples
update_categories <- function(data, categories, categories_new = NULL) {
  if (!is.null(categories_new)) {
    categories = append_new_labels(categories, categories_new)
    return(invisible(categories))
  }

  labels_in_data = data$receiver %>% str_to_lower() %>% unique()

  while(read_yes_no_input("Continue? ")) {
    labels_new = get_new_labels(labels_in_data, categories)
    print("New labels in data not present in categories: ")
    print(labels_new)

    categories_new = eval(parse(text = readline("Give list of new categories. ")))
    print("Adding the following to categories: ")
    print(categories_new)
    categories = append_new_labels(categories, categories_new)
  }

  if (read_yes_no_input("Save? ")) {
    filename = readline("Enter filename. ")
    while (file.exists(filename) && !read_yes_no_input(paste0("File ", filename, " exists. Override? (T or F) ")))
      filename = readline("Enter filename. ")
    print(paste0("Saving to ", filename))
    saveRDS(categories, filename)
  }
  invisible(categories)
}

#' Wrapper for readline
#' @param prompt string. see [readline]
#' @return TRUE or FALSE
#' @export
#' @md
read_yes_no_input = function(prompt) {
  options = list(yes = c("T", "t", "TRUE", "Yes", "Y", "y"),
                 no = c("F", "f", "FALSE", "No", "N", "n"))
  input = readline(prompt)
  while (!input %in% unlist(options)) {
    print(paste0("Allowed answers: ", paste(unlist(options), collapse=", ")))
    input = readline(prompt)
  }
  input %in% options$yes
}


#' Get new labels
#' Get the labels_in_data that are not in categories.
#' @param labels_in_data character vector
#' @param categories list of vectors
#' @return character vector
get_new_labels = function(labels_in_data, categories) {
  labels_in_data[!str_detect_vec(s = labels_in_data, patterns = unlist(categories))]
}

#' Append new labels
#' For each element of categories, append elements of categories_new that are not in categories.
#' @param categories list of vectors
#' @param categories_new list of vectors
#' @return list of vectors
append_new_labels <- function(categories, categories_new) {
  result = categories
  cat_names = intersect(names(result), names(categories_new))
  if (length(cat_names) < 1) stop("categories_new has no common names with categories.")
  for (cat_name in cat_names) {
    result[[cat_name]] = unique(append(categories[[cat_name]], categories_new[[cat_name]]))
  }
  result
}
