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
