#' @title Tidy number columns in a dataframe
#' @description This function `tidies' a dataframe's number columns by representing numbers as characters and equalising the number of digits. This is designed to support the inclusion of tables created in R in word processors.
#' @param .data A \code{\link[base]{data.frame}} or a \code{\link[dplyr]{tibble}}.
#' @param digits An integer or a vector of integers that define(s) the number of digits for numerical columns. If one integer is supplied, this is applied to all number columns; if a vector is supplied, vector elements are applied to number columns in the order given.
#' @param ignore A logical value that defines whether or not to ignore the input to \code{digits} for any number columns that only comprise integers (see \code{\link[prettyGraphics]{add_lagging_point_zero}}).
#' @return The function returns a \code{\link[base]{data.frame}} or a \code{\link[dplyr]{tibble}}.
#'
#' @details This function does not currently support scientific notation.
#'
#' @examples
#' # Define an example dataframe
#' d <- data.frame(x = 1:3, y = c(0.1, 0, 1), z = c("a", "b", NA))
#' # Use three decimal places for all number columns
#' tidy_numbers(d, 3, ignore  = FALSE)
#' # Use three decimal places for all number columns with decimal places
#' tidy_numbers(d, 3)
#' # Specify the number of decimal places for each number  column
#' tidy_numbers(d, c(2, 3))
#' @author Edward Lavender
#' @export

tidy_numbers <- function(.data, digits, ignore = TRUE){
  is_number <- function(x) {
    x <- unlist(x)
    is.numeric(x) || is.integer(x)
  }
  test_is_number <- rep(NA, ncol(.data))
  for(i in 1:ncol(.data)) test_is_number[i] <- is_number(.data[, i])
  if(any(test_is_number)){
    is_tibble <- inherits(.data, "tbl_df")
    .data <- data.frame(.data)
    ind_is_number <- which(test_is_number)
    len_is_number <- length(ind_is_number)
    if(length(digits) == 1L & len_is_number > 1L) digits <- rep(digits, len_is_number)
    if(length(digits) != len_is_number)
      stop("One value for 'digits', or one value for each numerical column, should be specified.", call. = FALSE)
    for(i in seq_len(len_is_number)){
      if(digits[i] > 0L){
        col      <- ind_is_number[i]
        accuracy <- as.numeric(paste0("0.", paste0(rep(0, digits[i]-1), collapse = ""), "1"))
        rows <- which(!is.na(.data[, col]))
        if(length(rows) > 0L){
          .data[rows, col] <- round_any(.data[rows, col], accuracy)
          .data[rows, col] <- add_lagging_point_zero(.data[rows, col], digits[i], ignore = ignore)
        }
      }
    }
    if(is_tibble) .data <- dplyr::as_tibble(.data)
  } else {
    message("No numerical columns in '.data'.")
  }
  return(.data)
}

#' @title Write `tidy' tables to file
#' @description This is a wrapper function for \code{\link[utils]{write.table}} with `tidier' defaults. This is designed to write tables to file that can be easily copied into word processors, such as Microsoft Word.
#' @param x,file,quote,sep,row.names,na,... Arguments passed to \code{\link[utils]{write.table}}.
#' @return The function writes a table to file.
#' @export

tidy_write <- function(x, file,
                       quote = FALSE,
                       sep = ",",
                       row.names = FALSE,
                       na = "-",...){
  utils::write.table(x,
                     file = file,
                     quote = quote,
                     sep = sep,
                     row.names = row.names,
                     na = na,...)
  return(invisible())
}
