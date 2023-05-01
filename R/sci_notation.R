#' @title Tidy scientific notation
#' @description This function is used to express numbers in scientific notation for plotting purposes. Specific elements in a vector \code{x}, or all elements in that vector, are converted into scientific notation if their absolute order of magnitude is greater than a user-specified value. Scientific notation is expressed using the 'x10' format, suitable for publication-quality plots, rather than R's default 'e' notation. The number of decimal places can be user-defined or defined automatically such that only the minimum number of decimal places required to distinguish numbers in a sequence is used (i.e., as suitable for pretty axes on a plot).
#'
#' @param x A numeric vector.
#' @param magnitude An integer that defines the order of magnitude (below or above 0) after which numbers in all or specific elements in x are converted to scientific notation (see \code{specific}).
#' @param digits An integer that defines the number of decimal places. If \code{NULL}, this is defined automatically to be the minimum number of decimal places required to distinguish numbers.
#' @param specific A logical input that defines whether or not to convert only the specific numbers in \code{x} whose order of magnitude exceed \code{magnitude} into scientific notation. Otherwise, if the absolute order of magnitude of element in \code{x} exceeds \code{magnitude}, all elements are reformatted in scientific notation. (However, 0 is always retained as 0.)
#' @param make_exp A logical input that defines whether or not to create an expression object or a character object.
#' @param make_sci A logical input that defines whether or not to create scientific notation. This acts as an overall control: if \code{make_sci} is \code{FALSE}, the function simplify returns \code{x} unchanged.
#'
#' @examples
#' #### Example (1): sci_notation() returns an expression object by default
#' sci_notation(seq(1e-10, 1e10, by = 1e9))
#' # Except for vectors in which all elements are below the specified magnitude,
#' # ... which are left unchanged:
#' sci_notation(1:10)
#'
#' #### Example (2): sci_notation() can be used to create pretty axis labels
#' x <- seq(1e-10, 1e10, by = 1e9)
#' y <- runif(length(x), 0, 100)
#' xtidy <- sci_notation(x)
#' plot(x, y, axes = FALSE)
#' axis(side = 1, at = x, labels = xtidy, pos = min(y), las = 2)
#' axis(side = 2, at = seq(0, 100, by = 10), pos = min(x))
#' # This is implemented automatically by pretty_axis() (e.g., via pretty_plot()):
#' pretty_plot(x, y)
#'
#' #### Example (3): The digits argument controls the number of decimal places:
#' # The default is to select the minimum number of decimal places to distinguish
#' # ... numbers:
#' sci_notation(c(1.29876e11, 1.29e11, 1.29e12))
#' sci_notation(c(1.29876e11, 1.298769e11, 1.29e12))
#' sci_notation(c(1.29876e11, 1.298769e12, 1.29e13))
#' # Otherwise, this can be directly specified:
#' sci_notation(c(1.29876e11, 1.29e11), digits = 8)
#'
#' #### Example (4) Magnitude and specific control implementation
#' sci_notation(c(0, 1, 2, 1e9), magnitude = 0)
#' sci_notation(c(0, 1, 2, 1e9), magnitude = 5, specific = FALSE)
#' sci_notation(c(0, 1, 2, 1e9), magnitude = 5, specific = TRUE)
#'
#' @return A vector of expression objects (or a vector of character objects) that can be added to a plot.
#'
#' @seealso  The function is implemented internally in \code{\link[prettyGraphics]{pretty_axis}} for numeric observations.
#' @author Edward Lavender
#' @export
#'


sci_notation <- function(x,
                         magnitude = 5L,
                         digits = NULL,
                         specific = TRUE,
                         make_exp = TRUE,
                         make_sci = TRUE){
  # Return x unchanged if make_sci is FALSE
  if(!make_sci) return(x)
  # Capture NAs
  is_na <- is.na(x)
  if (any(is_na)) {
    pos_na <- which(is_na)
    x <- x[!is.na(x)]
  }
  # Define power
  pwr  <- floor(log10(abs(x)))
  # If there are no elements in x greater than the specified magnitude, then return the numbers unchanged
  if(!any(abs(pwr)[!is.infinite(abs(pwr))] >= 5)) return(x)
  # Define coefficient to specified number of decimal places
  coef <- x/10^pwr
  # Determine minimum suitable number of digits to distinguish numbers
  if(is.null(digits)){
    # If all the whole strings or powers are unique (i.e., if there are numbers with both
    #... the same whole strings and the same powers) then digits = 0
    coef_char <- as.character(coef)
    whole_strings <- substr(coef_char, 1, 1)
    if(any(whole_strings == "-")){
      pos_neg <- which(whole_strings == "-")
      whole_strings[pos_neg] <- paste0(whole_strings[pos_neg], substr(coef_char[pos_neg], 2, 2))
    }
    if(!any(duplicated(paste0(whole_strings, "_", pwr)))){
      digits <- 0
    # Otherwise, determine the minimum number of digits to separate all numbers
    } else{
      # Focus on elements in x for which the whole strings (integer component) and powers are the same
      # And, for each of these elements,
      # ... determine the minimum number of digits required to distinguish those elements
      ws_pwr_pairs <- data.frame(ws = whole_strings,
                                 pwr = pwr,
                                 ws_pwr = paste0(whole_strings, "_", pwr)
                                 )
      digits_vec <- sapply(unique(ws_pwr_pairs$ws_pwr[duplicated(ws_pwr_pairs$ws_pwr)]), function(pair){
        # Isolate elements in coef_char that are relevant for consideration
        pos_pair <- which(ws_pwr_pairs$ws_pwr == pair)
        n <- length(pos_pair)
        coef_char_sbt <- coef_char[pos_pair]
        coef_char_sbt <- sprintf("%f", as.numeric(coef_char_sbt))
        # Isolate decimal strings
        decimal_strings <- substr(coef_char_sbt, 3, nchar(coef_char_sbt))
        # For decimal places from 1:16, determine whether the strings are unique
        decimal_strings_unique <- rep(NA, 16)
        for(stop in 1:16){
          decimal_strings_to_stop <- substr(decimal_strings, 1, stop)
          decimal_strings_unique[stop] <- (length(unique(decimal_strings_to_stop)) == n) + 0
        }
        # If any of the strings are unique, pick the minimum number of decimal places required:
        if(any(decimal_strings_unique == 1)){
          digits <- min(which(decimal_strings_unique > 0))
          # Otherwise, return the maximum number of digits before all zeros or the max number of digits = 16
        } else{
          digits <- min(c(nchar(decimal_strings_to_stop)), 16)
        }
        return(digits)
      })
      # We need the maximum of the minimum value for the number of digits
      digits <- max(digits_vec)
    }
  }
  # Define coefficients to the desired number of decimal places
  coef <- sprintf(paste0("%.", digits, "f"), coef)
  # Create a list of calls:
  lab <- list()
  for(i in 1:length(x)){
    if(make_exp) {
      lab[[i]] <- bquote(.(coef[i]) ~ "x" ~ 10^.(pwr[i]))
    } else {
      lab[[i]] <- paste0(coef[i], " x 10^", pwr[i])
    }
  }
  # Post-process any 0's
  if(any(x == 0)){
    pos0 <- which(x == 0)
    lab[pos0] <- sprintf(paste0("%.", digits, "f"), 0)
  }
  # Post-process any numbers less than magnitude, if specific = TRUE
  if(specific){
    if(any(abs(pwr) < magnitude)){
      pos1 <- which(abs(pwr) < magnitude)
      lab[pos1] <- sprintf(paste0("%.", digits, "f"), x[pos1])
    }
  }
  # Convert to vector
  if(make_exp) {
    lab <- do.call(expression, lab)
  } else {
    lab <- unlist(lab)
  }
  # Append NAs in original positions
  if (any(is_na)) {
    for (p in pos_na) {
      lab <- append(lab, NA, after = p - 1)
    }
  }
  # Return labels
  return(lab)
}


#### End of code.
##############################################
##############################################
