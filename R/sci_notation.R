#' @title Tidy scientific notation
#' @description Reformat numbers in scientific notation, translating the default 'e' notation used in base R to the 'x10' format more suitable for publication quality plots. If any number in a vector is expressed in R's default scientific notation, then all numbers in that vector are translated into expression objects with the 'x10' notation which can be added to plots. Thus, for consistency, any numbers without 'e' in that vector are treated similarly (e.g. \eqn{1} becomes \eqn{1 \times 10^0}). However, vectors which do not contain any number in R's default scientific notation are returned in as-is condition.
#'
#' @param x A numeric vector.
#'
#' @return A vector of expression objects that can be added to a plot.
#'
#' @examples
#'
#' #### Example (1):
#' x <- seq(1e-10, 1e10, by = 1e9)
#' y <- runif(length(x), 0, 100)
#' xtidy <- sci_notation(x)
#' plot(x, y, axes = FALSE)
#' axis(side = 1, at = x, labels = xtidy, pos = 0, las = 2)
#' axis(side = 2, at = seq(0, 100, by = 10), pos = 1e-10)
#'
#' @author Edward Lavender
#' @export

##############################################
##############################################
#### sci_notation

sci_notation <- function(x) {

  #### Require numeric input
  stopifnot(is.numeric(x))

  #### If any of the numbers inputted contains 'e' then we're
  # ... dealing with scientific notation
  if(any(stringi::stri_detect_fixed(as.character(x), "e"))){

    # Convert to characer
    x <- as.character(x); x

    # Remove + symbol and leading zeros on expoent, if > 1
    x <- sub("\\+0", "", x); x
    #Leaves - symbol but removes leading zeros on expoent, if < 1
    x <- sub("-0", "-", x); x
    # Replace e with 10^
    # x <- sub("e", "x10^", x); x

    # Deal with 0, if present, which loses the e
    # ... notation when converted to a character (we need to add this back)
    x0_pos <- which(x == "0")
    if(length(x0_pos) > 0){
      x[x0_pos] <- paste0(x[x0_pos], "e0")
    }

    # Identify the numbers before and after "e"
    x <- stringr::str_split_fixed(x, pattern = "e", 2)

    # Create a list of calls:
    lab <- list()
    for(i in 1:nrow(x)){
      lab[[i]] <- bquote(.(x[i, 1]) ~ "x" ~ 10^.(x[i, 2]))
    }

    # Convert to expressions which can be stored in a vector
    lab <- do.call(expression, lab)

    # Return lab
    return(lab)

  } else{
    return(x)
  }

} # close function


#### End of code.
##############################################
##############################################
