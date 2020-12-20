#' @title Add a regression line and an error envelope to a plot
#' @description This function adds regression lines and associated error envelopes (e.g., confidence intervals) to plots. This function is designed to streamline plotting codes. Confidence intervals can be added as a polygon or as lines (with or without the fitted line added ontop). For large datasets, adding confidence intervals with lines is much faster.
#'
#' @param x A numeric vector defining the x values at which fitted values/CIs will be plotted.
#' @param CI A named list with fitted values (optional) and lower and upper CIs (i.e. \code{CI$fit}, \code{CI$lowerCI}, \code{CI$upperCI}). This can be created by \code{\link[prettyGraphics]{list_CIs}}.
#' @param fCI A character defining the method by which CIs will be added to a plot: as lines (\code{"lines"}) or as a shaded polygon (\code{"poly"}). For large datasets, \code{fCI = "lines"} is much faster.
#' @param CI_gp A named list of graphical parameters for CIs. If \code{fCI = "lines"}, then two nested lists can be included to specify the lower and upper CI lines differently, if desired (see Examples).
#' @param add_fitted A logical input defining whether or not add the fitted line.
#' @param fitted_gp A named list of graphical parameters for the fitted line (passed to \code{\link[graphics]{lines}}).
#'
#' @return The function adds model predictions to a plot.
#' @details The function is designed for continuous explanatory variables. \code{\link[prettyGraphics]{add_error_bars}} is used for discrete explanatory variables to add fitted values and associated errors to plots.
#'
#' @examples
#' # Define some data for a model
#' set.seed(1)
#' x <- runif(100, 0, 100)
#' y <- rnorm(100, 0.5*x - 50, 100)
#' # Define model
#' m1 <- lm(y ~ x)
#' # Define predictions
#' xp <-  seq(0, 100, by = 10)
#' p <- predict(m1, newdata = data.frame(x = xp), se.fit = TRUE)
#' # List CIs
#' CIs <- list_CIs(pred = p, plot_suggestions = FALSE)
#' # Visualise
#' plot(x, y)
#'
#' #### Example (1):
#' # Add predicted CIs as a polygon and add fitted line ontop using default graphical parameters
#' # Note that fCI = "poly", and add_fitted = TRUE do not need to be supplied
#' # ... since these are the default options
#' add_error_envelope(x = xp, CI = CIs)
#'
#' #### Example (2):
#' # Add predicted CIs as a polygon and add fitted lines with user-specified parameters
#' plot(x, y)
#' add_error_envelope(x = xp,
#'                       CI = CIs,
#'                       CI_gp = list(col = scales::alpha("skyblue", 0.8), border = FALSE),
#'                       fitted_gp = list(col = "blue", lwd = 3, lty = 1)
#'                       )
#'
#' #### Example (3):
#' # Add predicted CIs as lines, where both upper and lower CIs have identical graphical parameters
#' plot(x, y)
#' add_error_envelope(x = xp,
#'                       CI = CIs,
#'                       fCI = "lines",
#'                       CI_gp = list(col = "red")
#'                       )
#'
#' #### Example (4):
#' # Control lower and upper CI lines independently in a nested list
#' # The first element is the lower CI; the second element is the upper CI
#' plot(x, y)
#' add_error_envelope(x = xp,
#'                       CI = CIs,
#'                       fCI = "lines",
#'                       CI_gp = list(list(col = "red"), list(col = "blue"))
#'                       )
#'
#' @author Edward Lavender
#' @export
#'

add_error_envelope <-
  function(
    x,
    CI = list(),
    fCI = "poly",
    CI_gp = list(col = scales::alpha("lightgrey", 0.8), border = FALSE),
    add_fitted = TRUE,
    fitted_gp = list(col = "black", lwd = 1, lty = 1)
  ){

    #### Check fCI is supported
    stopifnot(fCI %in% c("lines", "poly"))

    #### Add CIs as lines if requested
    if(fCI == "lines"){
      # Define a list of parameters for both the upper and lower lines, if not specified separately
      if(plotrix::listDepth(CI_gp) == 1){
        CI_gp <- list(CI_gp, CI_gp)
      }
      # Add lines by looping over each list element
      lines_ls <- mapply(gp = CI_gp, ci = list(CI$lowerCI, CI$upperCI), function(gp, ci){
        # Add x and y values
        CI_gp_line <- rlist::list.merge(gp, list(x = x, y = ci))
        do.call(graphics::lines, CI_gp_line)
      })

      #### Add Cis as poly if requested:
    } else if(fCI == "poly"){

      #### Processing to avoid NAs:
      # NAs can be included in lines() but cause issues for polygon()...
      # ... so we'll define a dataframe with x values and CI values;
      # ... we'll split the dataframe at NAs and remove NAs
      # ... we'll then loop over each element and create the polygon/lines
      posNA <- which(is.na(CI$lowerCI))
      d <- data.frame(x = x, lowerCI = CI$lowerCI, upperCI = CI$upperCI)
      if(length(posNA) > 0){
        dls <- split(d, findInterval(1:nrow(d), posNA + 1))
        dls <- lapply(dls, function(df) return(df[stats::complete.cases(df), ]))
        dls <- plyr::compact(dls)
      } else{
        dls <- list(d)
      }

      #### Define the CI envelope based on inputted values
      # ... and supplied graphical parameters:
      lapply(dls, function(df){
        CI_gp <- rlist::list.merge(CI_gp, list(x = c(df$x, rev(df$x)), y = c(df$upperCI, rev(df$lowerCI))))
        do.call(graphics::polygon, CI_gp)
      })

    } # close else if(fCI == "poly"){

    #### Add back the fitted line, if requested
    if(add_fitted){
      # Check CI$fit assumed to be supplied for function:
      if(is.null(CI$fit)){ stop("To add the fitted line back over the polygon, you need to supply CI$fit") }
      # Define a default list of grpahical parameters for the line:
      dfgp <- list(x = x, y = CI$fit, col = "black", lwd = 1, lty = 1)
      # Merge the default parameters list with the fitted parameters supplied:
      fgp <- rlist::list.merge(dfgp, fitted_gp); fgp
      # Pass the list of parameters to the lines function to be plotted
      do.call(graphics::lines, fgp)
    } # close if(add_fitted){

  } # close function


#############################################
#############################################
#### add_model_predictions() (now depreciated)

#' @title (depreciated) Add model predictions (including fitted values and confidence intervals) to a plot
#' @description This function has been renamed. Please use \code{\link[prettyGraphics]{add_error_envelope}} instead.
#'
#' @param x A numeric vector defining the x values at which fitted values/CIs will be plotted.
#' @param CI A named list with fitted values (optional) and lower and upper CIs (i.e. \code{CI$fit}, \code{CI$lowerCI}, \code{CI$upperCI}). This can be created by \code{\link[prettyGraphics]{list_CIs}}.
#' @param fCI A character defining the method by which CIs will be added to a plot: as lines (\code{"lines"}) or as a shaded polygon (\code{"poly"}). For large datasets, \code{fCI = "lines"} is much faster.
#' @param CI_gp A named list of graphical parameters for CIs. If \code{fCI = "lines"}, then two nested lists can be included to specify the lower and upper CI lines differently, if desired (see Examples).
#' @param add_fitted A logical input defining whether or not add the fitted line.
#' @param fitted_gp A named list of graphical parameters for the fitted line (passed to \code{\link[graphics]{lines}}).
#'
#' @return The function adds model predictions to a plot.
#' @details The function is designed for continuous explanatory variables (i.e., adding regression lines and CIs to a plot). See \code{\link[prettyGraphics]{add_error_bars}} for discrete explanatory variables.
#'
#' @author Edward Lavender
#' @export
#'

add_model_predictions <-
  function(
    x,
    CI = list(),
    fCI = "poly",
    CI_gp = list(col = scales::alpha("lightgrey", 0.8), border = FALSE),
    add_fitted = TRUE,
    fitted_gp = list(col = "black", lwd = 1, lty = 1)
    ){

    #### Depreciated
    .Deprecated("add_error_envelope")

    #### Check fCI is supported
    stopifnot(fCI %in% c("lines", "poly"))

    #### Add CIs as lines if requested
    if(fCI == "lines"){
      # Define a list of parameters for both the upper and lower lines, if not specified separately
      if(plotrix::listDepth(CI_gp) == 1){
        CI_gp <- list(CI_gp, CI_gp)
      }
      # Add lines by looping over each list element
      lines_ls <- mapply(gp = CI_gp, ci = list(CI$lowerCI, CI$upperCI), function(gp, ci){
        # Add x and y values
        CI_gp_line <- rlist::list.merge(gp, list(x = x, y = ci))
        do.call(graphics::lines, CI_gp_line)
      })

    #### Add Cis as poly if requested:
    } else if(fCI == "poly"){

      #### Processing to avoid NAs:
      # NAs can be included in lines() but cause issues for polygon()...
      # ... so we'll define a dataframe with x values and CI values;
      # ... we'll split the dataframe at NAs and remove NAs
      # ... we'll then loop over each element and create the polygon/lines
      posNA <- which(is.na(CI$lowerCI))
      d <- data.frame(x = x, lowerCI = CI$lowerCI, upperCI = CI$upperCI)
      if(length(posNA) > 0){
        dls <- split(d, findInterval(1:nrow(d), posNA + 1))
        dls <- lapply(dls, function(df) return(df[stats::complete.cases(df), ]))
        dls <- plyr::compact(dls)
      } else{
        dls <- list(d)
      }

      #### Define the CI envelope based on inputted values
      # ... and supplied graphical parameters:
      lapply(dls, function(df){
        CI_gp <- rlist::list.merge(CI_gp, list(x = c(df$x, rev(df$x)), y = c(df$upperCI, rev(df$lowerCI))))
        do.call(graphics::polygon, CI_gp)
      })

    } # close else if(fCI == "poly"){

    #### Add back the fitted line, if requested
    if(add_fitted){
      # Check CI$fit assumed to be supplied for function:
      if(is.null(CI$fit)){ stop("To add the fitted line back over the polygon, you need to supply CI$fit") }
      # Define a default list of grpahical parameters for the line:
      dfgp <- list(x = x, y = CI$fit, col = "black", lwd = 1, lty = 1)
      # Merge the default parameters list with the fitted parameters supplied:
      fgp <- rlist::list.merge(dfgp, fitted_gp); fgp
      # Pass the list of parameters to the lines function to be plotted
      do.call(graphics::lines, fgp)
    } # close if(add_fitted){

  } # close function


#### End of code.
##############################################
##############################################
