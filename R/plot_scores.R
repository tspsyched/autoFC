#' Scatter plot for true vs estimated scores, true score vs absolute error, etc.
#'
#' @description  TO BE DONE
#'
#' @usage TO BE DONE
#'
#' @param x_scores  Scores to be plotted on the x axis
#' @param y_scores  Scores to be plotted on the y axis
#' @param type  Which type of plots is plotted? Can be \code{"simple"} for simple x-y plot, or \code{"abs.diff"}
#' for plotting absolute difference of (y-x) vs x.
#' 
#'
#' @details This is only a very crude plot function extending \code{plot()} for demonstrative purposes.
#' Users are free to develop their own versions of plotting.
#'
#' @returns A scatter plot
#'
#' @author Mengtong Li
#'
#' 
#' @examples TO BE DONE
#'
#' @export
plot_scores <- function(x_scores, y_scores, type = "simple", ...) {
  if (!type %in% c("simple", "abs.diff")) {
    return("type must be in \"simple\" or \"abs.diff\"")
  }
  
  if (type == "simple") {
    plot(x_scores, y_scores, ...)
  }
  else if (type == "abs.diff") {
    plot(x_scores, abs(y_scores - x_scores), ...)       
  }
  
}