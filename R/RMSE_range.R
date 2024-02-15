#' Calculate the Overall RMSE of the Trait Scores, or the RMSE in a Certain Trait Score Range
#'
#' @description This function is also for diagnostic purposes, examining which interval on the
#' latent trait continuum does the FC scale demonstrate the best measurement accuracy.
#'
#'
#' @param true_scores  Actual trait scores
#' @param estimated_scores  Estimated trait scores from a specified model
#' @param range_breaks  A numeric vector. Specifies which trait scores ranges will the RMSE be calculated
#' 
#' 
#' @details TO BE DONE
#' @returns If \code{range_breaks} is not specified, an overall RMSE numeric value will be returned;
#' else, a named list showing the RMSE in each score range will be returned.
#' 
#' @import SimDesign
#' @author Mengtong Li
#'
#'
#' @examples
#' RMSE_range(rnorm(100), rnorm(100))
#' RMSE_range(rnorm(100), rnorm(100), range_breaks = c(-3, -2, -1, 0, 1, 2, 3))
#'
#' @export

RMSE_range <- function(true_scores, estimated_scores, range_breaks = NA) {
  if (length(range_breaks) > 1) {
    score_cut <- cut(true_scores, breaks = range_breaks, ordered_result = TRUE)
    RMSE_ranges <- setNames(rep(0, length(unique(score_cut))), sort(unique(score_cut)))
    for (sc in sort(unique(score_cut))) {
      indices <- which(score_cut == sc)
      RMSE_ranges[sc] <- RMSE(true_scores[indices], estimated_scores[indices])
    }
    return(RMSE_ranges)
  }
  else {
    return(RMSE(true_scores, estimated_scores))
  }
  
}