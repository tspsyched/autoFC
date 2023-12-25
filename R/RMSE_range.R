#' Calculate the overall RMSE of the trait scores, or the RMSE in a certain trait score range
#'
#' @description TO BE DONE
#'
#' @usage TO BE DONE
#'
#' @param true_scores  Actual trait scores
#' @param estimated_scores  Estimated trait scores from a specified model
#' @param range_breaks  A numeric vector. Specifies which trait scores ranges will the RMSE be calculated
#' 
#' 
#' @details TO BE DONE
#' @returns TO BE DONE
#' 
#' @import SimDesign
#' @author Mengtong Li
#'
#'
#' @examples TO BE DONE
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