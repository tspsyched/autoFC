#' Calculate the empirical reliability of each latent trait following the formula in Brown & Maydeu-Olivares (2018).
#'
#' @description TO BE DONE
#'
#' @usage TO BE DONE
#'
#' @param dataset  Data frame with trait estimates and standard errors
#'
#' @param score_names  Vector of characters. Which columns specify trait scores?
#' 
#' @param se_names  Vector of characters. Which columns specify trait standard errors?
#' 
#' @details TO BE DONE
#'
#' @returns A numeric vector containing empirical reliability estimates, ordered the same as in \code{score_names}.
#'
#' @author Mengtong Li
#'
#' @examples TO BE DONE
#'
#' @export
#'
empirical_reliability <- function(dataset, score_names, se_names) {
  results <- c()
  for (i in 1:length(score_names)) {
    score_name <- score_names[i]
    se_name <- se_names[i]
    emp_alpha <- var(dataset[score_name]) / (var(dataset[score_name]) + sum((dataset[se_name]^2))/nrow(dataset))
    results <- c(results, emp_alpha)
  }
  return(results)
}
