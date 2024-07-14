#' Calculate the Empirical Reliability of the Latent Trait Scores,
#' Following the Formula in Brown & Maydeu-Olivares (2018).
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
#' @details For trait scores estimated using item response theory models, a suitable reliability estimate
#' is empirical reliability, which provides a summary estimate on how reliable the trait scores are "as a whole". 
#'
#' @returns A numeric vector containing empirical reliability estimates, ordered the same as in \code{score_names}.
#'
#' @author Mengtong Li
#'
#' @examples 
#' \dontrun{empirical_reliability(dataset, c("Trait1", "Trait2", "Trait3"), c("se1", "se2", "se3"))}
#' 
#' @references 
#' Brown, A., & Maydeu-Olivares, A. (2018). Ordinal factor analysis of graded-preference questionnaire data. 
#' \emph{Structural Equation Modeling: A Multidisciplinary Journal, 25}(4), 516-529. https://doi.org/10.1080/10705511.2017.1392247
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
