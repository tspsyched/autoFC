#' Calculation of Item Block "Energy" with IIAs Included
#'
#' @description Calculates the total "energy" of one or multiple paired
#' item blocks, which is a linear combination of different functions
#' applied to different item characteristics of interest.
#'
#' This function extends \code{cal_block_energy} function
#' with consideration of inter item agreement (IIA) metrics.
#'
#' @usage cal_block_energy_with_iia(block, item_chars, weights,
#'                                  FUN, rater_chars,
#'                                  iia_weights = c(BPlin = 1, BPquad = 1,
#'                                  AClin = 1, ACquad = 1), verbose = FALSE)
#'
#' @param block,item_chars,weights,FUN See \code{?cal_block_energy} for details.
#'
#' @param rater_chars A \emph{p} by \emph{m} numeric matrix with scores of each of the \emph{p}
#' participants for the \emph{m} items.
#'
#' @param iia_weights A vector of length 4 indicating weights given to each IIA metric:
#'
#' Linearly weighted AC (Gwet, 2008; 2014);
#'
#' Quadratic weighted AC;
#'
#' Linearly weighted Brennan-Prediger (BP) Index(Brennan & Prediger, 1981; Gwet, 2014);
#'
#' Quadratic weighted BP.
#'
#' @param verbose Logical. Should IIAs be printed when this function is called?
#'
#' @details
#' This energy calculation function serves as the core for determining
#' the acceptance or rejection of a newly built block over the previous one.
#' Higher energy is considered more preferable in this case.
#'
#' Items in the same block can be paired based on characteristics such as:
#' Mean score, Item Factor, Factor loading, Item IRT Parameters,
#' Reverse Coding, etc.
#'
#' In addition, IIAs can be adopted to further estimate
#' rater agreements between different items,
#' if such information is available for the researchers.
#'
#' Pairings of different characteristics can be optimized in different way,
#' by determining the customized function vector \code{FUN}
#' and the corresponding \code{weights}.
#' Currently only linear weighted combination
#' for IIAs can be used in optimization.
#'
#'
#'
#' @returns A numeric value indicating the total energy
#' for the given item block(s).
#'
#' @author Mengtong Li
#'
#' @references
#' Brennan, R. L., & Prediger, D. J. (1981). Coefficient kappa: Some uses, misuses,
#' and alternatives. \emph{Educational and Psychological Measurement, 41}(3),
#' 687-699. https://doi.org/10.1177/001316448104100307
#'
#' Gwet, K. L. (2008). Computing inter rater reliability and its
#' variance in the presence of high agreement.
#' \emph{British Journal of Mathematical and Statistical Psychology, 61}(1),
#' 29-48. https://doi.org/10.1348/000711006X126600
#'
#' Gwet, K. L. (2014). \emph{Handbook of inter-rater reliability (4th ed.):
#' The definitive guide to measuring the extent of agreement among raters}.
#' Gaithersburg, MD: Advanced Analytics Press.
#' 
#' @note Use \code{cal_block_energy_with_iia} if inter-item agreement
#' (IIA) metrics are needed.
#'
#' @seealso \code{cal_block_energy}
#'
#' @examples
#' ## Simulate 60 items loading on different Big Five dimensions,
#' ## with different mean and item difficulty
#'
#' item_dims <- sample(c("Openness","Conscientiousness","Neuroticism",
#'                      "Extraversion","Agreeableness"), 60, replace = TRUE)
#' item_mean <- rnorm(60, 5, 2)
#' item_difficulty <- runif(60, -1, 1)
#'
#' ## Construct data frame for item characteristics and produce
#' ## 20 random triplet blocks with these 60 items
#'
#' item_df <- data.frame(Dimensions = item_dims, Mean = item_mean,
#'                      Difficulty = item_difficulty)
#' solution <- make_random_block(60, 60, 3)
#'
#'
#' ## Simple simulation of responses from 600 participants on the 60 items.
#' ## In practice, should use real world data or simluation based on IRT parameters.
#'
#' item_responses <- matrix(sample(seq(1:5), 600*60, replace = TRUE), ncol = 60, byrow = TRUE)
#'
#'
#' cal_block_energy_with_iia(solution, item_chars = item_df, weights = c(1,1,1),
#'                           FUN = c("facfun", "var", "var"),
#'                           rater_chars = item_responses, iia_weights = c(1,1,1,1))
#'
#' @importFrom irrCAC bp.coeff.raw gwet.ac1.raw
#'
#' @export


cal_block_energy_with_iia <- function(block, item_chars, weights, FUN, rater_chars,
                                      iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1),
                                      verbose = FALSE) {
  if (missing(block)) {
    block <- make_random_block(nrow(item_chars), item_per_block = 2)
  }
  if (missing(FUN)) {
    types <- sapply(item_chars, class)
    types <- replace(types, types == "factor" | types == "character", "facfun")
    types <- replace(types, types == "numeric", "var")
    FUN <- types
  }
  if (missing(weights)) {
    weights <- rep(1, ncol(item_chars))
  }

  indices <- seq(1:ncol(item_chars))
  energy <- 0

  # Apply separate functions for each item characteristic to get an estimate of energy, then sum this up.
  for (row in seq(1:nrow(block))) {
    for (i in indices) {
      fun_i <- FUN[i]
      energy <- energy + weights[i] * eval(call(fun_i, item_chars[block[row,], i]))
    }
    # Then we add up IIA metrics.
    selected_item <- rater_chars[,block[row,]]
    BPlin <- bp.coeff.raw(selected_item, weights = "linear")$est[,4]
    BPquad <- bp.coeff.raw(selected_item, weights = "quadratic")$est[,4]
    AClin <- gwet.ac1.raw(selected_item, weights = "linear")$est[,4]
    ACquad <- gwet.ac1.raw(selected_item, weights = "quadratic")$est[,4]
    iia_energy <- iia_weights %*% c(BPlin, BPquad, AClin, ACquad)
    if (verbose == TRUE) {
      print(sprintf("BPlin: %f, BPquad: %f, AClin: %f, ACquad: %f", BPlin, BPquad, AClin, ACquad))
    }
    energy <- energy + iia_energy

  }
  return(energy)
}
