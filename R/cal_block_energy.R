#' Calculation of Item Block "Energy"
#'
#' @description Calculates the total "energy" of one or multiple paired
#' item blocks, which is a linear combination of different functions
#' applied to different item characteristics of interest.
#'
#' @usage cal_block_energy(block, item_chars, weights, FUN)
#'
#' @param block An \emph{n} by \emph{k} integer matrix,
#' where \emph{n} is the number of item blocks
#' and \emph{k} is the number of items per block.
#'
#' @param item_chars An \emph{m} by \emph{r} data frame,
#' where \emph{m} is the total number of items to sample from,
#' whether it is included in the block or not,
#' whereas \emph{r} is the number of item characteristics.
#'
#' @param weights A vector of length \emph{r} with weights for each
#' item characteristics in \code{item_chars}.
#' Should provide a weight of 0 for specific characteristics
#' not of interest, such as item ID.
#'
#' @param FUN A vector of customized function names for optimizing each
#' item characteristic within each block, with length \emph{r}.
#'
#' @details
#' This energy calculation function serves as the core for determining the
#' acceptance or rejection of a newly built block over the previous one.
#'
#' Higher energy is considered more preferable in this case.
#'
#' Items in the same block can be paired based on characteristics such as:
#'
#' Mean score, Item Factor, Factor loading, Item IRT Parameters,
#' Reverse Coding, etc.
#'
#' Pairings of different characteristics can be optimized in different way,
#' by determining the customized function vector \code{FUN}
#' and the corresponding \code{weights}.
#'
#'
#'
#' @returns A numeric value indicating the total energy
#' for the given item block(s).
#'
#' @author Mengtong Li
#'
#' @note Use \code{cal_block_energy_with_iia} if inter-item agreement
#' (IIA) metrics are needed.
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
#' ## See ?facfun for its use.
#' cal_block_energy(solution, item_chars = item_df,
#'               weights = c(1,1,1), FUN = c("facfun", "var", "var"))
#'
#' @export



cal_block_energy <- function(block, item_chars, weights, FUN) {
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
  }
  return(energy)
}
