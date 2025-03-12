#' Automatic Item Pairing Method in Forced-Choice Test Construction
#'
#' @description Automatic construction of forced-choice tests based on
#' Simulated Annealing algorithm. Allows items to be:
#'
#' 1. Matched in either pairs, triplets, quadruplets or blocks of any size;
#'
#' 2. Matched based on any number of item-level characteristics
#' (e.g. Social desirability, factor) based on any customized criteria;
#'
#' 3. Matched based on person-level inter-item agreement (IIA) metrics.
#'
#' @usage sa_pairing_generalized(block, total_items, Temperature,
#'                               eta_Temperature = 0.01, r = 0.999,
#'                               end_criteria = 10^(-6),
#'                               item_chars, weights, FUN,
#'                               n_exchange = 2, prob_newitem = 0.25,
#'                               use_IIA = FALSE, rater_chars,
#'                               iia_weights = c(BPlin = 1, BPquad = 1,
#'                               AClin = 1, ACquad = 1))
#'
#' @param block An \emph{n} by \emph{k} integer matrix,
#' where \emph{n} is the number of item blocks
#' and \emph{k} is the number of items per block.
#'
#' Serves as the initial starting blocks for the automatic pairing method.
#'
#' @param total_items Integer value. How many items do we sample from
#' in order to build this \code{block}? Should be more than number of unique values
#' in \code{block}.
#'
#' @param Temperature Initial temperature value. Can be left blank and be computed based on
#' the absolute value of initial energy of \code{block} (Recommended), and scaled
#' by \code{eta_Temperature}.
#'
#' In general, higher temperature represents a higher probability of
#' accepting an inferior solution.
#'
#' @param eta_Temperature A positive numeric value. The ratio of initial temperature to
#' initial energy of \code{block}, if \code{Temperature} is not designated.
#'
#' @param r A positive numeric value less than 1.
#' Determines the reduction rate of \code{Temperature} after each iteration.
#'
#' @param end_criteria A positive numeric value less than 1.
#' Iteration stops when temperature drops to below \code{end_criteria * Temperature}.
#' Default to be \eqn{10^-6}.
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
#' @param n_exchange Integer value. Determines how many blocks are exchanged
#' in order to produce a new solution for each iteration.
#' Should be a value larger than 1 and less than \code{nrow(block)}.
#'
#' @param prob_newitem A value between \emph{0} and \emph{1}.
#' Probability of choosing the strategy of picking a new item,
#' when not all candidate items are used to build the FC scale.
#'
#' @param use_IIA Logical. Are IIA metrics used when performing automatic pairing?
#'
#' @param rater_chars A \emph{p} by \emph{m} numeric matrix with scores of each of the
#' \emph{p} participants for the \emph{m} items. Ignored when \code{use_IIA == FALSE}.
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
#' @returns A list containing:
#' \itemize{
#'   \code{block_initial} Initial starting block
#'
#'   \code{energy_initial} Initial energy for \code{block_initial}
#'
#'   \code{block_final} Final paired block after optimization by SA
#'
#'   \code{energy_final} Final energy for \code{block_final}
#' }
#'
#' @note
#' The essence of SA is the probablistic acceptance of solutions inferior to
#' the current state, which avoids getting stuck in local maxima/minima.

#' It is also recommended to try out different values of
#' \code{weights, iia_weights, eta_Temperature} to find out the best
#' combination of initial temperature and energy value
#' in order to provide optimally paired blocks.
#'
#' @author Mengtong Li
#'
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
## Construct data frame for item characteristics and produce 20
## random triplet blocks with these 60 items
#'
#' item_df <- data.frame(Dimensions = item_dims,
#'                      Mean = item_mean, Difficulty = item_difficulty)
#' solution <- make_random_block(60, 60, 3)
#'
## Simple simulation of responses from 600 participants on the 60 items.
## In practice, should use real world data or simulation based on IRT parameters.
#'
#' item_responses <- matrix(sample(seq(1:5), 600*60, replace = TRUE), nrow = 60, byrow = TRUE)
#'
#' ## Automatic pairing, without use of IIAs
#' ## See ?facfun for information about what it does
#' \donttest{
#' sa_pairing_generalized(solution, 60, eta_Temperature = 0.01,
#'                                    r = 0.999, end_criteria = 0.001,
#'                                    weights = c(1,1,1),
#'                                    item_chars = item_df,
#'                                    FUN = c("facfun", "var", "var"))
#' }

#'
#' ## Automatic pairing, with IIAs
#' \donttest{
#' sa_pairing_generalized(solution, 60, eta_Temperature = 0.01,
#'                                    r = 0.999, end_criteria = 0.001,
#'                                    weights = c(1,1,1),
#'                                    item_chars = item_df,
#'                                    FUN = c("facfun", "var", "var"),
#'                                    use_IIA = TRUE,
#'                                    rater_chars = item_responses,
#'                                    iia_weights = c(BPlin = 1, BPquad = 1,
#'                                    AClin = 1, ACquad = 1))
#' }
#'
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
#' @importFrom irrCAC bp.coeff.raw gwet.ac1.raw
#' @importFrom stats runif
#'
#' @export


sa_pairing_generalized <- function(block, total_items, Temperature, eta_Temperature = 0.01,
                                   r = 0.999, end_criteria = 10^(-6),
                                   item_chars, weights, FUN, n_exchange = 2, prob_newitem = 0.25,
                                   use_IIA = FALSE, rater_chars,
                                   iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1)) {
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
  if (missing(total_items)) {
    total_items <- length(unique(block))
  }
  if (r >= 1 | r < 0) {
    stop("Invalid value for r: Should be a value between 0 and 1.")
  }
  if (end_criteria >= 1 | end_criteria < 0) {
    stop("Invalid value for end_criteria: Should be a value between 0 and 1.")
  }
  if (use_IIA & missing(rater_chars)) {
    stop("Item responses required if use_IIA = TRUE.")
  }
  if (prob_newitem > 1 | prob_newitem < 0) {
    stop("Invalid value for prob_newitem: Should be a value between 0 and 1.")
  }
  if (eta_Temperature < 0){
    stop("Invalid value for eta_Temperature: Should be a value larger than 0.")
  }
  if (!(n_exchange %% 1 == 0) | n_exchange > nrow(block) | n_exchange < 2) {
    stop("Invalid value for n_exchange: Should be an integer between 2 and nrow(block)")
  }


  # Store initial block of items and the corresponding energy.
  block0 <- block
  energy0 <- ifelse(!use_IIA, cal_block_energy(block, item_chars, weights, FUN),
                              cal_block_energy_with_iia(block, item_chars, weights, FUN,
                                                        rater_chars = rater_chars, iia_weights = iia_weights))
  energy <- energy0

  # Provide initial temperature value if it is not given, then store the initial temperature.
  if (missing(Temperature)) {
    Temperature <- eta_Temperature * abs(energy0)
  }
  T0 <- Temperature


  # How do we know that all items are used?
  all_item_used <- length(unique(c(block))) == total_items


  # When we see that all items are used......
  if (all_item_used) {
    while (Temperature > end_criteria * T0) {
      # 1. We randomly pick several blocks then calculate the total energy for these blocks first.
      sample_index <- sample(1:nrow(block), n_exchange)
      sample_block <- block[sample_index,]
      sample_energy <- ifelse(!use_IIA, cal_block_energy(sample_block, item_chars, weights, FUN),
                              cal_block_energy_with_iia(sample_block, item_chars, weights, FUN, rater_chars, iia_weights))

      l <- length(sample_block)

      # 2, Then we randomly shuffle items in these blocks and calculate the energy again.
      exchanged_items <- sample(sample_block,l)
      exchanged_block <- matrix(exchanged_items, nrow = n_exchange, byrow = TRUE)
      exchanged_energy <- ifelse(!use_IIA, cal_block_energy(exchanged_block, item_chars, weights, FUN),
                                 cal_block_energy_with_iia(exchanged_block, item_chars, weights, FUN, rater_chars, iia_weights))


      # 3. If the new energy is higher, then we replace these blocks with shuffled ones, and update the energy.
      if (exchanged_energy >= sample_energy) {
        for (i in seq(1:n_exchange)) {
          block[sample_index[i],] <- exchanged_block[i,]
        }
        # print("Accept better solutions")
        energy <- energy + exchanged_energy - sample_energy
      }
      # 4. Else, we accept the inferior solution with a (Potentially small) probability.
      else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
        # print("Accept Conditionally")
        for (i in seq(1:n_exchange)) {
          block[sample_index[i],] <- exchanged_block[i,]
        }
        energy <- energy + exchanged_energy - sample_energy
      }
      else {
        # print("Reject")
        ### No need to do anything.
      }
      # 5. Finally we update the temperature.
      Temperature <- Temperature * r
    }
  }
  # Otherwise...
  else {
    # 1. We first see proportion of items that are unused.
    while (Temperature > end_criteria * T0) {
      # 2. Choice of which method to use is determined by that proportion.
      if (prob_newitem > runif(1)) {
        # 2.1 Pick an unused item and a block. Calculate the energy for this block.
        unused_items <- setdiff(seq(1:total_items), block)
        sample_index <- sample(nrow(block),1)
        sample_block <- block[sample_index,,drop=FALSE]
        sample_energy <- ifelse(!use_IIA, cal_block_energy(sample_block, item_chars, weights, FUN),
                                cal_block_energy_with_iia(sample_block, item_chars, weights, FUN, rater_chars, iia_weights))

        # 2.2 Replace the first item in this block with this unused item. Calculate the energy for the block with item replaced.
        picked_item <- sample(unused_items, 1)
        exchanged_block <- sample_block
        exchanged_block[1] <- picked_item
        exchanged_energy <- ifelse(!use_IIA, cal_block_energy(exchanged_block, item_chars, weights, FUN),
                                   cal_block_energy_with_iia(exchanged_block, item_chars, weights, FUN, rater_chars, iia_weights))

        # 2.3 Determine whether the new block is accepted or rejected as is done above.
        if (exchanged_energy >= sample_energy) {
          block[sample_index[1],] <- exchanged_block
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          block[sample_index[1],] <- exchanged_block
          energy <- energy + exchanged_energy - sample_energy
        }
        else {
          ### No need to do anything.
        }
        Temperature <- Temperature * r
      }
      else {
        # 2.4 Or we pick several blocks and exchange their items, like what is done when all items are used.
        sample_index <- sample(1:nrow(block), n_exchange)
        sample_block <- block[sample_index,,drop=FALSE]
        sample_energy <- ifelse(!use_IIA, cal_block_energy(sample_block, item_chars, weights, FUN),
                                cal_block_energy_with_iia(sample_block, item_chars, weights, FUN, rater_chars, iia_weights))

        l <- length(sample_block)

        exchanged_items <- sample(sample_block,l)
        exchanged_block <- matrix(exchanged_items, nrow = n_exchange, byrow = TRUE)

        exchanged_energy <- ifelse(!use_IIA, cal_block_energy(exchanged_block, item_chars, weights, FUN),
                                   cal_block_energy_with_iia(exchanged_block, item_chars, weights, FUN, rater_chars, iia_weights))

        if (exchanged_energy >= sample_energy) {
          for (i in seq(1:n_exchange)) {
            block[sample_index[i],] <- exchanged_block[i,]
          }
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          for (i in seq(1:n_exchange)) {
            block[sample_index[i],] <- exchanged_block[i,]
          }
          energy <- energy + exchanged_energy - sample_energy
        }
        else {
          ### No need to do anything.
        }
        Temperature <- Temperature * r
      }
    }
  }
  return(list(block_initial = block0, energy_initial = energy0, block_final = block, energy_final = energy))
}
