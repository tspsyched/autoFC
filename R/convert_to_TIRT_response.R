#' Convert the Latent Utility Values into Thurstonian IRT Pairwise/Rank Responses
#' with Pre-Specified Block Design
#'
#' @description This function simulates the responses to forced-choice blocks (both MOLE and RANK format), with
#' the raw responses converted into pairwise or rank data to be understood by the Thurstonian IRT model.
#'
#' @param Utility The utility matrix of all items. 
#'
#' @param block_design A numeric matrix specifying which items will be in the same forced-choice block (row).
#' 
#' @param N_response Number of simulated responses you wish to generate. Default to \code{nrow(Utility)}.
#'
#' @param format What format should the converted responses be in? Can be \code{"pairwise"} or \code{"ranks"}.
#'
#' @param partial Only used when \code{format == "ranks"}. Should partial ranking responses be produced?
#'
#' @param block_size,N_blocks The block size and total number of the forced-choice scale. 
#' Preferably left blank and obtained through \code{block_design}.
#' 
#' @param verbose Logical. Should warning message be displayed?
#' 
#' @note Importantly, the \code{Utility} matrix produced by \code{get_simulation_matrices()} may not be directly
#' used in this function because that utility matrix will have the item columns placed
#' in the order they appear in the CFA model, not in the original Item 1, Item 2...order.
#' Users need to re-order the columns of the \code{Utility} matrix produced by \code{get_simulation_matrices()} accordingly 
#' before feeding the utility matrix to this function.
#'
#' @details 
#' According to the Thurstonian IRT model, when a respondent needs to make a choice
#' between two items, they elicit a latent utility value for the two items and choose the item
#' that has a higher utility value. Choosing/Ranking among >2 items follows a similar procedure
#' where the respondent generate latent utility for each item and produces a ranking or preference.
#' 
#' For forced-choice blocks, the above choice procedure is conducted among the \code{block_size} items in the same block,
#' and the respondent can either indicate the most/least preferred item (MOLE format) or rank all the items
#' in terms of preference (RANK format). 
#' 
#' Regardless of the format, the raw responses to the forced-choice blocks need to be converted into
#' either all pairwise comparisons (\code{format = "pairwise"}), or a full ranking (\code{format = "ranks"}), 
#' among the the \code{block_size} items in the same block. 
#' 
#' We note that the when \code{block_size} is larger than 3 and when the MOLE format is used, some
#' pairwise comparisons among the items in the block will be missing by design. As for now, the current technique 
#' is not yet able to handle missing pairwise responses when \code{format = "pairwise"}.
#' Thus, if users wish to simulate responses to MOLE format blocks with \code{block_size} larger than 3, 
#' we recommend using \code{format = "ranks"} and also set \code{partial = TRUE}.
#' 
#'
#'
#' @returns A data frame containing pairwise (if \code{format == "pairwise"}) or
#' rank (if \code{format == "ranks"}) responses to each block for the \code{N_response} participants. 
#'
#' @author Mengtong Li
#'
#'
#' @examples 
#' rating_data <- HEXACO_example_data
#' cfa_model <- paste0("H =~ ", paste0("SS", seq(6,60,6), collapse = " + "), "\n",
#'                     "E =~ ", paste0("SS", seq(5,60,6), collapse = " + "), "\n",
#'                     "X =~ ", paste0("SS", seq(4,60,6), collapse = " + "), "\n",
#'                     "A =~ ", paste0("SS", seq(3,60,6), collapse = " + "), "\n",
#'                     "C =~ ", paste0("SS", seq(2,60,6), collapse = " + "), "\n",
#'                     "O =~ ", paste0("SS", seq(1,60,6), collapse = " + "), "\n")
#' cfa_estimates <- get_CFA_estimates(response_data = rating_data,
#'                                    fit_model = cfa_model, 
#'                                    item_names = paste0("SS",c(1:60)))
#' cfa_matrices <- get_simulation_matrices(loadings = cfa_estimates$loadings,
#'                                         intercepts = cfa_estimates$intercepts,
#'                                         residuals = cfa_estimates$residuals,
#'                                         covariances = cfa_estimates$covariances,
#'                                         N = 100, N_items = 60, N_dims = 6,
#'                                         dim_names = c("H", "E", "X", "A", "C", "O"),
#'                                         empirical = TRUE)
#' 
#' ### Re-order the Utility columns!
#' cfa_matrices$Utility <- cfa_matrices$Utility[,c(t(matrix(1:60, ncol = 6)[,6:1]))]
#' ### N_response need to be consistent with those specified in get_simulated_matrices()
#' FC_resp <- convert_to_TIRT_response(Utility = cfa_matrices$Utility,
#'                                     block_design = make_random_block(60, 60, 3),
#'                                     N_response = 100, format = "pairwise",
#'                                     block_size = 3, N_blocks = 20)
#' FC_rank_resp <- convert_to_TIRT_response(Utility = cfa_matrices$Utility,
#'                                          block_design = make_random_block(60, 60, 5),
#'                                          N_response = 100, format = "ranks",
#'                                          block_size = 5, N_blocks = 12) 
#' FC_rank_partial_resp <- convert_to_TIRT_response(Utility = cfa_matrices$Utility,
#'                                                  block_design = make_random_block(60, 60, 5),
#'                                                  N_response = 100, format = "ranks", partial = TRUE,
#'                                                  block_size = 5, N_blocks = 12)                                          
#' FC_resp
#' FC_rank_resp
#' FC_rank_partial_resp
#' 
#' @import thurstonianIRT
#'
#' @export
#' 
convert_to_TIRT_response <- function(Utility, block_design, format = "pairwise", partial = FALSE,
                                     block_size, N_blocks, N_response, verbose = TRUE) {
  if (verbose) {
    warning("We assume that you have already re-ordered the columns in the utility matrix 
        so that the first column represents the utility of your first item, etc.
        Ensure that you have already done so before using this function!")
  }

  # ## Each row represent each block.
  if (missing(block_size)) {
     block_size <- ncol(block_design)
  }
  if (missing(N_blocks)) {
     N_blocks <- nrow(block_design)
  }
  
  N_pairs <- block_size * (block_size - 1) / 2
  temp <- Utility[,as.vector(t(block_design))]
  
  combs <- t(combn(1:block_size, 2))
  
  if (format == "pairwise") {
    converted_FC_temp <- data.frame(matrix(0, nrow = N_response, ncol = N_pairs * N_blocks))
    colnames(converted_FC_temp) <- paste0("Item", rep(1:N_blocks, each = N_pairs), "_",
                                          apply(combn(LETTERS[1:block_size], 2), 2, paste, collapse = ""))
    for (i in 1:N_blocks) {
      col_indices <- (i-1) * block_size + combs
      for (j in 1:nrow(combs)) {
        converted_FC_temp[,(i-1) * N_pairs + j] <- ifelse(temp[,col_indices[j,1]] > temp[,col_indices[j,2]], 1, 0)
      }
    }    
  }
  else if (format == "ranks") {
    converted_FC_temp <- data.frame(matrix(0, nrow = N_response, ncol = block_size * N_blocks))
    colnames(converted_FC_temp) <- paste0("Item", rep(1:N_blocks, each = block_size), "_", LETTERS[1:block_size])
    for (i in 1:N_blocks) {
      rank_df <- temp[,(i*block_size-block_size+1):(i*block_size)]
      converted_FC_temp[,(i*block_size-block_size+1):(i*block_size)] <- block_size + 1 - t(apply(rank_df, 1, rank))
    }      
    if (partial == TRUE) {
      converted_FC_temp[converted_FC_temp > 1 & converted_FC_temp < block_size] <- round((1 + block_size)/2)
    }
  }
  else {
    return("Error in the argument \"format\": Must be [pairwise] or [ranks]")      
  }
  return(type.convert(converted_FC_temp, as.is = TRUE))
}