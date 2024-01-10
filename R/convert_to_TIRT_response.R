#' Convert the latent utility values into Thurstonian IRT pairwise/rank responses
#' with a pre-specified block design
#'
#' @description TO BE DONE
#'
#' @usage TO BE DONE
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
#'
#' @details TODO
#'
#'
#' @returns A data frame containing pairwise (if \code{format == "pairwise"}) or
#' rank (if \code{format == "ranks}) responses to each block for the \code{N_response} participants. 
#'
#' @author Mengtong Li
#'
#'
#' @examples TO BE DONE
#' 
#' 
#' @import thurstonianIRT
#'
#' @export
#' 
convert_to_TIRT_response <- function(Utility, block_design, format = "pairwise", partial = FALSE,
                                     block_size, N_blocks, N_response) {
  warning("We assume that you have already re-ordered the columns in the utility matrix 
        so that the first column represents the utility of your first item, etc.
        Ensure that you have already done so before using this function!")
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