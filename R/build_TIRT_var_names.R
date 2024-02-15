#' Build Variable Names for the Pairwise/Rank Responses in the TIRT Model
#'
#' @description This function builds the variable names that corresponds to
#' the pairwise comparisons or ranks among items within each block.
#'
#'
#' @param item_name The prefix you want to have for your response variables.
#'
#' @param block_size,N_blocks The block size and total number of the forced-choice scale. 
#' 
#' @param format What format should the converted responses be in? Can be \code{"pairwise"} or \code{"ranks"}.
#' 
#' @details Choose the correct \code{item_name} so that they are consistent with the item names
#' in the data frame storing information of the items.
#'
#'
#' @returns A vector of variable names 
#'
#' @author Mengtong Li
#'
#' @seealso \code{get_TIRT_long_data()}
#'
#' @examples
#' build_var_names("i", block_size = 3, N_blocks = 20, format = "pairwise")
#' build_var_names("i", block_size = 5, N_blocks = 12, format = "ranks")
#' 
#'
#' @export
#'
build_TIRT_var_names <- function(item_name = "i", block_size, N_blocks, format = "pairwise") {
  if (format == "ranks") {
    return(paste0(item_name, c(1:(block_size * N_blocks))))
  }
  else if (format == "pairwise") {
    temp_seq <- matrix(paste0(item_name, c(1:(block_size * N_blocks))), ncol = block_size, byrow = TRUE)
    temp_seq_2 <- matrix(c(apply(temp_seq, 1, combn, 2)), ncol = 2, byrow = TRUE)
    return(paste0(temp_seq_2[,1], temp_seq_2[,2]))
  }
}
