#' Convert the TIRT Pairwise/Rank Response Data into Long Format Compatible with the thurstonianIRT Package
#' 
#' 
#' @description To estimate the TIRT model using the thurstonianIRT Package, the pairwise/rank data needs to 
#' be converted into long format. Current function serves as that purpose.
#'
#' @param block_info  Information data frame related to keying, dimension, and ID of each item in each block
#'
#' @param response_data  TIRT pairwise/rank response data. 
#' 
#' @param response_varname  Column names of TIRT pairwise/ranked responses. Can be generated from \code{build_TIRT_var_names()}.
#' 
#' @param format,direction,family,range  These parameters works the same as \code{thurstonianIRT::make_TIRT_data()}.
#' 
#' @param block_name,item_name,trait_name,sign_name  These parameters indicate the column names in \code{block_info} that specify
#' the following information of each item:
#' 
#' \code{block_name}: Which block does this item belong to?
#' \code{item_name}: What is the name of this item? 
#' \code{trait_name}: Which trait does this item belong to?
#' \code{sign_name}: What is the keying of this item?
#' 
#' @param verbose Logical. Should warning message be displayed?
#'
#' @details
#' This function is essentially a wrapper of \code{thurstonianIRT::make_TIRT_data()}
#' to allow more functionalities to be incorporated in a single function.
#'
#' @returns A long format data frame that is compatible with subsequent analyses using the thurstonianIRT package
#'
#' @seealso \code{thurstonianIRT::set_blocks_from_df()}, \code{thurstonianIRT::make_TIRT_data()}
#' @author Mengtong Li
#'
#' @import thurstonianIRT
#' 
#' @examples 
#' ## See example in convert_to_TIRT_response() for FC_resp
#' ## This example is just for demonstrative purposes showing how the long format data would look like.
#' \dontrun{get_TIRT_long_data(block_info = triplet_block_info,
#'                             response_data = FC_resp[,c(1:15)], response_varname = build_TIRT_var_names("i", 3, 5, format = "pairwise"),
#'                             trait_name = "Factor", sign_name = "Keying")}
#' 
#' @export
#'
#'
#'
get_TIRT_long_data <- function(block_info,                                         
                               response_data,                                       
                               response_varname,                                   
                               ## The following arguments works the same as in the thurstonianIRT package
                               format = "pairwise", 
                               partial = FALSE, 
                               direction = "larger",
                               family = "bernoulli",
                               range = c(0, 1),
                               ## The following arguments indicates which columns in block_info stores the information we need
                               # Which column indicates the block number of each item?
                               block_name = "Block", 
                               # Which column indicates item ID?
                               item_name = "ID", 
                               # Which column indicates item dimension?
                               trait_name = "Factor", 
                               # Which column indicates item keying?
                               sign_name = "Reversed",
                               verbose = TRUE) {
  if (verbose) {
    warning("We assume that you have already re-ordered the rows in block_info
             so that the first row represents the information of the first item in your FC scale.
             Ensure that you have already done so before using this function!")
  }

  block_info_response <- set_blocks_from_df(data = block_info,
                                            blocks = block_name,
                                            items = item_name,
                                            traits = trait_name,
                                            signs = sign_name)
  colnames(response_data) <- response_varname
  response_data_long <- make_TIRT_data(
    data = type.convert(response_data, as.is = TRUE),
    blocks = block_info_response,
    direction = direction,
    format = format,
    family = family,
    partial = partial,
    range = range
  )
  return(response_data_long)
}
