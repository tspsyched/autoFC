#' Build a Blueprint Data Frame for the Focal FC Scale
#'
#' @description This function takes in specifications of block size, number of blocks,
#' as well as trait and keying of each item in these blocks, and returns a data frame
#' incorporating these information and ready to be further used for constructing FC
#' blocks by other functions like \code{build_scale_with_blueprint()}.
#'
#'
#' @param N_blocks  Number of total FC blocks
#' @param block_size  Desired block size for the FC scale
#' @param traits,signs  Optional vectors. If given, specifies which traits and signs
#' each item in the FC scale should have. \code{traits} is a string vector,
#' while \code{signs} is a numeric vector (1 for positive items and -1 for negative items)
#' 
#' @details A "blueprint" of the forced-choice scale is essentially a data frame
#' where each row represents one item in the forced-choice scale, and columns specify
#' which block the item belongs to, the trait that the item measures, and the keying of
#' that item.
#' 
#' Note that these are only the basic item information typically needed when matching items
#' into FC blocks; Users can further add other columns to the blueprint if they want to match
#' based on more criteria.
#' 
#' @returns A data frame, containing the block membership, trait and keying information of
#' all the items.
#' 
#' @author Mengtong Li
#'
#' @examples
#' example_blueprint <- construct_blueprint(N_blocks = 5, block_size = 3, 
#'                                          traits = sample(c("Openness", 
#'                                                            "Conscientiousness", 
#'                                                            "Extraversion", 
#'                                                            "Agreeableness", 
#'                                                            "Neuroticism"), 15, replace = TRUE),
#'                                          signs = sample(c(-1, 1), 15, replace = TRUE))
#'
#' @export
#' 
construct_blueprint <- function(N_blocks, block_size, traits, signs) {
   output_bp <- data.frame(block = rep(1:N_blocks, each = block_size),
                           item_num = rep(1:block_size, times = N_blocks))
   if (!missing(traits)) {
      output_bp$traits <- traits
   }
   if (!missing(signs)) {
      output_bp$signs <- signs
   }
   return(output_bp)
}
