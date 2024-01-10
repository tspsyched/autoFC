#' Build an initial blueprint for the FC scale
#'
#' @description TO BE DONE
#'
#' @usage TO BE DONE
#'
#' @param N_blocks  Number of total FC blocks
#' @param block_size  Desired block size for the FC scale
#' @param traits,signs  Optional vectors. If given, specifies which traits and signs
#' each item in the FC scale should have. \code{traits} is a string vector,
#' while \code{signs} is a numeric vector (1 for positive items and -1 for negative items)
#' 
#' @details TO BE DONE
#' @returns TO BE DONE
#' 
#' @author Mengtong Li
#'
#'
#' @examples TO BE DONE
#'
#' @export
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
