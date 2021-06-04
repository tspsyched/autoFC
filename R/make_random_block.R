#' Construction of Random Item Blocks
#'
#' @description Returns a matrix of randomly paired blocks
#' where each row represents a block.
#'
#' @usage make_random_block(total_items, target_items = total_items, item_per_block)
#'
#'
#' @param total_items Integer value. Determines the total number of items we sample from.
#' @param target_items Integer value. Determines the number of items to use from
#'                     \code{total_items} to build item blocks.
#'                     Default to be equal to \code{total_items}. Should be no more than \code{total_items}.
#' @param item_per_block Integer value. Determines the number of items in each item block.
#'                       Should be no less than 2.
#' @details   Given the total number of items to pair from, number of items to build
#' paired blocks and number of items in each block,
#' \code{make_random_block} produces a matrix randomly paired blocks
#' where each row represents a block.
#'
#' It can also accommodate cases when \code{target_items}
#' is not a multiple of \code{item_per_block}.
#'
#' Can be used as initial solution for other functions in this package.
#'
#'
#' @returns   A matrix of integers indicating the item numbers, where the number of rows
#' equals \code{target_items} divided by \code{item_per_block}, rounded up,
#' and number of columns equals \code{item_per_block}.
#'
#' @author Mengtong Li
#'
#' @note  If \code{target_items} is not a multiple of \code{item_per_block},
#'        the item set produced by \code{target_items} will be looped
#'        until number of sampled items becomes a multiple of \code{item_per_block}.
#'
#' @examples
#' # Try out cases where you make target_items the default.
#' make_random_block(60, item_per_block = 3)
#'
#' # You can also set your own values of target_items.
#' make_random_block(60, 45, item_per_block = 3)
#'
#' # Also see what happens if target_items is not a multiple of item_per_block.
#' make_random_block(60, 50, item_per_block = 3)
#'
#' @export

make_random_block <- function(total_items, target_items = total_items, item_per_block) {
  if (!is.numeric(total_items)) {
    stop("Invalid data type for total_items. Expect positive integer values.")
  }
  else if (total_items %% 1 != 0) {
    stop("Value for total_items must be integer and positive.")
  }
  else if (total_items < 0) {
    stop("Value for total_items must be positive.")
  }

  if (!is.numeric(target_items)) {
    stop("Invalid data type for target_items. Expect positive integer values.")
  }
  else if (target_items %% 1 != 0) {
    stop("Value for target_items must be integer and positive.")
  }
  else if (target_items < 0) {
    stop("Value for target_items must be positive.")
  }


  if (!is.numeric(item_per_block)) {
    stop("Invalid data type for item_per_block. Expect positive integer values.")
  }
  else if (item_per_block %% 1 != 0) {
    stop("Value for item_per_block must be integer and larger than 1.")
  }
  else if (item_per_block < 1) {
    stop("Value for item_per_block must be larger than 1.")
  }



  if (target_items > total_items) {
    stop("Number of target items should not be larger than number of total items.")
  }
  if (item_per_block > target_items) {
    stop("item_per_block should not be larger than target_items.")
  }
  if (item_per_block > floor(total_items/2)) {
    stop("item_per_block value too big: Can't build two item blocks without duplicate items.")
  }


  item_indices <- sample(seq(1:total_items), target_items)
  if (target_items %% item_per_block == 0) {
    # If it is a multiple, return a matrix of item numbers.
    return(matrix(item_indices, ncol = item_per_block, byrow = TRUE))
  }
  else {
    # Otherwise, append the list of selected items until # of items is a multiple of item_per_block
    item_indices <- c(item_indices, item_indices[1:(item_per_block - (target_items %% item_per_block))])
    return(matrix(item_indices, ncol = item_per_block, byrow = TRUE))
  }

}
