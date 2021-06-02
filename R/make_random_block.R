make_random_block <- function(total_items, target_items = total_items, item_per_block) {
  if (target_items > total_items) {
    stop("Number of target items should not be larger than number of total items")
  }
  if (target_items <= 0) {
    stop("Number of target items should be larger than 0")
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
