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
