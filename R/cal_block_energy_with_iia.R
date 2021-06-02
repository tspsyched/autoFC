cal_block_energy_with_iia <- function(block, item_chars, weights, FUN, rater_chars,
                                      iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1),
                                      verbose = FALSE) {
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
    # Then we add up IIA metrics.
    selected_item <- rater_chars[,block[row,]]
    BPlin <- bp.coeff.raw(selected_item, weights = "linear")$est[,4]
    BPquad <- bp.coeff.raw(selected_item, weights = "quadratic")$est[,4]
    AClin <- gwet.ac1.raw(selected_item, weights = "linear")$est[,4]
    ACquad <- gwet.ac1.raw(selected_item, weights = "quadratic")$est[,4]
    iia_energy <- iia_weights %*% c(BPlin, BPquad, AClin, ACquad)
    if (verbose == TRUE) {
      print(sprintf("BPlin: %f, BPquad: %f, AClin: %f, ACquad: %f", BPlin, BPquad, AClin, ACquad))
    }
    energy <- energy + iia_energy

  }
  return(energy)
}
