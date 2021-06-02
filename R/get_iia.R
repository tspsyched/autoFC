get_iia <- function(block, data) {
  results <- cbind(BPlin = c(), BPquad = c(), AClin = c(), ACquad = c())
  for (i in seq(1:nrow(block))) {
    selected_item <- data[,block[i,]]
    BPlin <- bp.coeff.raw(selected_item, weights = "linear")$est[,4]
    BPquad <- bp.coeff.raw(selected_item, weights = "quadratic")$est[,4]
    AClin <- gwet.ac1.raw(selected_item, weights = "linear")$est[,4]
    ACquad <- gwet.ac1.raw(selected_item, weights = "quadratic")$est[,4]
    results <- rbind(results, c(BPlin = BPlin, BPquad = BPquad, AClin = AClin, ACquad = ACquad))
  }
  return(results)
}

