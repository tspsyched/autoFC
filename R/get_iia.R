#' Helper Function for Outputting IIA Characteristics of Each Block
#'
#' @description This function prints IIA metrics for select items,
#'              given the individual responses for the items.
#'
#' @usage get_iia(block, data)
#'
#'
#' @param block An \emph{n} by \emph{k} integer matrix,
#'              where \emph{n} is the number of item blocks
#'              and \emph{k} is the number of items per block.
#' @param data  A \emph{p} by \emph{m} numeric matrix with scores of
#'              each of the \emph{p} participants for the \emph{m} items.
#'
#' @returns An \emph{n} by \emph{k} matrix indicating the four IIA metrics for each item block.
#'
#' @author Mengtong Li
#'
#' @examples
#'   item_responses <- matrix(sample(seq(1:5), 600*60, replace = TRUE), ncol = 60, byrow = TRUE)
#'   get_iia(matrix(seq(1:60), ncol = 3, byrow = TRUE), item_responses)
#'
#' @importFrom irrCAC bp.coeff.raw gwet.ac1.raw
#' @export


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
