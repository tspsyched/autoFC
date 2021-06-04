#' Function for Checking If All Items in a Vector Are Unique
#'
#' @description Returns \emph{1} if each element in the vector is unique,
#' and \emph{0} otherwise.
#'
#' @usage facfun(vec)
#'
#'
#' @param vec Input vector.
#'
#' @returns \emph{1} if each element in the vector is unique,
#' and \emph{0} otherwise.
#'
#' @author Mengtong Li
#'
#' @examples
#'   facfun(c("Openness", "Neuroticism", "Agreeableness"))
#'   facfun(c("Openness", "Openness", "Agreeableness"))
#' @export

facfun <- function(vec){
  if (typeof(vec) == "list") {vec <- unlist(vec)}
  if (length(vec) == 0) {stop("Should input a vector or list with elements.")}
  return(ifelse(length(vec) == length(unique(vec)), 1, 0))
}
