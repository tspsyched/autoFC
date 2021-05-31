facfun <- function(vec){
  if (typeof(vec) == "list") {vec <- unlist(vec)}
  return(ifelse(length(vec) == length(unique(vec)), 1, 0))
}
