#' Example Triplet Response Data
#'
#' Responses to a 5-block triplet forced-choice measure, 
#' converted into pairwise comparisons. This dataset originates from Li et al.'s 
#' (in press) study on forced-choice measurement.
#'
#' @format A data frame with 541 rows and 15 columns.
#' \describe{
#'   \item{i1i2, i1i3, i2i3, ... i14i15}{All possible pairwise comparisons among items in the same block. With a triplet format, each block contains three items, producing three possible pairwise comparisons among these items.
#'   Therefore, i1, i2, and i3 belong to block 1, and so on.
#'   
#'   i1i2 represents the pairwise comparison indicating whether i1 is preferable over i2.
#'   If i1 is preferable over i2, then i1i2 = 1, else i1i2 = 0.}
#' }
#' @source https://osf.io/yvpz3/?view_only=08601755f471440b80973194571b60bd
#' @references 
#' Li, M., Zhang, B., Li, L., Sun, T., & Brown, A., (in press). Mixed-Keying or Desirability-Matching in the Construction of Forced-Choice Measures? An Empirical Investigation and Practical Recommendations. \emph{Organizational Research Methods}.
"triplet_example_data"