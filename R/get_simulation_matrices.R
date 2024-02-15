#' Generate Simulated Person and Item Parameter Matrices for the Thurstonian IRT Model
#' Based on Confirmatory Factor Analysis Results
#'
#' @description This function takes in factor analysis results from \code{lavaan::cfa()} or \code{get_CFA_estimates()}, and
#' generates simulated person and item parameter matrices for the Thurstonian IRT model. 
#' The latent "utility" value of each item for each simulated person is also produced. 
#'
#'
#' @param loadings,intercepts,residuals,covariances 
#' Data frame of factor loadings, intercepts, residuals and latent variable covariances,
#' preferably obtained from \code{get_CFA_estimates()}, or extracted from \code{lavaan::parameterEstimates()}.
#' 
#' @param N Number of simulated responses you wish to generate.
#'
#' @param N_items Optional. Total number of response items. Default to the number of rows
#' in \code{loadings}.
#'
#' @param N_dims Optional. Total number of response items. Default to the length of \code{dim_names}.
#'
#' @param dim_names Name of the latent variables (dimensions); Order should be consistent with 
#' how they appear in your CFA model as you have specified in \code{get_CFA_estimates()}.
#' 
#' @param empirical As in \code{MASS::mvrnorm()};  Should mu and sigma specify the empirical, 
#' rather than population mean and covariance?
#'
#'
#' @details
#' Based on the Thurstonian IRT model (Brown & Maydeu-Olivares, 2011), this function
#' generates the latent utility value of \code{N_item} Likert items for each of the 
#' \code{N} participants.
#' 
#' Readers can refer to Brown & Maydeu-Olivares (2011) and the online tutorial in
#' Li et al., (in press) for detailed description of simulation procedures. 
#'
#'
#'
#' @returns A list containing:
#' \itemize{
#'   \code{Lambda} Item loading matrix specifying which items load onto which dimension,
#'
#'   \code{Mu} Item intercept matrix,
#'
#'   \code{Epsilon} Item residual matrix,
#'
#'   \code{Theta} Simulated latent scores for each of the \code{N_dims} dimensions for all \code{N} simulated respondents,
#'   
#'   \code{Utility} latent utility value of \code{N_item} Likert items for each of the \code{N} participants.
#' }
#'
#' @author Mengtong Li
#'
#'
#' @seealso \code{get_CFA_estimates()}
#'
#' @examples 
#' rating_data <- HEXACO_example_data
#' cfa_model <- paste0("H =~ ", paste0("SS", seq(6,60,6), collapse = " + "), "\n",
#'                     "E =~ ", paste0("SS", seq(5,60,6), collapse = " + "), "\n",
#'                     "X =~ ", paste0("SS", seq(4,60,6), collapse = " + "), "\n",
#'                     "A =~ ", paste0("SS", seq(3,60,6), collapse = " + "), "\n",
#'                     "C =~ ", paste0("SS", seq(2,60,6), collapse = " + "), "\n",
#'                     "O =~ ", paste0("SS", seq(1,60,6), collapse = " + "), "\n")
#' cfa_estimates <- get_CFA_estimates(response_data = rating_data,
#'                                    fit_model = cfa_model, 
#'                                    item_names = paste0("SS",c(1:60)))
#' cfa_matrices <- get_simulation_matrices(loadings = cfa_estimates$loadings,
#'                                         intercepts = cfa_estimates$intercepts,
#'                                         residuals = cfa_estimates$residuals,
#'                                         covariances = cfa_estimates$covariances,
#'                                         N = 100, N_items = 60, N_dims = 6,
#'                                         dim_names = c("H", "E", "X", "A", "C", "O"),
#'                                         empirical = TRUE)
#' 
#' @importFrom MASS mvrnorm
#'
#' @references 
#' Brown, A., & Maydeu-Olivares, A. (2011). Item response modeling of forced-choice questionnaires. \emph{Educational and Psychological Measurement, 71}(3), 460-502. https://doi.org/10.1177/0013164410375112
#' Li, M., Zhang, B., Li, L., Sun, T., & Brown, A., (2024). Mixed-Keying or Desirability-Matching in the Construction of Forced-Choice Measures? An Empirical Investigation and Practical Recommendations. \emph{Organizational Research Methods}. https://doi.org/10.1177/10944281241229784
#'
#'
#' @export

get_simulation_matrices <- function(loadings, intercepts, residuals, covariances,  ### Required components from CFA. Preferably generated from lavaan::cfa()
                                    N,            ## Simulated sample size
                                    N_items,      ## Total items
                                    N_dims,       ## Total dimensions
                                    dim_names,    ## Name of the dimensions; Order should be consistent with how they appear in the CFA model
                                    empirical)     ## As in MASS:mvrnorm; Should mu and sigma specify the empirical, rather than population mean and covariance?
{
  # Factor loading matrix
  if (missing(N_items)) {
     N_items <- nrow(loadings)
  }
  if (missing(N_dims)) {
     N_dims <- length(dim_names)
  }
  Lambda <- matrix(0, nrow = N_items, ncol = N_dims)
  colnames(Lambda) <- dim_names
  for (dn in dim_names) {
    item_id <- which(loadings$lhs == dn)
    Lambda[item_id, dn] <- loadings[item_id, "est"]
  }
  
  # Intercept matrix
  Mu <- matrix(rep(intercepts$est, N), ncol = N_items, byrow = TRUE)
  
  # Residual matrix. Only support non-correlated residuals for now
  Epsilon <- mvrnorm(N, mu = rep(0, N_items), Sigma = diag(residuals$est), 
                     empirical = empirical)
  
  # Outcome vector (Theta)
  Theta <- mvrnorm(N, mu = rep(0, N_dims), Sigma = covariances, 
                   empirical = empirical)
  
  # Overall utility based on the Thurstonian IRT model
  Utility <- Mu + Theta %*% t(Lambda) + Epsilon
  
  return(list(Lambda = Lambda, Mu = Mu,
              Epsilon = Epsilon, Theta = Theta,
              Utility = as.data.frame(Utility)))
}