#' Produce simuated matrices based on the Thurstonian IRT model
#'
#' @description TO BE DONE
#'
#' @usage get_simulation_matrices(loadings, intercepts, residuals, covariances,
#'                                N, N_items, N_dims, dim_names, empirical)
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
#'   \code{Theta} Simulated latent scores for each of the \code{N_dims} dimensions for all \code{N} simualated respondents,
#'   
#'   \code{Utility} latent utility value of \code{N_item} Likert items for each of the \code{N} participants.
#' }
#'
#' @author Mengtong Li
#'
#'
#' @seealso \code{get_CFA_estimates}
#'
#' @examples TO BE DONE
#' 
#' 
#' @importFrom MASS mvrnorm
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