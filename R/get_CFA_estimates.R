#' Conduct Confirmatory Factor Analysis (CFA) and Obtain Parameter Estimates
#'
#' @description Reads in responses to Likert-type scales and a specified factor model,
#' performs CFA, and produces parameter estimates required for producing 
#' subsequent simulation data (i.e., use as inputs for \code{get_simulation_matrices()}).
#'
#' This function returns factor loadings, intercepts, residual variances, 
#' and covariances among latent variables
#'
#' @param response_data Likert-type response data. Requires the header including 
#' variable names.
#' 
#' @param fit_model A pre-specified CFA model written in lavaan syntax.
#'
#' @param item_names Names of the items you wish to obtain loadings, intercepts, 
#' and residuals. These variable names should appear in \code{response_data}.
#'
#'
#' @details
#' This function is essentially a wrapper for \code{lavaan::parameterEstimates()}
#' to obtain specific set of parameter estimates.
#' 
#' Notice that we assume your CFA model does not have hierarchical factor structure,
#' nor does it have cross loadings or correlated residuals.
#'
#'
#' @returns A list containing:
#' \itemize{
#'   \code{loadings} Item loadings for \code{item_names}
#'
#'   \code{intercepts} Item intercepts for \code{item_names}
#'
#'   \code{residuals} Item residual variances for \code{item_names}
#'
#'   \code{covariances} Covariances between latent variables defined in \code{fit_model}
#'   
#'   \code{model_fit} Model fit for \code{fit_model} on \code{response_data}
#' }
#'
#' @author Mengtong Li
#'
#'
#' @seealso \code{get_simulation_matrices()}
#'
#' @examples
#'
#' ## We have a small response sample data (N = 100) on the HEXACO-60 (Ashton & Lee, 2009) scale in the package
#' ## Names of the items are SS1-SS60 (Single-statement)
#' rating_data <- HEXACO_example_data
#' cfa_model <- paste0("H =~ ", paste0("SS", seq(6,60,6), collapse = " + "), "\n",
#'                    "E =~ ", paste0("SS", seq(5,60,6), collapse = " + "), "\n",
#'                    "X =~ ", paste0("SS", seq(4,60,6), collapse = " + "), "\n",
#'                    "A =~ ", paste0("SS", seq(3,60,6), collapse = " + "), "\n",
#'                    "C =~ ", paste0("SS", seq(2,60,6), collapse = " + "), "\n",
#'                    "O =~ ", paste0("SS", seq(1,60,6), collapse = " + "), "\n")
#' cfa_estimates <- get_CFA_estimates(response_data = rating_data,
#'                                    fit_model = cfa_model, 
#'                                    item_names = paste0("SS",c(1:60)))
#'
#' @importFrom lavaan cfa
#' 
#' @references 
#' Ashton, M. C., & Lee, K. (2009). The HEXACO–60: A short measure of the major dimensions of personality. 
#' \emph{Journal of personality assessment, 91}(4), 340-345. https://doi.org/10.1080/00223890902935878
#'
#' @export

get_CFA_estimates <- function(response_data, fit_model, item_names) {
  fit_object <- cfa(fit_model, data = response_data, std.lv = TRUE, meanstructure = TRUE)
  
  item_loadings <- parameterEstimates(fit_object) %>% filter(op == "=~")
  item_intercepts <- parameterEstimates(fit_object) %>% 
    filter(op == "~1" & (lhs %in% item_names))
  item_residuals <- parameterEstimates(fit_object) %>% 
    filter(op == "~~" & (lhs %in% item_names) & (lhs == rhs))
  lv_covariances <- as.matrix(inspect(fit_object, "cor.lv"))
  
  return(list(loadings = item_loadings, intercepts = item_intercepts,
              residuals = item_residuals, covariances = lv_covariances, model_fit = fit_object))     
}
