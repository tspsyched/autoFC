#' Fit the Thurstonian IRT model with the long format response data and obtain necessary outputs.
#'
#' @description TODO
#'
#' @usage XX
#'
#' @param data_TIRT  Long format TIRT response data as generated from \code{get_TIRT_long_data} or \code(thurstonianIRT::make_TIRT_data).
#'
#' @param method  Estimation method for the TIRT model. Can be \code{"lavaan"}, \code{"mplus"} or \code{"stan"}.
#' 
#' @param lavaan_estimator  Which estimator to use when lavaan is chosen as the method of estimating the TIRT model. Defaults to "WLSMV".
#' 
#' @param stan_cores,chains,verbose  Parameters used in \code{thurstonianIRT::fit_TIRT_stan} 
#' 
#' @details TO BE DONE
#'
#' @seealso thurstonianIRT::fit_TIRT_lavaan, thurstonianIRT::fit_TIRT_mplus, thurstonianIRT::fit_TIRT_stan
#' @returns A list containing:
#' \itemize{
#'   \code{final_estimates}  Final trait score and standard error estimates
#'
#'   \code{fit_object}  TIRT model fit object
#'
#'   \code{responses_TIRT}  The long format TIRT response
#'
#'   \code{long_estimates}  Final trait score and standard error estimates, in long format
#'
#' }
#'
#' @author Mengtong Li
#'
#' @examples TO BE DONE
#'
#' @export
#'
fit_TIRT_model <- function(data_TIRT, method = "lavaan", lavaan_estimator = "WLSMV", stan_cores = 4, chains = 4, verbose = TRUE) {
  FC_results <- list()
  
  if (method == "lavaan") {
    fit_TIRT <- fit_TIRT_lavaan(data_TIRT, estimator = lavaan_estimator)
    traits_TIRT <- predict(fit_TIRT, newdata = data_TIRT)  
  }
  else if (method == "mplus") {
    warning("Due to the mechanics Mplus handles its input syntax, the input files generated here might not be correctly read by Mplus. We recommend using the Excel Macro developed by Brown & Maydeu-Olivares (2012) for generating Mplus syntax if you wish to use Mplus for estimating the TIRT model.")
    # print("Fitting TIRT")
    fit_TIRT <- fit_TIRT_mplus(data_TIRT)
    # print("Predicting traits")
    # print(fit_TIRT)
    traits_TIRT <- predict(fit_TIRT)           
  }
  else if (method == "stan") {
    fit_TIRT <- fit_TIRT_stan(data_TIRT, cores = stan_cores, chains = chains) 
    traits_TIRT  <- predict(fit_TIRT, newdata = data_TIRT, core = stan_cores, chains = chains) 
  }
  else {
    return("Estimation method must be lavaan, mplus, or stan")
  }
  
  print(traits_TIRT)
  if (method == "lavaan") {
    traits_TIRT_s <- traits_TIRT %>% pivot_wider(id_cols = id, names_from = trait,
                                                 values_from = c(estimate))
  }
  else {
    traits_TIRT_s <- traits_TIRT %>% pivot_wider(id_cols = id, names_from = trait,
                                                 values_from = c(estimate, se))        
  }
  
  
  traits_TIRT_s <- as.data.frame(traits_TIRT_s)
  
  
  FC_results$final_estimates <- traits_TIRT_s
  FC_results$fit_object <- fit_TIRT
  FC_results$responses_TIRT <- data_TIRT
  FC_results$long_estimates <- traits_TIRT
  
  return(FC_results)
}