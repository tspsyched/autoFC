#' Fit the Thurstonian IRT Model with Long Format Response Data
#'
#' @description Fits the Thurstonian IRT response model using either lavaan, Mplus, or stan methods. 
#' A long format response data set needs to be provided.
#'
#'
#' @param data_TIRT Long format TIRT response data as generated from \code{get_TIRT_long_data()} or \code{thurstonianIRT::make_TIRT_data()}.
#'
#' @param method Estimation method for the TIRT model. Can be \code{"lavaan"}, \code{"mplus"} or \code{"stan"}.
#' 
#' @param lavaan_estimator Which estimator to use when lavaan is chosen as the method of estimating the TIRT model. Defaults to "WLSMV".
#' 
#' @param remove_file Whether the input/output files will be removed after model estimation, when Mplus is chosen as the method of estimating the TIRT model.
#' 
#' @param stan_cores,chains,verbose,iter Parameters used in \code{thurstonianIRT::fit_TIRT_stan} 
#' 
#' @details 
#' This function incorporates the fit TIRT models functions in the \code{thurstonianIRT} package (Bürkner, 2019) and
#' by (a) providing a wrapper interface for users to choose from estimating from lavaan, MPLUS, or stan, (b) placing
#' the fit object, resulting trait estimates, and the original long format response data into one list as the return object.
#' Users need to provide a long format TIRT response data set as generated from \code{get_TIRT_long_data()} or from \code{thurstonianIRT::make_TIRT_data()},
#' and they can choose from three estimation methods: lavaan, MPLUS or stan. For lavaan and stan, additional arguments can be specified.
#' 
#' We note that currently the lavaan method does not provide standard error estimates. The stan method is the most stable
#' but can take a very long time for estimation. The mplus method can be a good choice but may occassionally produce errors
#' due to model convergence issues. In these rare cases, users may also consider using the Excel macro
#' developed by Brown & Maydeu-Olivares (2012) to generate Mplus syntax and directly run the syntax in Mplus.
#' 
#' @references
#' Bürkner, P. C. (2019). thurstonianIRT: Thurstonian IRT models in R. \emph{Journal of Open Source Software, 4}(42), 1662. https://doi.org/10.21105/joss.01662
#' 
#' Brown, A., & Maydeu-Olivares, A. (2012). Fitting a Thurstonian IRT model to forced-choice data using Mplus. \emph{Behavior Research Methods, 44}, 1135-1147. https://doi.org/10.3758/s13428-012-0217-x
#'
#' @seealso 
#'    \code{thurstonianIRT::fit_TIRT_lavaan},
#'    
#'    \code{thurstonianIRT::fit_TIRT_mplus}, 
#'    
#'    \code{thurstonianIRT::fit_TIRT_stan}
#'    
#' @returns A list containing:
#' \itemize{
#'   \code{final_estimates}   Final trait score and standard error estimates
#'
#'   \code{fit_object}   TIRT model fit object
#'
#'   \code{responses_TIRT}   The long format TIRT response
#'
#'   \code{long_estimates}   Final trait score and standard error estimates, in long format
#'
#' }
#'
#' @author Mengtong Li
#'
#' @examples 
#' set.seed(2024)
#' test_data <- triplet_example_data
#' block_info <- triplet_block_info
#' test_data_long <- get_TIRT_long_data(block_info = triplet_block_info, response_data = test_data,
#'                                      response_varname = build_TIRT_var_names(N_blocks = 5, block_size = 3, item_name = "i"),
#'                                      block_name = "Block", item_name = "ID", trait_name = "Factor", sign_name = "Keying")
#' \dontrun{
#'     test_fit <- fit_TIRT_model(test_data_long, method = "lavaan")
#'     test_fit$fit_object
#'     test_fit$final_estimates
#' }
#'
#' @export
#'


fit_TIRT_model <- function(data_TIRT, method = "lavaan", lavaan_estimator = "WLSMV", 
                           stan_cores = 4, chains = 4, iter = 2000, verbose = TRUE, remove_file = FALSE) {
  FC_results <- list()
  
  if (method == "lavaan") {
    fit_TIRT <- fit_TIRT_lavaan(data_TIRT, estimator = lavaan_estimator)
    traits_TIRT <- predict(fit_TIRT, newdata = data_TIRT)  
  }
  else if (method == "mplus") {
    # warning("Due to the mechanics Mplus handles its input syntax, the input files generated here might not be correctly read by Mplus. We recommend using the Excel Macro developed by Brown & Maydeu-Olivares (2012) for generating Mplus syntax if you wish to use Mplus for estimating the TIRT model.")
    # print("Fitting TIRT")
    fit_TIRT <- fit_TIRT_mplus_new(data_TIRT, remove_file)
    # print("Predicting traits")
    # print(fit_TIRT)
    traits_TIRT <- predict(fit_TIRT)           
  }
  else if (method == "stan") {
    fit_TIRT <- fit_TIRT_stan(data_TIRT, cores = stan_cores, chains = chains) 
    traits_TIRT  <- predict(fit_TIRT, newdata = data_TIRT, core = stan_cores, chains = chains, iter = iter) 
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
