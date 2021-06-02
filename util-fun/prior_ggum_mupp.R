#' Prior GGUM Distributions
#'
#' Simulate prior distribution parameters for the GGUM model
#'
#' @param x vector of quantiles
#' @param n number of observations required for the simulation
#' @param ... additional arguments to pass to the prior functions
#'
#' @author Steven Nydick, \email{steven.nydick@@kornferry.com}
#'
#' @importFrom extraDistr rnsbeta dnsbeta
#'
#' @name prior_ggum
#'
NULL

#' @rdname prior_ggum
#' @importFrom stats
#'             dnorm
#' @export
d_thetas_prior <- function(x, ...){
  dnorm(x, ...)
} # END d_thetas_prior FUNCTION

#' @rdname prior_ggum
#' @importFrom utils
#'             modifyList
#' @export
d_alpha_prior <- function(x, ...){
  params <- list(x      = x,
                 shape1 = 1.5,
                 shape2 = 1.5,
                 min    = 0.25,
                 max    = 4.00)
  params <- modifyList(params, list(...))
  
  do.call(dnsbeta, params)
} # END d_alpha_prior FUNCTION

#' @rdname prior_ggum
#' @importFrom utils
#'             modifyList
#' @export
d_delta_prior <- function(x, ...){
  params <- list(x      = x,
                 shape1 = 2.0,
                 shape2 = 2.0,
                 min    = -4.0,
                 max    = +4.0)
  params <- modifyList(params, list(...))
  
  do.call(dnsbeta, params)
} # END d_delta_prior FUNCTION

#' @rdname prior_ggum
#' @importFrom utils
#'             modifyList
#' @export
d_tau_prior <- function(x, ...){
  params <- list(x      = x,
                 shape1 = 2.0,
                 shape2 = 2.0,
                 min    = -4.0,
                 max    = +4.0)
  params <- modifyList(params, list(...))
  
  do.call(dnsbeta, params)
} # END d_tau_prior FUNCTION


#' @rdname prior_ggum
#' @importFrom stats
#'             rnorm
#' @export
r_thetas_prior <- function(n, ...){
  rnorm(n, ...)
} # END r_thetas_prior FUNCTION

#' @rdname prior_ggum
#' @importFrom utils
#'             modifyList
#' @export
r_alpha_prior <- function(n, ...){
  params <- list(n      = n,
                 shape1 = 1.5,
                 shape2 = 1.5,
                 min    = 0.25,
                 max    = 4.00)
  params <- modifyList(params, list(...))
  
  do.call(rnsbeta, params)
} # END r_alpha_prior FUNCTION

#' @rdname prior_ggum
#' @importFrom utils
#'             modifyList
#' @export
r_delta_prior <- function(n, ...){
  params <- list(n      = n,
                 shape1 = 2.0,
                 shape2 = 2.0,
                 min    = -4.0,
                 max    = +4.0)
  params <- modifyList(params, list(...))
  
  do.call(rnsbeta, params)
} # END r_delta_prior FUNCTION

#' @rdname prior_ggum
#' @importFrom utils
#'             modifyList
#' @export
r_tau_prior <- function(n, ...){
  params <- list(n      = n,
                 shape1 = 2.0,
                 shape2 = 2.0,
                 min    = -4.0,
                 max    = +4.0)
  params <- modifyList(params, list(...))
  
  do.call(rnsbeta, params)
} # END r_tau_prior FUNCTION