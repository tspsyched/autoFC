make_mplus_code_new <- function (data, iter = 2000, eta_file = "eta.csv") 
{
  # if (!is.TIRTdata(data)) {
  #   stop("'data' should be of class 'TIRTdata'. See ?make_TIRT_data")
  # }
  iter <- iter
  data <- convert_factors(data)
  data <- filter(data, .data$person == unique(.data$person)[1])
  att <- attributes(data)
  family <- check_family(att$family, "mplus")
  nitems <- att[["nitems"]]
  nitems_per_block <- att[["nitems_per_block"]]
  ntraits <- att[["ntraits"]]
  traits <- seq_len(ntraits)
  if (isTRUE(att[["partial"]])) {
    stop("Cannot yet handle partial comparisons when using Mplus.")
  }
  mplus_variable <- ""
  if (family %in% c("bernoulli", "cumulative")) {
    mplus_variable <- "CATEGORICAL ARE ALL;\n"
  }
  mplus_loadings <- vector("list", ntraits)
  for (i in traits) {
    for (n in seq_len(nrow(data))) {
      if (data$trait1[n] == i) {
        mplus_loadings[[i]] <- c(mplus_loadings[[i]], 
                                 with(data, paste0("i", item1[n], "i", item2[n], 
                                                   "*1  (L", item1[n], ")")))
      }
      else if (data$trait2[n] == i) {
        mplus_loadings[[i]] <- c(mplus_loadings[[i]], 
                                 with(data, paste0("i", item1[n], "i", item2[n], 
                                                   "*-1  (L", item2[n], "n)")))
      }
    }
    mplus_loadings[[i]] <- paste0("trait", i, " BY\n", paste0(mplus_loadings[[i]], 
                                                              collapse = "\n"), ";\n")
  }
  mplus_loadings <- collapse_default(unlist(mplus_loadings), "\n")
  mplus_fix_factor_variances <- collapse_default("trait", traits, "@1;\n")
  mplus_fix_factor_means <- collapse_default("[trait", traits,"@0];\n")
  mplus_factor_correlations <- collapse_default(sapply(1:(ntraits - 
                                                    1), function(i) paste0("trait", i, " WITH\n  ", paste0("trait", 
                                                                                                           (i + 1):ntraits, "*0", collapse = "\n  "), ";\n")))
  mplus_fix_factor_loadings <- ""
  items_both_dir <- which(1:nitems %in% data$item1 & 1:nitems %in% 
                            data$item2)
  if (length(items_both_dir)) {
    mplus_fix_factor_loadings <- collapse_default("L", items_both_dir, 
                                          " = -L", items_both_dir, "n;\n")
  }
  mplus_uniqueness <- with(data, collapse_default("i", item1, "i", 
                                          item2, "*1 (P", item1, "P", item2, ");\n"))
  mplus_cor_uniqueness <- ""
  for (n in 1:(nrow(data) - 1)) {
    for (m in (n + 1):nrow(data)) {
      pos_psi1 <- with(data, item1[n] == item1[m])
      pos_psi2 <- with(data, item2[n] == item2[m])
      neg_psi <- with(data, item2[n] == item1[m])
      if (pos_psi1) {
        mplus_cor_uniqueness <- with(data, paste0(mplus_cor_uniqueness, 
                                                  "i", item1[n], "i", item2[n], " WITH ", "i", 
                                                  item1[m], "i", item2[m], "*1 ", "(P", item1[n], 
                                                  ");\n"))
      }
      else if (pos_psi2) {
        mplus_cor_uniqueness <- with(data, paste0(mplus_cor_uniqueness, 
                                                  "i", item1[n], "i", item2[n], " WITH ", "i", 
                                                  item1[m], "i", item2[m], "*1 ", "(P", item2[n], 
                                                  ");\n"))
      }
      else if (neg_psi) {
        mplus_cor_uniqueness <- with(data, paste0(mplus_cor_uniqueness, 
                                                  "i", item1[n], "i", item2[n], " WITH ", "i", 
                                                  item1[m], "i", item2[m], "*-1 ", "(P", item2[n], 
                                                  "n);\n"))
      }
    }
  }
  mplus_equal_uniqueness <- ""
  if (nitems_per_block > 2) {
    psi_item1 <- paste0("P", data$item1)
    psi_item2 <- paste0("P", data$item2)
    neg_psi1 <- sapply(paste0(" \\(", psi_item1, "n\\);"), 
                       grepl, mplus_cor_uniqueness)
    neg_psi2 <- sapply(paste0(" \\(", psi_item2, "n\\);"), 
                       grepl, mplus_cor_uniqueness)
    mplus_equal_uniqueness <- with(data, collapse_default(psi_item1, 
                                                  psi_item2, " = ", ifelse(neg_psi1, paste0("- ", psi_item1, 
                                                                                            "n"), psi_item1), ifelse(neg_psi2, paste0("- ", 
                                                                                                                                      psi_item2, "n"), paste0(" + ", psi_item2)), ";\n"))
  }
  mplus_fix_uniqueness <- ""
  if (family %in% "bernoulli") {
    if (nitems_per_block > 2) {
      mplus_fix_uniqueness <- collapse_default("P", seq(1, nitems, 
                                                nitems_per_block), " = 1;\n")
    }
    else {
      psi_item1 <- paste0("P", data$item1)
      psi_item2 <- paste0("P", data$item2)
      mplus_fix_uniqueness <- collapse_default(psi_item1, psi_item2, 
                                       " = 1;\n")
    }
  }
  mplus_equal_items <- ""
  for (i in seq_along(att$dupl_items)) {
    first <- att$dupl_items[[i]][1]
    dup <- att$dupl_items[[i]][-1]
    mplus_equal_items <- paste0(mplus_equal_items, collapse_default("L", 
                                                            first, " = L", dup, ";\n"), collapse_default("P", first, 
                                                                                                 " = P", dup, ";\n"))
  }
  list(TITLE = "Thurstonian IRT model", DATA = collapse_lines("! It is assumed that the input file contains only item responses", 
                                                              "! Any additional variables should be added below"), 
       VARIABLE = collapse_lines(mplus_variable), ANALYSIS = collapse_lines("  ESTIMATOR = ulsmv;", "STARTS = 5;",
                                                                            paste0("  ITERATIONS = ", iter, ";"), "  PARAMETERIZATION = theta;\n"), 
       MODEL = collapse_lines("! factor loadings (lambda)", 
                              mplus_loadings, "! fix factor variances to 1", mplus_fix_factor_variances, 
                              "! mean of factor set to 0", mplus_fix_factor_means,
                              "! factor correlations", mplus_factor_correlations, 
                              "! declare uniquenesses (psi)", mplus_uniqueness, 
                              "! correlated uniqunesses", mplus_cor_uniqueness), 
       MODELCONSTRAINT = collapse_lines("! fix factor loadings of the same item to the same value", 
                                        mplus_fix_factor_loadings, "! pair's uniqueness is equal to sum of 2 utility uniqunesses", 
                                        mplus_equal_uniqueness, "! fix certain uniquenesses for identification", 
                                        mplus_fix_uniqueness, "! force item parameters of the same item to be equal", 
                                        mplus_equal_items, "! trait scores for individuals are estimated and saved in a file"), 
       SAVEDATA = collapse_lines(paste0("  FILE IS '", eta_file, 
                                        "';"), "  SAVE = FSCORES;"))
}

convert_factors <- function(data) {
  # data and code generating functions require
  # items and traits to be numeric
  # stopifnot(is.TIRTdata(data))
  for (v in c("item1", "item2", "trait1", "trait2")) {
    data[[v]] <- as.integer(data[[v]])
  }
  data
}

check_family <- function(family, software = NULL) {
  options <- family_options(software)
  match.arg(family, options)
}

family_options <- function(software = NULL) {
  if (is.null(software)) {
    # TODO: the 'beta' family is implemented in Stan but still
    # needs to be understood theoretically before exporting it
    all_ops <- c("bernoulli", "cumulative", "gaussian")
    return(all_ops)
  }
  software <- match.arg(software, c("stan", "lavaan", "mplus"))
  if (software == "stan") {
    out <- c("bernoulli", "cumulative", "gaussian")
  } else if (software == "lavaan") {
    out <- c("bernoulli", "gaussian")
  } else if (software == "mplus") {
    out <- c("bernoulli", "gaussian")
  }
  out
}

collapse_default <- function(..., sep = "") {
  # wrapper for paste with collapse = ""
  paste(..., sep = sep, collapse = "")
}


collapse_lines <- function(...) {
  dots <- c(...)
  paste0(dots, collapse = "\n")
}

TIRTfit <- function(fit, data) {
  version <- utils::packageVersion("thurstonianIRT")
  structure(nlist(fit, data, version), class = "TIRTfit")
}

nlist <- function(...) {
  # create a named list using object names
  m <- match.call()
  dots <- list(...)
  no_names <- is.null(names(dots))
  has_name <- if (no_names) FALSE
  else nzchar(names(dots))
  if (all(has_name)) return(dots)
  nms <- as.character(m)[-1]
  if (no_names) {
    names(dots) <- nms
  } else {
    names(dots)[!has_name] <- nms[!has_name]
  }
  dots
}
