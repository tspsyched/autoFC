arrange_by_vars <- function(df,
                            vars       = NULL,
                            decreasing = FALSE){
  
  # load dependencies
  require(roperators)
  
  # if df is a list, apply arrange_by_vars to the list
  if(is.irregular_list(df)){
    return(lapply(df,
                  FUN         = arrange_by_vars,
                  vars        = vars,
                  decreasing  = decreasing))
  } # END if STATEMENT
  
  ## checking arguments ##
  
  # if data frame is NULL, return NULL
  if(!length(df)){
    return(NULL)
  } # END if STATEMENT
  
  # make sure df is a data.frame #
  stopifnot(is.data.frame(df))
  
  # if vars is NULL, return df
  if(any_bad_for_calcs(vars)){
    return(df)
  } # END if STATEMENT
  
  # make sure vars is a numeric or character string
  stopifnot(is.numeric(vars) || is.character(vars))
  
  # make sure decreasing is a logical vector
  stopifnot(is.logical(decreasing))
  
  ## determining vars ##
  vars <- unique(vars)
  
  # if vars is numeric, round it and keep only vars in df
  if(is.numeric(vars)){
    vars <- as.integer(vars)
    vars <- intersect(vars,
                      seq_len(ncol(df)))
  } # END if STATEMENT
  
  # if vars is character, turn it into indices of df columns
  if(is.character(vars)){
    vars <- intersect(vars,
                      names(df))
    vars <- match(vars, names(df))
  } # END if STATEMENT
  
  # if we have no variables, return the df
  if(!length(vars)){
    return(df)
  } # END if STATEMENT
  
  ## determining decreasing ##
  
  # should be the same length as vars
  decreasing <- rep(decreasing,
                    length.out = length(vars))
  
  # - pull out columns of df
  # - order all of those columns based on decreasing
  # - arrange df based on order
  lapply(vars,
         function(ind)
           df[[ind]]) %>%
    do.call(what = function(...)
      order(...,
            decreasing = decreasing,
            method     = "radix")) %>%
    "["(df, ., , drop = FALSE)
} # END arrange_by_vars FUNCTION