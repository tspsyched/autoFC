## check to make sure all names is in df and return df in appropriate order

check_names <- function(df, names){
  
  # pull out argument name AND round argument
  arg_char  <- deparse(substitute(df))
  bad_names <- setdiff(names, names(df))
  
  if(length(bad_names)){
    stop("column ", bad_names, " is not in ", arg_char, ".",
         call. = FALSE)
  } # END if STATEMENT
  
  return(df[names])
  
} # END check_names FUNCTION



## ensure argument is within numeric range

check_numeric <- function(arg,
                          min_number = 1){
  
  # pull out argument name AND round argument
  arg_char <- deparse(substitute(arg))
  arg      <- round(arg)
  
  # ensure that argument is within bounds
  if(!is.numeric(arg) || length(arg) != 1 || arg < min_number || arg == Inf){
    stop(arg_char, " must be a single number greater than ", min_number,
         call. = FALSE)
  } # END if STATEMENT
  
  return(arg)
} # END check_numeric FUNCTION
