#' Setup an initial FC scale that is consistent with the desired blueprint.
#'
#' @description TO BE DONE
#'
#' @usage TO BE DONE
#'
#' @param item_df  Data frame containing information related to all the available items
#' @param blueprint  Pre-specified blueprint for your blocks. Preferably constructed from \code{construct_blueprint()}
#' @param bp_block_name,bp_item_nums_name,bp_trait_name,bp_sign_name
#' Column names in the blueprint that specifies block number, item number in the block, desired trait of the item, and desired keying of the item, respectively
#' @param bp_criterion_name  Column name in the blueprint that indicates the additional matching criterion (cutoff value) you wish to test
#' @param df_item_nums_name,df_trait_name,df_sign_name,
#' Column names in \code{item_df} that specifies item_number, trait of the item, and keying of the item, respectively
#' @param df_matching_criteria_name  Column name in \code{item_df} that is used to evaluate the matching criterion specified in \code{bp_criterion_name}
#' @param df_matching_function  A character string containing function name for evaluating the matching criterion
#' @param df_matching_adjust_factor  A numeric value. If after \code{max_attempts_in_comb} attempts the additional
#' criteria in \code{df_matching_criteria_name} cannot be met (> the cutoff value specified in \code{blueprint[, bp_criterion_name]}), 
#' multiply that cutoff value by this adjusting factor.
#' @param max_attempts_in_comb  An interger value. How many attempts will be made for finding a block that satisfies the blueprint, before we adjust the cutoff value?
#' @param max_attempts_in_adjust  An interger value. How many attempts will be made for adjusting cutoff value? 
#' Will throw a warning and return the currently partially constructed scale (and specify which block might have problems) if number of attempts exceeds this value.
#' 
#' @details Although automatically finding the block combinations that can satisfy
#' multiple certain criteria for matching can be helpful, users may also wish to
#' have exact specifications for some blocks in many cases. This function serves as this purpose.
#' Typically in FC construction, we may want to explicitly specify the trait and keying combinations
#' so this function allows you to explicitly do that. Users are free to extend this function if further
#' exact specifications are needed.
#' 
#' For now, this function also allows users to specify one additional matching criterion for
#' the blocks. Users can designate the function for calculating this criterion and specify
#' a multiplicative adjusting factor, if the criterion fails to be met after a specified number of attempts.
#' One good example of matching criterion is matching in rating, where you want ratings of the items in the same block
#' to be less than a certain cutoff.
#'
#'
#' @returns TO BE DONE
#' 
#' @author Mengtong Li
#'
#' @import dplyr
#' @examples TO BE DONE
#'
#' @export
#' 
build_scale_with_blueprint <- function(item_df, blueprint,
                                       bp_block_name, bp_item_nums_name, 
                                       bp_trait_name, bp_sign_name, bp_criterion_name,
                                       df_item_nums_name, df_trait_name, df_sign_name,
                                       df_matching_criteria_name, df_matching_function,
                                       df_matching_adjust_factor, 
                                       max_attempts_in_comb, max_attempts_in_adjust) {
  picked_df <- data.frame()
  remaining_items <- item_df
  remaining_items$picked <- 0
  blocks <- blueprint[, bp_block_name]
  block_size <- max(blueprint[, bp_item_nums_name])
  
  
  ## This repeat loop is for a given block.
  for (current_block_ID in 1:max(blocks)) {
    ### We first extract the blueprint information for this block.
    current_block_in_bp <- blueprint[which(blueprint[, bp_block_name] == current_block_ID),]
    current_block_traits <- current_block_in_bp[, bp_trait_name]
    current_block_signs <- current_block_in_bp[, bp_sign_name]
    current_block_criterion <- current_block_in_bp[, bp_criterion_name]
    
    ### Then we check for trait and sign combination requirements, and see how many possible combinations we can get
    item_combs <- list()
    for (i in 1:block_size) {
      all_possible_items <- intersect(which(remaining_items[, df_trait_name] == current_block_traits[i] & remaining_items$picked == 0),
                                      which(remaining_items[, df_sign_name] == current_block_signs[i] & remaining_items$picked == 0))
      item_combs[[i]] <- all_possible_items
    }
    ### Make a Catersian product of all the combinations that satisfies the trait/sign combination
    all_possible_combs <- as.matrix(expand.grid(item_combs))
    ## But of course, sometimes there are simply no combinations to be found.
    ## If that happens we launch a warning, but still give them the partially built scale.
    if (nrow(all_possible_combs) == 0) {
      warning("It seems like the block with specified combination of traits and signs cannot be built. Try running the function again, or if problem repeatedly persists, check your blueprint.")
      warning(paste0("Problem appears when constructing block ", current_block_ID))
      return(picked_df)
    }
    
    ### If there are combinations, then start trying
    N_attempts_in_adjust <- 0
    block_success <- FALSE
    ## This repeat loop is for finding a block that satisfies the specification of your blueprint.
    repeat {
      ## Look for one combination that meets the matching criteria, if any.
      ## We try for max_attempts_in_comb times.
      matched_blocks <- NULL
      success_comb <- NULL
      for (i in 1:max_attempts_in_comb) {
        ### Randomly select one combination
        selected_comb <- sample(1:nrow(all_possible_combs), 1)
        temp_df <- remaining_items[c(all_possible_combs[selected_comb, ]),]
        ## Succeed in meeting the criteria?
        if (eval(call(df_matching_function, temp_df[, df_matching_criteria_name, drop = TRUE]))
            <= current_block_criterion[1]) {
          matched_blocks <- temp_df
          success_comb <- c(all_possible_combs[selected_comb, ])
          break
        }
        else {
          next
        }
      }
      if (!is.null(matched_blocks)) {
        ## Succeed? This block satisfies the specification of blueprint!
        matched_blocks$N_adjusts <- N_attempts_in_adjust
        matched_blocks$final_criteria <- current_block_criterion[1]
        picked_df <- rbind(picked_df, matched_blocks)
        remaining_items$picked[success_comb] <- 1
        ## And we are done with this block!
        block_success <- TRUE
        break
        
      }
      else {
        ## Else we might need to relax the criterion for matching
        N_attempts_in_adjust <- N_attempts_in_adjust + 1
        if (N_attempts_in_adjust > max_attempts_in_adjust) {
          warning(paste0("It seems like we cannot find a match for your blueprint after ", max_attempts_in_adjust,
                         " attempts in relaxing the matching criteria. Consider increasing max_attempts_in_adjust and/or max_attempts, or adjust your blueprint."))
          warning(paste0("Problem appears when constructing block ", current_block_ID))
          return(picked_df)
        }
        else {
          current_block_criterion <- current_block_criterion * df_matching_adjust_factor
          ## And try again           
        }
        
      }
    }
    ### If you make it through until here, you should have already succeeded in constructing this block.
    if (block_success){
      next
    }
    
    
  }
  return(picked_df)
  
}