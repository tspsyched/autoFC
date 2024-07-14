#' Construct Forced-Choice Blocks Aligned with the Specifications in a Blueprint
#'
#' @description This function takes in the information of all available items as well as
#' a blueprint data frame specifying the design of blocks, and returns a data frame of item
#' blocks consistent with the blueprint (if possible).
#'
#'
#' @param item_df  Data frame containing information related to all the available items
#' @param blueprint  Pre-specified blueprint for your blocks. Preferably constructed from \code{construct_blueprint()}
#' @param bp_block_name,bp_item_nums_name,bp_trait_name,bp_sign_name
#' Column names in the blueprint that specifies block number, item number in the block, desired trait of the item, and desired keying of the item, respectively
#' @param bp_matching_criterion_name  Column name in the blueprint that indicates the additional matching criterion (cutoff value) you wish to test
#' @param df_item_nums_name,df_trait_name,df_sign_name,
#' Column names in \code{item_df} that specifies item_number, trait of the item, and keying of the item, respectively
#' @param df_matching_criterion_name  Optional. Column name in \code{item_df} that is used to evaluate the matching criterion specified in \code{bp_matching_criterion_name}
#' @param df_matching_function  Optional. A character string containing function name for evaluating the matching criterion
#' @param df_matching_adjust_factor  Optional. A numeric value. If after \code{max_attempts_in_comb} attempts the additional
#' criteria in \code{df_matching_criterion_name} cannot be met (> the cutoff value specified in \code{blueprint[, bp_matching_criterion_name]}), 
#' multiply that cutoff value by this adjusting factor.
#' @param max_attempts_in_comb  Optional. An integer value. How many attempts will be made for finding a block that satisfies the blueprint, before we adjust the cutoff value?
#' @param max_attempts_in_adjust  Optional. An integer value. How many attempts will be made for adjusting cutoff value? 
#' Will throw a warning and return the currently partially constructed scale (and specify which block might have problems) if number of attempts exceeds this value.
#' 
#' @details Although automatically finding the block combinations that can satisfy
#' multiple certain criteria for matching can be helpful (as the primary functionality of the previous
#' version of \code{autoFC} is about), users may also wish to have exact specifications for some blocks in many cases. 
#' For example, typically in FC construction, we may want to explicitly specify the trait and keying combinations
#' for each block. This function allows you to explicitly do that. Users are free to extend this function if further
#' exact specifications are needed.
#' 
#' For now, this function also allows users to specify one additional matching criterion for
#' the blocks. Users can designate the function for calculating this criterion (\code{df_matching_function}) and specify
#' a multiplicative adjusting factor (\code{df_matching_adjust_factor}), 
#' if the criterion fails to be met after a specified number of attempts (\code{max_attempts_in_comb}).
#' One good example of matching criterion is matching in social desirability rating, where you want ratings of the items in the same block
#' to be less than a certain cutoff.
#' 
#' If after a certain number of times (\code{max_attempts_in_adjust}) the given block is still unable to be constructed 
#' (i.e., criterion matching still fails even if we relax the cutoff multiple times), a warning message will be shown
#' and a partially built scale will be returned. Warnings along with a partially built scale may also be returned 
#' when it is impossible for the remaining items in \code{item_df} to satisfy the specification in the blueprint
#'  (e.g. we have no items for trait1 left, but the blueprint requires a block with an item measuring trait1).
#'
#' @seealso \code{construct_blueprint()}
#'
#' @returns A data frame containing the selected items for each specified block.
#' If matching criteria is specified, the data frame will also contain the number of
#' times we adjusted the cutoffs for each block, and the final matching criteria cutoff
#' resulting from adjustments.
#' 
#' @author Mengtong Li
#'
#' @import dplyr
#' @examples
#' #### For the case you do not need additional matching criterion
#' item_info <- triplet_block_info
#' test_bp <- construct_blueprint(N_blocks = 2, block_size = 3, 
#'                                traits = c("honestyhumility", "emotionality", "extraversion",
#'                                           "agreeableness", "conscientiousness", "openness"),
#'                                signs = c(-1, 1, 1,
#'                                          -1, -1, -1))
#' ### Some arguments can be omitted if you don't have extra matching criteria.
#' picked_scale <- build_scale_with_blueprint(item_df = item_info,
#'                                            blueprint = test_bp,
#'                                            ### These parameters are column names in test_bp
#'                                            bp_block_name = "block",
#'                                            bp_item_nums_name = "item_num",
#'                                            bp_trait_name = "traits",
#'                                            bp_sign_name = "signs",
#'                                            ### These parameters are column names in item_info
#'                                            df_item_nums_name = "ID",
#'                                            df_trait_name = "Factor",
#'                                            df_sign_name = "Keying")
#' 
#' #### Or you may want to match social desirability ratings, for example
#' test_bp2 <- test_bp
#' test_bp2$SD_matching <- rep(0.5, 15)
#' 
#' #### Suppose that the items also have their own ratings
#' item_info2 <- item_info
#' item_info2$SD_rating <- rnorm(15, 3.5, 1)
#' range_m <- function(x) {
#'   return(max(x) - min(x))
#' }
#' 
#' ### Some parameters can be omitted if you don't have extra matching criteria.
#' picked_scale2 <- build_scale_with_blueprint(item_df = item_info,
#'                                             blueprint = test_bp,
#'                                             ### These parameters are column names in test_bp
#'                                             bp_block_name = "block",
#'                                             bp_item_nums_name = "item_num",
#'                                             bp_trait_name = "traits",
#'                                             bp_sign_name = "signs",
#'                                             ### These parameters are column names in item_info
#'                                             df_item_nums_name = "ID",
#'                                             df_trait_name = "Factor",
#'                                             df_sign_name = "Keying",
#'                                             ### These parameters will be used when you have extra matching criteria
#'                                             df_matching_criterion_name = "SD_rating",
#'                                             bp_matching_criterion_name = "SD_matching",
#'                                             ## Which function is used to calculate matching?
#'                                             df_matching_function = "range_m",
#'                                             df_matching_adjust_factor = 1.25,
#'                                             max_attempts_in_comb = 100,
#'                                             max_attempts_in_adjust = 20)
#'
#' @export
#' 
#' 


build_scale_with_blueprint <- function(item_df, blueprint,
                                       bp_block_name, bp_item_nums_name, 
                                       bp_trait_name, bp_sign_name, bp_matching_criterion_name,
                                       df_item_nums_name, df_trait_name, df_sign_name,
                                       df_matching_criterion_name, df_matching_function,
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
    if (!missing(bp_matching_criterion_name)) {
      current_block_criterion <- current_block_in_bp[, bp_matching_criterion_name]
    }
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
        ## If df_matching_criterion_name is not given, then just treat it is succeed.
        if (missing(bp_matching_criterion_name)) {
          matched_blocks <- temp_df
          success_comb <- c(all_possible_combs[selected_comb, ])
          break
        }
        ## Succeed in meeting the criteria?
        else if (eval(call(df_matching_function, temp_df[, df_matching_criterion_name, drop = TRUE]))
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
        if (missing(bp_matching_criterion_name)) {
          matched_blocks$N_adjusts <- "Not Applicable"
          matched_blocks$final_criteria <- "Not specified"
        }
        else {
          matched_blocks$N_adjusts <- N_attempts_in_adjust
          matched_blocks$final_criteria <- current_block_criterion[1]
        }
        
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
                         " attempts in relaxing the matching criteria. Consider increasing max_attempts_in_adjust and/or max_attempts_in_comb, or adjust your blueprint."))
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