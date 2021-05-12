# set.seed(2021)
# N_items <- 90
# N_target <- 90
# 
# ### Simulate some item metrics.
# SD_rating <- rnorm(N_items, mean = 5.5, sd = 1.0)
# location <- runif(N_items, min = -4, max = 4)
# discr <- runif(N_items, min = 0.2, max = 2.0)
# dimensions <- sample(rep(seq(1:5),18))
# 
# item_df <- cbind(SD = SD_rating, loc = location, dim = dimensions, disc = discr)
# 
# rb <- make_random_block(90, 4, item_per_block = 2)
# 
# cal_block_energy(rb, item_df, weights = c(-1,-1,3,-1), fac = 3)

### Deprecated. Calculating energy for every pair of items is space demanding when we want >2 items per block.
### Hence there is even no need for energy matrix.


#' Building a random block as the initial solution for SA algorithm.
#' 
#' @description \code {make_random_block()} Given a set of numbers, randomly pick some (or all) or them
#' and build blocks each with `item_per_block` items.
#' If `target_items` is not a multiple of `item_per_block`, random duplicate items in the set of used items are filled.
#' 
#' @param total_items Number of items to choose from
#' @param target_items Number of items used for building item blocks. Default as equal to `total_items`.
#' @param item_per_block Number of items for each block.
#'
#' @return A list of paired item blocks (list) each with `item_per_block` integers.
#' @export
#'
#' @examples
#' {
#'    make_random_block(total_items = 90, target_items = 45, item_per_block = 3)
#' }
#' 
make_random_block <- function(total_items, target_items = total_items, item_per_block) {
  item_indices <- sample(seq(1:total_items), target_items)
  item_blocks <- list()
  if (target_items %% item_per_block == 0) {
     i <- 1
     while (i < target_items) {
       item_blocks <- c(list(item_indices[i:(i+item_per_block-1)]), item_blocks)
       i <- i + item_per_block
     }
  }
  else {
    item_indices <- c(item_indices, item_indices[1:(item_per_block - (target_items %% item_per_block))])
    i <- 1
    while (i < target_items) {
      item_blocks <- c(list(item_indices[i:(i+item_per_block-1)]), item_blocks)
      i <- i + item_per_block
    }
  }
  return(item_blocks)
  
}



#' Given an item block, calculate its relative energy/benefit.
#' 
#' @description \code Invoked within `sa_pairing_generalized` for calculating the energy (engergy). Can also be used
#' separately for calculating the energy of a certain paired block, given item characteristics.
#' 
#' @param block A list of item blocks (list) produced by `make_random_block` or when running the SA algorithm.
#' @param item_chars Item characteristics, where each row represents an item. Should contain one column for item dimensionality.
#' @param weights Weights assigned to each item characteristic (column).
#' @param fac Which of the columns denotes item dimensionality?
#' @param FUN Function for calculating item energy within a block of items. Default to var().
#'
#' @return The total energy/benefit of the whole block.
#' @examples
#' {
#'    make_random_block(total_items = 90, target_items = 45, item_per_block = 3)
#' }
#' 
cal_block_energy <- function(block, item_chars, weights, fac, FUN = var) {
    indices <- seq(1:ncol(item_chars))
    indices <- indices[!indices %in% fac]
    energy <- 0
    for (b in block) {
       for (i in indices) {
          energy <- energy + weights[i] * FUN(item_chars[unlist(b), i])
       }
       item_dims <- item_chars[unlist(b), fac]
       energy <- energy + weights[fac] * ifelse(length(item_dims) == length(unique(item_dims)), 1, 0)
    }

    return(energy)
}


cal_block_energy_with_irr <- function(block, item_chars, weights, fac, FUN = var, rater_chars, 
                                      irr_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1),
                                      verbose = FALSE) {
  indices <- seq(1:ncol(item_chars))
  indices <- indices[!indices %in% fac]
  energy <- 0
  for (b in block) {
    for (i in indices) {
      energy <- energy + weights[i] * FUN(item_chars[unlist(b), i])
    }
    item_dims <- item_chars[unlist(b), fac]
    energy <- energy + weights[fac] * ifelse(length(item_dims) == length(unique(item_dims)), 1, 0)
    selected_item <- rater_chars[,b]
    BPlin <- bp.coeff.raw(selected_item, weights = "linear")$est[,4]
    BPquad <- bp.coeff.raw(selected_item, weights = "quadratic")$est[,4]
    AClin <- gwet.ac1.raw(selected_item, weights = "linear")$est[,4]
    ACquad <- gwet.ac1.raw(selected_item, weights = "quadratic")$est[,4]
    irr_energy <- irr_weights %*% c(BPlin, BPquad, AClin, ACquad)
    if (verbose == TRUE) {
      print(sprintf("BPlin: %f, BPquad: %f, AClin: %f, ACquad: %f", BPlin, BPquad, AClin, ACquad))
    }
    energy <- energy + irr_energy
  
  }

  
  return(energy)
}

## TODO Allow lower energy to be considered desirable.
sa_pairing_generalized <- function(block, total_items, Temperature, r = 0.999, 
                       item_chars, weights, fac, FUN = var) {
   # TODO implement the pairing function
   # Pick two blocks and randomly exchange their items (If all items are used)
   # Pick an unused item and replace one of the used items or exchange items (If not all items are used)
   # Change of energy is the energy of two new blocks minus the energy of two old blocks 
   block0 <- block
   energy0 <- cal_block_energy(block, item_chars, weights, fac, FUN)
   energy <- energy0

   if (missing(Temperature)) {
     Temperature <- 10 * abs(energy0)
   }
   T0 <- Temperature
   # How do we know that all items are used?
   all_item_used <- length(unique(unlist(block))) == total_items
   
   if (all_item_used) {
     while (Temperature > 10^(-6) * T0) {
        sample_index <- sample(1:length(block),2)
        sample_block <- block[sample_index]
        sample_energy <- cal_block_energy(sample_block, item_chars, weights, fac, FUN)
      
        sample_items <- unlist(sample_block)
        l <- length(sample_items)
      
        exchanged_items <- sample(sample_items,length(sample_items))
        exchanged_block <- c(list(exchanged_items[1:(l/2)]),
                           list(exchanged_items[(l/2+1):l]))
      
        exchanged_energy <- cal_block_energy(exchanged_block, item_chars, weights, fac, FUN)
      
        if (exchanged_energy >= sample_energy) {
          block[sample_index[1]] <- exchanged_block[1]
          block[sample_index[2]] <- exchanged_block[2]
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          block[sample_index[1]] <- exchanged_block[1]
          block[sample_index[2]] <- exchanged_block[2]
          energy <- energy + exchanged_energy - sample_energy
        }
        else {
          ### No need to do anything.
        }
        Temperature <- Temperature * r
     }
   }
   else {
     eta <- length(unique(unlist(block))) / total_items
     Temperature <- Temperature * eta
     while (Temperature > 10^(-6) * T0) {
       if (-1*eta^2+eta > runif(1)) {
         ### Pick a new item
         unused_items <- setdiff(seq(1:total_items), unlist(block))
         sample_index <- sample(1:length(block),1)
         sample_block <- block[sample_index]
         sample_energy <- cal_block_energy(sample_block, item_chars, weights, fac, FUN)
         
         picked_item <- sample(unused_items, 1)
         exchanged_block <- unlist(sample_block)
         exchanged_block[1] <- picked_item
         exchanged_block <- list(exchanged_block)
         exchanged_energy <- cal_block_energy(exchanged_block, item_chars, weights, fac, FUN)
         
         if (exchanged_energy >= sample_energy) {
           block[sample_index[1]] <- exchanged_block
           energy <- energy + exchanged_energy - sample_energy
         }
         else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
           block[sample_index[1]] <- exchanged_block
           energy <- energy + exchanged_energy - sample_energy
         }
         else {
           ### No need to do anything.  
         }         
         Temperature <- Temperature * r
       }
       else {
         ### Change items for two blocks
         sample_index <- sample(1:length(block),2)
         sample_block <- block[sample_index]
         sample_energy <- cal_block_energy(sample_block, item_chars, weights, fac, FUN)
         
         sample_items <- unlist(sample_block)
         l <- length(sample_items)
         
         exchanged_items <- sample(sample_items,length(sample_items))
         exchanged_block <- c(list(exchanged_items[1:(l/2)]),
                              list(exchanged_items[(l/2+1):l]))
         
         exchanged_energy <- cal_block_energy(exchanged_block, item_chars, weights, fac, FUN)
         
         if (exchanged_energy >= sample_energy) {
           block[sample_index[1]] <- exchanged_block[1]
           block[sample_index[2]] <- exchanged_block[2]
           energy <- energy + exchanged_energy - sample_energy
         }
         else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
           block[sample_index[1]] <- exchanged_block[1]
           block[sample_index[2]] <- exchanged_block[2]
           energy <- energy + exchanged_energy - sample_energy
         }
         else {
           ### No need to do anything.  
         }
         Temperature <- Temperature * r
       }
     }
   }
   return(list(block_initial = block0, energy_initial = energy0, block_final = block, energy_final = energy))
}

sa_pairing_generalized_with_irr <- function(block, total_items, Temperature, r = 0.999, 
                                   item_chars, weights, fac, FUN = var,
                                   rater_chars, irr_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1)) {
  # TODO implement the pairing function
  # Pick two blocks and randomly exchange their items (If all items are used)
  # Pick an unused item and replace one of the used items or exchange items (If not all items are used)
  # Change of energy is the energy of two new blocks minus the energy of two old blocks 
  block0 <- block
  energy0 <- cal_block_energy_with_irr(block, item_chars, weights, fac, FUN, 
                                       rater_chars, irr_weights)
  energy <- energy0
  
  if (missing(Temperature)) {
    Temperature <- 10 * abs(energy0)
  }
  T0 <- Temperature
  # How do we know that all items are used?
  all_item_used <- length(unique(unlist(block))) == total_items
  
  if (all_item_used) {
    while (Temperature > 10^(-6) * T0) {
      sample_index <- sample(1:length(block),2)
      sample_block <- block[sample_index]
      sample_energy <- cal_block_energy_with_irr(sample_block, item_chars, weights, fac, FUN, 
                                                 rater_chars, irr_weights)
      
      sample_items <- unlist(sample_block)
      l <- length(sample_items)
      
      exchanged_items <- sample(sample_items,length(sample_items))
      exchanged_block <- c(list(exchanged_items[1:(l/2)]),
                           list(exchanged_items[(l/2+1):l]))
      
      exchanged_energy <- cal_block_energy_with_irr(exchanged_block, item_chars, weights, fac, FUN, 
                                                    rater_chars, irr_weights)
      
      if (exchanged_energy >= sample_energy) {
        block[sample_index[1]] <- exchanged_block[1]
        block[sample_index[2]] <- exchanged_block[2]
        energy <- energy + exchanged_energy - sample_energy
      }
      else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
        block[sample_index[1]] <- exchanged_block[1]
        block[sample_index[2]] <- exchanged_block[2]
        energy <- energy + exchanged_energy - sample_energy
      }
      else {
        ### No need to do anything.
      }
      Temperature <- Temperature * r
    }
  }
  else {
    print("It's here")
    eta <- length(unique(unlist(block))) / total_items
    Temperature <- Temperature * eta
    while (Temperature > 10^(-6) * T0) {
      if (-1*eta^2+eta > runif(1)) {
        ### Pick a new item
        unused_items <- setdiff(seq(1:total_items), unlist(block))
        sample_index <- sample(1:length(block),1)
        sample_block <- block[sample_index]
        sample_energy <- cal_block_energy_with_irr(sample_block, item_chars, weights, fac, FUN, 
                                                   rater_chars, irr_weights)
        picked_item <- sample(unused_items, 1)
        exchanged_block <- unlist(sample_block)
        exchanged_block[1] <- picked_item
        exchanged_block <- list(exchanged_block)
        exchanged_energy <- cal_block_energy_with_irr(exchanged_block, item_chars, weights, fac, FUN, 
                                                      rater_chars, irr_weights)
        
        if (exchanged_energy >= sample_energy) {
          block[sample_index[1]] <- exchanged_block
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          block[sample_index[1]] <- exchanged_block
          energy <- energy + exchanged_energy - sample_energy
        }
        else {
          ### No need to do anything.  
        }         
        Temperature <- Temperature * r
      }
      else {
        ### Change items for two blocks
        sample_index <- sample(1:length(block),2)
        sample_block <- block[sample_index]
        sample_energy <- cal_block_energy_with_irr(sample_block, item_chars, weights, fac, FUN, 
                                                   rater_chars, irr_weights)
        
        sample_items <- unlist(sample_block)
        l <- length(sample_items)
        
        exchanged_items <- sample(sample_items,length(sample_items))
        exchanged_block <- c(list(exchanged_items[1:(l/2)]),
                             list(exchanged_items[(l/2+1):l]))
        
        exchanged_energy <- cal_block_energy_with_irr(exchanged_block, item_chars, weights, fac, FUN, 
                                                      rater_chars, irr_weights)
        
        if (exchanged_energy >= sample_energy) {
          block[sample_index[1]] <- exchanged_block[1]
          block[sample_index[2]] <- exchanged_block[2]
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          block[sample_index[1]] <- exchanged_block[1]
          block[sample_index[2]] <- exchanged_block[2]
          energy <- energy + exchanged_energy - sample_energy
        }
        else {
          ### No need to do anything.  
        }
        Temperature <- Temperature * r
      }
    }
  }
  return(list(block_initial = block0, energy_initial = energy0, block_final = block, energy_final = energy))
}




# t <- sa_pairing_generalized(rb, 4, item_chars = item_df, weights = c(-1, -1, 10, -1), fac = 3)
# 
# 
# t$block_final
# t$energy_final
# t$energy_initial
# for (block in t$block_final) {
#    print(item_df[unlist(block),3])
# }

