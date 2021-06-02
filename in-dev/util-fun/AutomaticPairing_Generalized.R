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




#### Produce a number of randomly sampled blocks of items in a matrix format. Can also accommodate cases when # of items used is not a multiple of the # of items per block.
#### Can be used as initial solution for the automatic pairing functions.
### Input:
## total_items - How many items do we sample from?
## target_items - How many items do we sample to build item blocks?
## item_per_block - How many items will there be in each block?

## Note: If target_items is no a multiple of item_per_block, the item set produced by target_items will be looped until # of sampled items becomes a multiple of item_per_block.

make_random_block <- function(total_items, target_items = total_items, item_per_block) {
  if (target_items > total_items) {
     stop("Number of target items should not be larger than number of total items")
  }
  if (target_items <= 0) {
     stop("Number of target items should be larger than 0")
  }
  item_indices <- sample(seq(1:total_items), target_items)
  if (target_items %% item_per_block == 0) {
    # If it is a multiple, return a matrix of item numbers.
    return(matrix(item_indices, ncol = item_per_block, byrow = TRUE))
  }
  else {
    # Otherwise, append the list of selected items until # of items is a multiple of item_per_block
    item_indices <- c(item_indices, item_indices[1:(item_per_block - (target_items %% item_per_block))])
    return(matrix(item_indices, ncol = item_per_block, byrow = TRUE))
  }
  
}




#### Produce the "Energy" of a set of item blocks, given all the item characteristics of all items, weights for each characteristic, and a customized function to optimize the characteristics within each block.
#### This serves as the core function for determining the acceptance or rejection of a newly built block over the previous one.
### Input:
## block - An nxk integer matrix, where n is the number of item blocks and k is the number of items per block.
## item_chars - An mxr matrix, where m is the total number of items to sample from (Whether it is included in the block or not. Hence we have m >= nxk) and r is the number of item characteristics. These can include:
##   Social desirability score; Item dimensionality (Which factor this item loads on); Factor loading; Item difficulty; Reverse coding information, etc.
## weights - A vector of length r with weights for each item characteristics in item_chars. Should provide a weight of 0 for specific characteristics if the item characteristics in the corresponding positions are not of interest (Such as ID).
## FUN - A list of customized function for optimizing each item characteristic within each block. 

## target_items - How many items do we sample to build item blocks?
## item_per_block - How many items will there be in each block?
cal_block_energy <- function(block, item_chars, weights, FUN) {
  indices <- seq(1:ncol(item_chars))
  energy <- 0
  
  # Apply separate functions for each item characteristic to get an estimate of energy, then sum this up.
  for (row in seq(1:nrow(block))) {
    for (i in indices) {
      fun_i <- as.character(quote(FUN[i]))
      energy <- energy + weights[i] * eval(call(fun_i, item_chars[block[row,], i]))
    }
  }
  return(energy)
}

#### An extension of the cal_block_energy function with consideration of inter item agreement metrics.
#### This serves as the core function for determining the acceptance or rejection of a newly built block over the previous one.
### Input:
## rater_chars - A pxm numeric matrix with scores of each of the p participants for the m items.
## iia_weights - Weights for the four IIAs discussed in Pavlov et el. (Under review)
## verbose - Logical; Do we report IIAs each time we invoke this function?


cal_block_energy_with_iia <- function(block, item_chars, weights, FUN, rater_chars, 
                                      iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1),
                                      verbose = FALSE) {
  indices <- seq(1:ncol(item_chars))
  energy <- 0
  
  # Apply separate functions for each item characteristic to get an estimate of energy, then sum this up.
  for (row in seq(1:nrow(block))) {
    for (i in indices) {
      fun_i <- as.character(quote(FUN[i]))
      energy <- energy + weights[i] * eval(call(fun_i, item_chars[block[row,], i]))
    }
  # Then we add up IIA metrics.
    selected_item <- rater_chars[,block[row,]]
    BPlin <- bp.coeff.raw(selected_item, weights = "linear")$est[,4]
    BPquad <- bp.coeff.raw(selected_item, weights = "quadratic")$est[,4]
    AClin <- gwet.ac1.raw(selected_item, weights = "linear")$est[,4]
    ACquad <- gwet.ac1.raw(selected_item, weights = "quadratic")$est[,4]
    iia_energy <- iia_weights %*% c(BPlin, BPquad, AClin, ACquad)
    if (verbose == TRUE) {
      print(sprintf("BPlin: %f, BPquad: %f, AClin: %f, ACquad: %f", BPlin, BPquad, AClin, ACquad))
    }
    energy <- energy + iia_energy
    
  }
  return(energy)
}



#### Automatic item pairing function based on simulated annealing algorithm, which allows the simlutaneous optimization of any numbers of item characteristics.
#### This serves as the core function for determining the acceptance or rejection of a newly built block over the previous one.
### Input:
## block - An nxk integer matrix, where n is the number of item blocks and k is the number of items per block.
## total_items - How many items do we sample from? If number of unique items in the block matrix is smaller than this number, then randomly choose from picking a new item or swapping items in blocks.
## Temperature - Controls the initial temperature of the algorithm. Can be left blank and determined by the initial energy of the input block, determined by a factor of eta_Temperature.
## r - Cooling factor for the temperature
## end_criteria - The destination temperature proportional to the initial designated Temperature.
## item_chars, weights, fac, FUN - Use ?cal_block_energy for details.


sa_pairing_generalized <- function(block, total_items, Temperature, eta_Temperature = 0.01, r = 0.999, end_criteria = 10^(-6),
                                   item_chars, weights, FUN) {
  
  # Store initial block of items and the corresponding energy.
  block0 <- block
  energy0 <- cal_block_energy(block, item_chars, weights, FUN)
  energy <- energy0
  
  # Provide initial temperature value if it is not given, then store the initial temperature.
  if (missing(Temperature)) {
    Temperature <- eta_Temperature * abs(energy0)
  }
  T0 <- Temperature
  
  
  # How do we know that all items are used?
  all_item_used <- length(unique(block)) == total_items
  
  
  # When we see that all items are used......
  if (all_item_used) {
    while (Temperature > end_criteria * T0) {
      # 1. We randomly pick two blocks then calculate the total energy for these two blocks first.
      sample_index <- sample(1:nrow(block),2)
      sample_block <- block[sample_index,]
      sample_energy <- cal_block_energy(sample_block, item_chars, weights, FUN)
      
      l <- length(sample_block)
      
      # 2, Then we randomly shuffle items in these two blocks and calculate the energy again.
      exchanged_items <- sample(sample_block,l)
      exchanged_block <- matrix(c(exchanged_items[1:(l/2)], exchanged_items[(l/2+1):l]), nrow = 2)
      exchanged_energy <- cal_block_energy(exchanged_block, item_chars, weights, FUN)
      
      
      # 3. If the new energy is higher, then we replace these two blocks with shuffled ones, and update the energy.
      if (exchanged_energy >= sample_energy) {
        # print("Accept better solutions")
        block[sample_index[1],] <- exchanged_block[1,]
        block[sample_index[2],] <- exchanged_block[2,]
        energy <- energy + exchanged_energy - sample_energy
      }
      # 4. Else, we accept the inferior solution with a (Potentially small) probability.
      else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
        # print("Accept Conditionally")
        block[sample_index[1],] <- exchanged_block[1,]
        block[sample_index[2],] <- exchanged_block[2,]
        energy <- energy + exchanged_energy - sample_energy
      }
      else {
        # print("Reject")
        ### No need to do anything.
      }
      # 5. Finally we update the temperature.
      Temperature <- Temperature * r
    }
  }
  # Otherwise...
  else {
    # 1. We first see proportion of items that are unused.
    eta <- length(unique(block)) / total_items
    Temperature <- Temperature * eta
    while (Temperature > end_criteria * T0) {
    # 2. Choice of which method to use is determined by that proportion.
      if (-1*eta^2+eta > runif(1)) {
        # 2.1 Pick an unused item and a block. Calculate the energy for this block.
        unused_items <- setdiff(seq(1:total_items), block)
        sample_index <- sample(nrow(block),1)
        sample_block <- block[sample_index,]
        sample_energy <- cal_block_energy(sample_block, item_chars, weights, FUN)
        
        # 2.2 Replace the first item in this block with this unused item. Calculate the energy for the block with item replaced.
        picked_item <- sample(unused_items, 1)
        exchanged_block <- sample_block
        exchanged_block[1] <- picked_item
        exchanged_energy <- cal_block_energy(exchanged_block, item_chars, weights, FUN)
        
        # 2.3 Determine whether the new block is accepted or rejected as is done above.
        if (exchanged_energy >= sample_energy) {
          block[sample_index[1],] <- exchanged_block
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          block[sample_index[1],] <- exchanged_block
          energy <- energy + exchanged_energy - sample_energy
        }
        else {
          ### No need to do anything.  
        }         
        Temperature <- Temperature * r
      }
      else {
        # 2.4 Or we pick two blocks and exchange their items, like what is done when all items are used.
        sample_index <- sample(1:nrow(block),2)
        sample_block <- block[sample_index,]
        sample_energy <- cal_block_energy(sample_block, item_chars, weights, FUN)
        
        l <- length(sample_block)
        
        exchanged_items <- sample(sample_block,l)
        exchanged_block <- matrix(c(exchanged_items[1:(l/2)], exchanged_items[(l/2+1):l]), nrow = 2)
        
        exchanged_energy <- cal_block_energy(exchanged_block, item_chars, weights, FUN)
        
        if (exchanged_energy >= sample_energy) {
          block[sample_index[1],] <- exchanged_block[1,]
          block[sample_index[2],] <- exchanged_block[2,]
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          block[sample_index[1],] <- exchanged_block[1,]
          block[sample_index[2],] <- exchanged_block[2,]
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



#### Automatic item pairing function based on simulated annealing algorithm, which allows the simlutaneous optimization of any numbers of item characteristics, including inter item agreement (IIA) metrics.
#### This serves as the core function for determining the acceptance or rejection of a newly built block over the previous one.
### Input:
## block - An nxk integer matrix, where n is the number of item blocks and k is the number of items per block.
## total_items - How many items do we sample from? If number of unique items in the block matrix is smaller than this number, then randomly choose from picking a new item or swapping items in blocks.
## Temperature - Controls the initial temperature of the algorithm. Can be left blank and determined by the initial energy of the input block, determined by a factor of eta_Temperature.
## r - Cooling factor for the temperature
## end_criteria - The destination temperature proportional to the initial designated Temperature.
## item_chars, weights, fac, FUN - Use ?cal_block_energy for details.
## rater_chars - A pxm numeric matrix with scores of each of the p participants for the m items.
## iia_weights - Weights for the four IIAs discussed in Pavlov et el. (Under review)



sa_pairing_generalized_with_iia <- function(block, total_items, Temperature, r = 0.999, eta_Temperature = 0.01, end_criteria = 10^(-6),
                                            item_chars, weights, FUN,
                                            rater_chars, iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1)) {

  # Store initial block of items and the corresponding energy.
  block0 <- block
  energy0 <- cal_block_energy(block, item_chars, weights, FUN)
  energy <- energy0
  
  # Provide initial temperature value if it is not given, then store the initial temperature.
  if (missing(Temperature)) {
    Temperature <- eta_Temperature * abs(energy0)
  }
  T0 <- Temperature
  
  # How do we know that all items are used?
  all_item_used <- length(unique(block)) == total_items
  
  # When we see that all items are used......  
  if (all_item_used) {
    while (Temperature > end_criteria * T0) {
      
      # 1. We randomly pick two blocks then calculate the total energy for these two blocks first.
      

      sample_index <- sample(1:nrow(block),2)
      sample_block <- block[sample_index,]
      sample_energy <- cal_block_energy_with_iia(sample_block, item_chars, weights, FUN,
                                                 rater_chars, iia_weights)
      
      l <- length(sample_block)
      
      # 2, Then we randomly shuffle items in these two blocks and calculate the energy again.      
      exchanged_items <- sample(sample_block,l)
      exchanged_block <- matrix(c(exchanged_items[1:(l/2)], exchanged_items[(l/2+1):l]), nrow = 2)
      
      exchanged_energy <- cal_block_energy_with_iia(exchanged_block, item_chars, weights, FUN, 
                                                    rater_chars, iia_weights)
      
      # 3. Now we decide whether we accept the better solution, or sometimes accept an inferior one.
      if (exchanged_energy >= sample_energy) {
        # print("Accept better solutions")
        block[sample_index[1],] <- exchanged_block[1,]
        block[sample_index[2],] <- exchanged_block[2,]
        energy <- energy + exchanged_energy - sample_energy
      }
      else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
        # print("Accept Conditionally")
        block[sample_index[1],] <- exchanged_block[1,]
        block[sample_index[2],] <- exchanged_block[2,]
        energy <- energy + exchanged_energy - sample_energy
      }
      else {
        # print("Fuck off!")
        ### No need to do anything.
      }
      Temperature <- Temperature * r
    }
  }
  # Otherwise...
  else {
    eta <- length(unique(block)) / total_items
    Temperature <- Temperature * eta
    while (Temperature > end_criteria * T0) {
      # Randomly select one of the two strategies based on eta.
      if (-1*eta^2+eta > runif(1)) {
        ### Pick an unused item and a block, then calculate the energy for this block.
        unused_items <- setdiff(seq(1:total_items), block)
        sample_index <- sample(nrow(block),1)
        sample_block <- block[sample_index,]
        sample_energy <- cal_block_energy_with_iia(sample_block, item_chars, weights, FUN,
                                                   rater_chars, iia_weights)
        
        picked_item <- sample(unused_items, 1)
        
        ### Replace the first item in the picked block with the new item, then calculate the energy for the new block.
        exchanged_block <- sample_block
        exchanged_block[1] <- picked_item
        exchanged_energy <- cal_block_energy_with_iia(exchanged_block, item_chars, weights, FUN,
                                                      rater_chars, iia_weights)
        
        ## Then we determine the acceptance or rejection of solution based on the value of new energy.
        if (exchanged_energy >= sample_energy) {
          block[sample_index[1],] <- exchanged_block
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          block[sample_index[1],] <- exchanged_block
          energy <- energy + exchanged_energy - sample_energy
        }
        else {
          ### No need to do anything.  
        }         
        Temperature <- Temperature * r
      }
      else {
        ### Change items for two blocks, which is essentially the same as in the automatic pairing function without IIA.
        sample_index <- sample(1:nrow(block),2)
        sample_block <- block[sample_index,]
        sample_energy <- cal_block_energy(sample_block, item_chars, weights, FUN)
        
        l <- length(sample_block)
        
        exchanged_items <- sample(sample_block,l)
        exchanged_block <- matrix(c(exchanged_items[1:(l/2)], exchanged_items[(l/2+1):l]), nrow = 2)
        
        exchanged_energy <- cal_block_energy_with_iia(exchanged_block, item_chars, weights, FUN, 
                                                      rater_chars, iia_weights)
        
        if (exchanged_energy >= sample_energy) {
          block[sample_index[1],] <- exchanged_block[1,]
          block[sample_index[2],] <- exchanged_block[2,]
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          block[sample_index[1],] <- exchanged_block[1,]
          block[sample_index[2],] <- exchanged_block[2,]
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





#### Other helper functions to help print out informative results for the paired scale.

#### Prints iia metrics for select items
### Inputs:

## block - An nxk integer matrix, where n is the number of item blocks and k is the number of items per block.
## data - A pxm numeric matrix with scores of each of the p participants for the m items.

get_item_consistency <- function(block, data) {
  results <- cbind(BPlin = c(), BPquad = c(), AClin = c(), ACquan = c())
  for (i in seq(1:nrow(block))) {
    selected_item <- data[,block[i,]]
    BPlin <- bp.coeff.raw(selected_item, weights = "linear")$est[,4]
    BPquad <- bp.coeff.raw(selected_item, weights = "quadratic")$est[,4]
    AClin <- gwet.ac1.raw(selected_item, weights = "linear")$est[,4]
    ACquad <- gwet.ac1.raw(selected_item, weights = "quadratic")$est[,4]
    results <- rbind(results, c(BPlin = BPlin, BPquad = BPquad, AClin = AClin, ACquad = ACquad))
  }
  return(results)
}


#### Prints summary statistics (Mean and SD) of IIAs for all paired blocks in a scale.
### Inputs:

## ic - An IIA matrix produced by get_item_consistency().


get_block_iia_summary <- function(ic) {
  mean_summary <- colMeans(ic)
  sd_summary <- apply(ic, 2, sd)
  return(c(Mean1 = mean_summary[1], Mean2 = mean_summary[2],
           Mean3 = mean_summary[3], Mean4 = mean_summary[4],
           SD1 = sd_summary[1], SD2 = sd_summary[2],
           SD3 = sd_summary[3], SD4 = sd_summary[4]))
}






