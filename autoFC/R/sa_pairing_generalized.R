sa_pairing_generalized <- function(block, total_items, Temperature, eta_Temperature = 0.01,
                                   r = 0.999, end_criteria = 10^(-6),
                                   item_chars, weights, FUN, n_exchange = 2, prob_newitem = 0.25,
                                   use_IIA = FALSE, rater_chars,
                                   iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1)) {
  if (missing(block)) {
    block <- make_random_block(nrow(item_chars), item_per_block = 2)
  }
  if (missing(FUN)) {
    types <- sapply(item_chars, class)
    types <- replace(types, types == "factor" | types == "character", "facfun")
    types <- replace(types, types == "numeric", "var")
    FUN <- types
  }
  if (missing(weights)) {
    weights <- rep(1, ncol(item_chars))
  }
  if (missing(total_items)) {
    total_items <- length(unique(block))
  }
  if (r >= 1 | r < 0) {
    stop("Invalid value for r: Should be a value between 0 and 1.")
  }
  if (end_criteria >= 1 | end_criteria < 0) {
    stop("Invalid value for end_criteria: Should be a value between 0 and 1.")
  }
  if (use_IIA & missing(rater_chars)) {
    stop("Item responses required if use_IIA = TRUE.")
  }
  if (prob_newitem > 1 | prob_newitem < 0) {
    stop("Invalid value for prob_newitem: Should be a value between 0 and 1.")
  }
  if (eta_Temperature < 0){
    stop("Invalid value for eta_Temperature: Should be a value larger than 0.")
  }
  if (!(n_exchange %% 1 == 0) | n_exchange > nrow(block) | n_exchange < 2) {
    stop("Invalid value for n_exchange: Should be an integer between 2 and nrow(block)")
  }


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
      # 1. We randomly pick several blocks then calculate the total energy for these blocks first.
      sample_index <- sample(1:nrow(block), n_exchange)
      sample_block <- block[sample_index,]
      sample_energy <- ifelse(!use_IIA, cal_block_energy(sample_block, item_chars, weights, FUN),
                              cal_block_energy_with_iia(sample_block, item_chars, weights, FUN, rater_chars, iia_weights))

      l <- length(sample_block)

      # 2, Then we randomly shuffle items in these blocks and calculate the energy again.
      exchanged_items <- sample(sample_block,l)
      exchanged_block <- matrix(exchanged_items, nrow = n_exchange)
      exchanged_energy <- ifelse(!use_IIA, cal_block_energy(exchanged_block, item_chars, weights, FUN),
                                 cal_block_energy_with_iia(exchanged_block, item_chars, weights, FUN, rater_chars, iia_weights))


      # 3. If the new energy is higher, then we replace these blocks with shuffled ones, and update the energy.
      if (exchanged_energy >= sample_energy) {
        for (i in seq(1:n_exchange)) {
           block[sample_index[i],] <- exchanged_block[i,]
        }
        # print("Accept better solutions")
        energy <- energy + exchanged_energy - sample_energy
      }
      # 4. Else, we accept the inferior solution with a (Potentially small) probability.
      else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
        # print("Accept Conditionally")
        for (i in seq(1:n_exchange)) {
          block[sample_index[i],] <- exchanged_block[i,]
        }
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
    while (Temperature > end_criteria * T0) {
      # 2. Choice of which method to use is determined by that proportion.
      if (prob_newitem > runif(1)) {
        # 2.1 Pick an unused item and a block. Calculate the energy for this block.
        unused_items <- setdiff(seq(1:total_items), block)
        sample_index <- sample(nrow(block),1)
        sample_block <- block[sample_index,]
        sample_energy <- ifelse(!use_IIA, cal_block_energy(sample_block, item_chars, weights, FUN),
                                cal_block_energy_with_iia(sample_block, item_chars, weights, FUN, rater_chars, iia_weights))

        # 2.2 Replace the first item in this block with this unused item. Calculate the energy for the block with item replaced.
        picked_item <- sample(unused_items, 1)
        exchanged_block <- sample_block
        exchanged_block[1] <- picked_item
        exchanged_energy <- ifelse(!use_IIA, cal_block_energy(exchanged_block, item_chars, weights, FUN),
                                   cal_block_energy_with_iia(exchanged_block, item_chars, weights, FUN, rater_chars, iia_weights))

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
        # 2.4 Or we pick several blocks and exchange their items, like what is done when all items are used.
        sample_index <- sample(1:nrow(block), n_exchange)
        sample_block <- block[sample_index,]
        sample_energy <- ifelse(!use_IIA, cal_block_energy(sample_block, item_chars, weights, FUN),
                                cal_block_energy_with_iia(sample_block, item_chars, weights, FUN, rater_chars, iia_weights))

        l <- length(sample_block)

        exchanged_items <- sample(sample_block,l)
        exchanged_block <- matrix(exchanged_items, nrow = n_exchange)

        exchanged_energy <- ifelse(!use_IIA, cal_block_energy(exchanged_block, item_chars, weights, FUN),
                                   cal_block_energy_with_iia(exchanged_block, item_chars, weights, FUN, rater_chars, iia_weights))

        if (exchanged_energy >= sample_energy) {
          for (i in seq(1:n_exchange)) {
            block[sample_index[i],] <- exchanged_block[i,]
          }
          energy <- energy + exchanged_energy - sample_energy
        }
        else if (exp((exchanged_energy - sample_energy)/Temperature) > runif(1)) {
          for (i in seq(1:n_exchange)) {
            block[sample_index[i],] <- exchanged_block[i,]
          }
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
