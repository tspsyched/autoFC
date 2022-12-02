# autoFC
A collection of tools to automatically pair forced-choice items and examine their measurement performance

## Overview

Forced-choice (**FC**) tests are gaining researcher's interest increasingly for its faking resistance when well-designed. Well-designed FC tests should often be characterized by _items within a block measuring different latent traits_, and _items within a block having similar magnitude, or high inter-item agreement **(IIA)** in terms of their social desirability_. Other scoring models may also require _factor loading differences or item locations within a block to be maximized or minimized_.

Either way, decision on which items should be assigned to the same block - item pairing - is a crucial issue in building a well-designed FC test, which is currently carried out manually. However, given that we often need to simultaneously meet multiple objectives, manual pairing will turn out to be impractical and even infeasible, especially when the number of latent traits and/or the number of items per trait become relatively large.

The R package __autoFC__ is developed to address these difficulties and provides a tool for facilitating automatic FC test construction. It offers users the functionality to:

1. Customize one or more item pairing criteria and calculate a composite pairing index, termed __"energy"__ with user-specified weights for each criterion.

2. Automatically optimize the energy for the whole test by sequentially or simultaneously optimizing each matching rule, through the exchange of items among blocks or replacement with unused items.

3. Construct parallel forms of the same test following the same pairing rules.

Users are allowed to create an FC test of any block size (e.g. Pairs, Triplets, Quadruplets).

## Installation

You can install autoFC from CRAN:

``` r
install.packages("autoFC")
```

You can install the development version of autoFC from GitHub:

``` r
devtools::install_github("tspsyched/autoFC")
```

## Functions

Below is a brief explanation of all functions provided by __autoFC__. Details and usage can be found in the next section.

1. `cal_block_energy()` and `cal_block_energy_with_iia()` both calculate the total energy for a single item block, or a full FC test with multiple blocks, given a data frame of item characteristics. The latter function incorporates IIA metrics into energy calculation.

  * By default, numeric item characteristics are paired by minimizing variance within each block, and factor item characteristics are paired such that it's more preferable for items in the same block to be from different factor levels. Each characteristic has a default weight of 1.
  * In addition, `cal_block_energy_with_iia()` incorporates four IIA metrics in which items are paired by maximizing the IIA within each block. Each IIA has a default weight of 1.


2. `make_random_block()` takes in number of items and block size as input arguments and produces a test with blocks of randomly paired item numbers. Information about item characteristics is not required.

3. `get_iia()` takes in item responses and a single item block (Or a full FC test with multiple blocks), then returns IIA metrics for each item block.

4. `sa_pairing_generalized()` is the automatic pairing function which takes in item characteristics (and also individual responses for all items) and an initial FC test, then optimizes the energy of the test based on Simulated Annealing (SA) algorithm.

  * SA is a probabilistic technique for approximating the global optimum of a given function, in which each iteration involves the cool down of the "Temperature" until it reaches a certain value. Within each iteration, a new solution (**FC test**) is produced and compared with current solution in terms of their energy (Which is calculated by calling `cal_block_energy()` or `cal_block_energy_with_iia()`. Acceptance or rejection of new solution is determined as follows:
  
    - Better solution (An FC test with higher energy) is always accepted and updated into the new current solution.
    - Worse solution (An FC test with lower energy) is conditionally accepted, determined by the current temperature and the deviation of new solution's energy from the current one's. A worse solution is more likely to be accepted when temperature is high and when the deviance is relatively small.
    
    
  * If all items in the item characteristic data frame are used to construct the FC test, `sa_pairing_generalized()` will produce new solutions by randomly exchanging items between two blocks; Otherwise, it will randomly select between exchanging items and replacing with unused items based on proportion of items used to construct the test.
  
  * `sa_pairing_generalized()` has built-in default values for most of the arguments if they are not given; For example, an FC test with block size 2 using all of the given items will be constructed by default if the `block` argument is not provided. See the tutorial below for meanings and default values for other arguments.

## Citation

Li, M., Sun, T., & Zhang, B. (2022). autoFC: An R Package for Automatic Item Pairing in Forced-Choice Test Construction. _Applied Psychological Measurement, 46_(1), 70–72. https://doi.org/10.1177/01466216211051726

