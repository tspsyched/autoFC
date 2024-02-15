# autoFC

A collection of tools to automatically pair forced-choice items and examine their measurement performance

## Overview

Forced-choice (**FC**) tests are gaining researcher's interest increasingly for its faking resistance when well-designed. Well-designed FC tests should often be characterized by *items within a block measuring different latent traits*, and *items within a block having similar magnitude, or high inter-item agreement **(IIA)** in terms of their social desirability*. Other scoring models may also require *factor loading differences or item locations within a block to be maximized or minimized*.

Either way, decision on which items should be assigned to the same block - item pairing - is a crucial issue in building a well-designed FC test, which is currently carried out manually. However, given that we often need to simultaneously meet multiple objectives, manual pairing will turn out to be impractical and even infeasible, especially when the number of latent traits and/or the number of items per trait become relatively large.

The R package **autoFC** is developed to address these difficulties and provides a tool for facilitating automatic FC test construction as well as evaluating measurement performance using simulation data. It offers users the functionality to:

1.  Include multiple criteria for pairing items into the same block, with user-specified weights and calculating functions for each criterion.

2.  Automatically optimize the target function combined from the multiple criteria and produce near-optimal item pairings that satisfy the user-defined criteria.

3.  Specify blueprints for the FC blocks (i.e., exact specification on how the block should be, for example, in terms of measured traits and keying) and build FC blocks that are aligned with the setups in the blueprints.

4.  Produce simulated responses to FC scales, based on the Thurstonian IRT model (Brown & Maydeu-Olivares, 2011), and estimate the Thurstonian IRT model using the simulated responses.

5.  Examine the empirical reliability and measurement precision of the resulting trait scores produced from the estimation model.

Users are allowed to create an FC test of any block size (e.g. Pairs, Triplets, Quadruplets) and they can produce simulated responses to FC scales in both MOLE (Most & Least like me) and RANK formats.

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

Below is a brief explanation of all functions provided by the initial version of **autoFC**.

1.  `cal_block_energy()` and `cal_block_energy_with_iia()` both calculate the total energy for a single item block, or a full FC test with multiple blocks, given a data frame of item characteristics. The latter function incorporates IIA metrics into energy calculation.

-   By default, numeric item characteristics are paired by minimizing variance within each block, and factor item characteristics are paired such that it's more preferable for items in the same block to be from different factor levels. Each characteristic has a default weight of 1.
-   In addition, `cal_block_energy_with_iia()` incorporates four IIA metrics in which items are paired by maximizing the IIA within each block. Each IIA has a default weight of 1.

2.  `make_random_block()` takes in number of items and block size as input arguments and produces a test with blocks of randomly paired item numbers. Information about item characteristics is not required.

3.  `get_iia()` takes in item responses and a single item block (Or a full FC test with multiple blocks), then returns IIA metrics for each item block.

4.  `sa_pairing_generalized()` is the automatic pairing function which takes in item characteristics (and also individual responses for all items) and an initial FC test, then optimizes the energy of the test based on Simulated Annealing (SA) algorithm.

-   SA is a probabilistic technique for approximating the global optimum of a given function, in which each iteration involves the cool down of the "Temperature" until it reaches a certain value. Within each iteration, a new solution (**FC test**) is produced and compared with current solution in terms of their energy (Which is calculated by calling `cal_block_energy()` or `cal_block_energy_with_iia()`. Acceptance or rejection of new solution is determined as follows:

    -   Better solution (An FC test with higher energy) is always accepted and updated into the new current solution.
    -   Worse solution (An FC test with lower energy) is conditionally accepted, determined by the current temperature and the deviation of new solution's energy from the current one's. A worse solution is more likely to be accepted when temperature is high and when the deviance is relatively small.

-   If all items in the item characteristic data frame are used to construct the FC test, `sa_pairing_generalized()` will produce new solutions by randomly exchanging items between two blocks; Otherwise, it will randomly select between exchanging items and replacing with unused items based on proportion of items used to construct the test.

In the Feb, 2024 update, we added a lot more functions, including the following core ones:

1.  `construct_blueprint()` builds up exact specifications of the FC blocks (i.e., blueprints), which typically indicates the keying and measured traits of each item for each block. An additional matching criteria can also be set, indicating how well should the items be matched based on certain indicators using a pre-specified cutoff.

2.  `build_scale_with_blueprint()` takes in the blueprint that user built manually or through `construct_blueprint()` and automatically produces the paired FC blocks consistent with the specifications in the blueprint.

-   This is an addition to the automatic item pairing module as was already there back in the earlier version of autoFC. Typically when users want to construct FC scales, they would wish at least some blocks to be exactly in certain designs. This function exactly serves as that purpose.

3.  `get_simulation_matrices()` produces simulated item and person parameters based on the Thurstonian IRT model, using the factor analysis results extracted from `lavaan::cfa()` or `get_CFA_estimates()`.

4.  `convert_to_TIRT_response()`, `get_TIRT_long_data()`, `fit_TIRT_model()` are extensions to the various functions in the *ThurstonianIRT* package (Bürkner, 2019) which allow the simulated (using `convert_to_TIRT_response()`) or actual responses to FC scales to be processed and converted into long format (using `get_TIRT_long_data()`) and fitted using lavaan, Mplus or stan methods (using `fit_TIRT_model()`).

5.  `RMSE_range()`, `plot_scores()`, and `empirical_reliability()` for diagnostic purposes, examining the measurement accuracy of the trait scores produced from the TIRT model.

Detailed descriptions of all functions and other functions that are not listed here can be found in the manual and the help document of each function.

We also recently published a paper (Li et al., 2024) discussing the issues related to the development of forced-choice scales, which also includes a detailed tutorial on how to construct FC scales using these latest functionalities of the *autoFC* package. Users are also encouraged to refer to this paper for further details.


## References
Brown, A., & Maydeu-Olivares, A. (2011). Item response modeling of forced-choice questionnaires. *Educational and Psychological Measurement, 71*(3), 460-502. https://doi.org/10.1177/0013164410375112

Bürkner, P. C. (2019). thurstonianIRT: Thurstonian IRT models in R. *Journal of Open Source Software, 4*(42), 1662. https://doi.org/10.21105/joss.01662

Li, M., Zhang, B., Li, L., Sun, T., & Brown, A., (2024). Mixed-Keying or Desirability-Matching in the Construction of Forced-Choice Measures? An Empirical Investigation and Practical Recommendations. *Organizational Research Methods*. https://doi.org/10.1177/10944281241229784

