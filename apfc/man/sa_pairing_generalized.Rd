\name{sa_pairing_generalized}
\alias{sa_pairing_generalized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Automatic Item Pairing Method in Forced-Choice Test Construction
}
\description{
Automatic construction of forced-choice tests based on Simulated Annealing algorithm. Allows items to be:

  1. Matched in either pairs, triplets, quadruplets or blocks of any size;

  2. Matched based on any number of item-level characteristics (e.g. Social desirability, factor) based on any customized criteria;

  3. Matched based on person-level inter-item agreement (IIA) metrics.
}
\usage{
sa_pairing_generalized(block, total_items, Temperature, eta_Temperature = 0.01,
                       r = 0.999, end_criteria = 10^(-6),
                       item_chars, weights, FUN,
                       use_IIA = FALSE, rater_chars,
                       iia_weights = c(BPlin = 1, BPquad = 1,
                                       AClin = 1, ACquad = 1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{block}{
      An \emph{n} by \emph{k} integer matrix, where \emph{n} is the number of item blocks and \emph{k} is the number of items per block.

      Serves as the initial starting blocks for the automatic pairing method.
  }
  \item{total_items}{
      Interger value. How many items do we sample from in order to build this \code{block}?
  }
  \item{Temperature}{
      Initial temperature value. Recommended to be computed based on the absolute value of initial energy of \code{block}, rather than set arbitrary values.

      In general, higher temperature represents a higher probability of accepting an inferior solution.
  }
  \item{eta_Temperature}{
      A positive numeric value. The ratio of initial temperature to initial energy of \code{block}, if \code{Temperature} is not designated.
  }
  \item{r}{
      A positive numeric value less than 1. Determines the reduction rate of \code{Temperature} after each iteration.
  }
  \item{end_criteria}{
      A positive numeric value less than 1. Iteration breaks when temperature drops to below \code{end_criteria * Temperature}. Default to be 10^(-6).
  }
  \item{item_chars}{
      An \emph{m} by \emph{r} data frame, where \emph{m} is the total number of items to sample from, whether it is included in the block or not, whereas \emph{r} is the number of item characteristics.
  }
  \item{weights}{
      A vector of length \emph{r} with weights for each item characteristics in \code{item_chars}.

      Should provide a weight of 0 for specific characteristics not of interest, such as item ID.
  }
  \item{FUN}{
      A vector of customized function names for optimizing each item characteristic within each block, with length \emph{r}.
  }
  \item{use_IIA}{
      Logical. Are IIA metrics used when performing automatic pairing?
  }
  \item{rater_chars}{
      A \emph{p} by \emph{m} numeric matrix with scores of each of the \emph{p} participants for the \emph{m} items. Ignored when \code{use_IIA == FALSE}.
  }
  \item{iia_weights}{
    A vector of length 4 indicating weights given to each IIA metric:

    Linearly weighted AC (Gwet, 2008; 2014); Quadratic weighted AC; Linearly weighted BP (Brennan-Prediger) index (Brennan & Prediger, 1981; Gwet, 2014); Quadratic weighted BP.

    Ignored when \code{use_IIA == FALSE}.
  }

}

\value{
  \item{block_initial}{
          Initial starting block
  }
  \item{energy_initial}{
          Initial energy for \code{block_initial}
  }
  \item{block_final}{
          Final paired block after optimization by SA
  }
  \item{energy_final}{
          Final energy for \code{block_final}
  }
}

\author{
Mengtong Li
}
\note{
    The essence of SA is the probablistic acceptance of solutions inferior to the current state, which avoids getting stuck in local maxima/minima.

    It is also recommended to try out different values of \code{weights, iia_weights, eta_Temperature} to find out the best combination of initial temperature and energy value in order to provide optimally paired blocks.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{

## Simulate 60 items loading on different Big Five dimensions,
## with different mean and item difficulty

item_dims <- sample(c("Openness","Conscientiousness","Neuroticism",
                      "Extraversion","Agreeableness"), 60, replace = TRUE)
item_mean <- rnorm(60, 5, 2)
item_difficulty <- runif(60, -1, 1)

## Construct data frame for item characteristics and produce 20
## random triplet blocks with these 60 items

item_df <- data.frame(Dimensions = item_dims, Mean = item_mean, Difficulty = item_difficulty)
solution <- make_random_block(60, 60, 3)

## A function for determining whether items in a block are from different dimensions

facfun <- function(vec){
  if (typeof(vec) == "list") {vec <- unlist(vec)}
  return(ifelse(length(vec) == length(unique(vec)), 1, 0))
}

## Simple simulation of responses from 600 participants on the 60 items.
## In practice, should use real world data or simluation based on IRT parameters.

item_responses <- matrix(sample(seq(1:5), 600*60, replace = TRUE), nrow = 60)

## Automatic pairing, without use of IIAs
sa_pairing_generalized(solution, 60, eta_Temperature = 0.01,
                                   r = 0.999, end_criteria = 0.001, weights = c(1,1,1),
                                   item_chars = item_df, FUN = c("facfun", "var", "var"))

## Automatic pairing, with IIAs
sa_pairing_generalized(solution, 60, eta_Temperature = 0.01,
                                   r = 0.999, end_criteria = 0.001, weights = c(1,1,1),
                                   item_chars = item_df, FUN = c("facfun", "var", "var"),
                                   use_IIA = TRUE, rater_chars = item_responses,
                                   iia_weights = c(BPlin = 1, BPquad = 1, AClin = 1, ACquad = 1))

}
