\name{cal_block_energy}
\alias{cal_block_energy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Calculation of Item Block "Energy"
}
\description{
   Calculates the total "energy" of one or multiple paired item blocks, which is a linear combination of different functions applied to different item characteristics of interest.
}
\usage{
   cal_block_energy(block, item_chars, weights, FUN)
}

\arguments{
  \item{block}{
    An \emph{n} by \emph{k} integer matrix, where \emph{n} is the number of item blocks and \emph{k} is the number of items per block.
  }
  \item{item_chars}{
    An \emph{m} by \emph{r} data frame, where \emph{m} is the total number of items to sample from, whether it is included in the block or not, whereas \emph{r} is the number of item characteristics.
  }
  \item{weights}{
    A vector of length \emph{r} with weights for each item characteristics in \code{item_chars}. Should provide a weight of 0 for specific characteristics not of interest, such as item ID.
  }
  \item{FUN}{
    A vector of customized function names for optimizing each item characteristic within each block, with length \emph{r}.
  }
}
\details{
    This energy calculation function serves as the core for determining the acceptance or rejection of a newly built block over the previous one. Higher energy is considered more preferrable in this case.

    Items in the same block can be paired based on characteristics such as: Mean score, Item Factor, Factor loading, Item IRT Parameters, Reverse Coding, etc.

    Pairings of different characteristics can be optimized in different way, by determining the customized function vector \code{FUN} and the corresponding \code{weights}.
}
\value{
    A numeric value indicating the total energy for the given item block(s).
}

\author{
   Mengtong Li
}
\note{
   Use \code{cal_block_energy_with_iia} if inter-item agreement (IIA) metrics are needed.
}

\examples{

## Simulate 60 items loading on different Big Five dimensions,
## with different mean and item difficulty

item_dims <- sample(c("Openness","Conscientiousness","Neuroticism",
                      "Extraversion","Agreeableness"), 60, replace = TRUE)
item_mean <- rnorm(60, 5, 2)
item_difficulty <- runif(60, -1, 1)

## Construct data frame for item characteristics and produce
## 20 random triplet blocks with these 60 items

item_df <- data.frame(Dimensions = item_dims, Mean = item_mean, Difficulty = item_difficulty)
solution <- make_random_block(60, 60, 3)

## A function for determining whether items in a block are from different dimensions

facfun <- function(vec){
  if (typeof(vec) == "list") {vec <- unlist(vec)}
  return(ifelse(length(vec) == length(unique(vec)), 1, 0))
}

cal_block_energy(solution, item_chars = item_df,
                 weights = c(1,1,1), FUN = c("facfun", "var", "var"))

}
