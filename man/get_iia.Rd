\name{get_iia}
\alias{get_iia}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Helper Function for Outputting IIA Characteristics of Each Block
}
\description{
  This function prints IIA metrics for select items, given the individual responses for the items
}
\usage{
  get_iia(block, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{block}{
     An \emph{n} by \emph{k} integer matrix, where \emph{n} is the number of item blocks and \emph{k} is the number of items per block.
}
  \item{data}{
     A \emph{p} by \emph{m} numeric matrix with scores of each of the \emph{p} participants for the \emph{m} items.
}
}

\value{
    An \emph{n} by \emph{k} matrix indicating the four IIA metrics for each item block
}

\author{
Mengtong Li
}

\examples{

item_responses <- matrix(sample(seq(1:5), 600*60, replace = TRUE), ncol = 60)
get_iia(matrix(seq(1:60), ncol = 3), item_responses)

}
