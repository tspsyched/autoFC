\name{make_random_block}
\alias{make_random_block}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Construction of Random Item Blocks
}
\description{
  Returns a matrix of randomly paired blocks where each row represents a block.
}
\usage{
   make_random_block(total_items, target_items = total_items, item_per_block)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{total_items}{
    Integer value. Determines the total number of items we sample from.
  }
  \item{target_items}{
    Interger value. Determines the number of items to use from \code{total_items} to build item blocks. Default to be equal to \code{total_items}. Should be less than \code{total_items}.
  }
  \item{item_per_block}{
    Integer value. Determines the number of items in each item block.
    Should be no less than 2.
  }
}
\details{
  Given the total number of items to pair from, number of items to build paired blocks and number of items in each block, \code{make_random_block} produces a matrix randomly paired blocks where each row represents a block.
  It can also accommodate cases when \code{target_items} is not a multiple of \code{item_per_block}.
  Can be used as initial solution for other functions in this package.
}
\value{
  A matrix of integers indicating the item numbers, where the number of rows equals \code{ceiling(target_items/item_per_block)} and number of columns equals \code{item_per_block}.
}

\author{
  Mengtong Li
}
\note{
   If \code{target_items} is not a multiple of \code{item_per_block}, the item set produced by \code{target_items} will be looped until number of sampled items becomes a multiple of \code{item_per_block}.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
# Try out cases where you make target_items the default.
make_random_block(60, item_per_block = 3)

# You can also set your own values of target_items.
make_random_block(60, 45, item_per_block = 3)

# Also see what happens if target_items is not a multiple of item_per_block.
make_random_block(60, 50, item_per_block = 3)

}

