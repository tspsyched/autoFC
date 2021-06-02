\name{facfun}
\alias{facfun}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function for Checking If All Items in a Vector Are Unique
}
\description{
Returns 1 if each element in the vector is unique, and 0 otherwise.
}
\usage{
facfun(vec)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{vec}{
    Input vector.
}
}

\value{
   1 if each element in the vector is unique, and 0 otherwise.
}

\author{
   Mengtong Li
}


\examples{
  facfun(c("Openness", "Neuroticism", "Agreeableness"))
  facfun(c("Openness", "Openness", "Agreeableness"))
}
