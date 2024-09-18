\name{distinct}
\alias{distinct}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Number of Distinct Values in a Variable
}
\description{
This function provides a count of distinct observed values in a given variable.
}
\usage{
distinct(x, show.vals = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
	A single variable specified as a vector or factor.
  }
  \item{show.vals}{
	Logical (\code{TRUE} or \code{FALSE}). If set to \code{TRUE}, the collection of observed values is displayed.
  }
}
\details{
This function does not count NAs.
}
\author{
Brent Hill
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{unique}}
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().
distinct(iris$Species, show.vals=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
% \keyword{ ~kwd1 }
% \keyword{ ~kwd2 }
% Use only one keyword per line.
% For non-standard keywords, use \concept instead of \keyword:
% \concept{ ~cpt1 }
% \concept{ ~cpt2 }
% Use only one concept per line.
