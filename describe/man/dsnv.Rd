\name{dsnv}
\alias{dsnv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptive Statistics for Numerical Variables
}
\description{
This function computes various descriptive statistics for numerical variables. The available statistics are sample size, mean, median, variance, standard deviation, skewness, and kurtosis.
}
\usage{
dsnv(..., by = NULL, stats = "mean", dec = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
	Listing of numerical variables. There are a few different ways to specify the variables:
	\itemize{
		\item As a listing of one or more standalone vectors (i.e., autonomous variables not in a data frame)
		\item As a listing of one or more vectors from a data frame using the \code{$} operator
		\item A data frame followed by a listing of one or more column names from the data frame
		\item A data frame with nothing else specified (this implicitly specifies all numeric columns in the data frame)
	}
  }
  \item{stats}{
	Character string specifying the descriptive statistics to be computed for each of the numerical variables. The options are as follows:
	\tabular{ll}{
		\code{n}      \tab Count of non-NA (non-missing) observations \cr
		\code{mean}   \tab Mean \cr
		\code{median} \tab Median \cr
		\code{var}    \tab Variance \cr
		\code{sd}     \tab Standard deviation \cr
		\code{skew}   \tab Skewness \cr
		\code{kurt}   \tab Kurtosis \cr
	}
	One or more of these can be specified in this list (must be enclosed in quotation marks).

	==[ NOTE: Presently, it is not possible to request a subset of the descriptive statistics; all of these statistics are computed and reported. ]==
  }
  \item{by}{Specifies a grouping variable. ==[ not yet operational ]==}
  \item{dec}{Number of decimal places. ==[ not yet operational ]==}
}
\value{
I guess some big-ass matrix of all the descriptive statistics computed.
}
\references{
	\href{https://publish.obsidian.md/omnibus/EDUC+702/(00)+Table+of+Contents}{EDUC 702 Textbook}
}
\author{
Brent Hill
}
\note{
This function is still being developed and fine-tuned, so not all arguments/features are available.

All the individual statistical functions used for the computations in this function are set to \code{na.rm = TRUE}.

Skewness and kurtosis are computed using functions from the 'moments' package.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
	\code{\link{mean}}, \code{\link{median}}, \code{\link{var}}, \code{\link{sd}}, \code{\link{skewness}}, \code{\link{kurtosis}}
}
\examples{
## Example with standalone vectors
## First create some vectors:
%% vec1 <- c(12, 65, 34, NA, 28, 55, 23)
%% vec2 <- rnorm(25, 100, 15)
%% dsnv(vec1, vec2)

## Example with variables from a data frame using the $ operator
## (This example uses the built-in data frame -mtcars-.)
%% dsnv(mtcars$mpg, mtcars$wt, mtcars$cyl)

## Example with data frame and list of column names
%% dsnv(mtcars, mpg, wt, cyl)

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
