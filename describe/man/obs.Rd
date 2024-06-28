\name{obs}
\alias{obs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Count of Observations in a Data Object
}
\description{
This function provides a count of the number of observations (i.e., rows, records) in the specified data object.
}
\usage{
obs(data.obj, sepNA = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.obj}{
	Data object name; must be a vector, factor, or data frame.
  }
  \item{sepNA}{
	Logical (\code{TRUE}, \code{FALSE}); if set to \code{TRUE}, there will be separate counts for non-NA (non-missing) and NA (missing) observations. The default setting is \code{FALSE}.
  }
}
\details{
If a data frame is specified without referencing a specific variable, and \code{sepNA = TRUE}, then results will be shown for each variable in the data frame.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Brent Hill
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{length}}, \code{\link{nrow}}
}
\examples{
## -- Using the mtcars built-in data frame
obs(mtcars)

## -- Setup for example: Create a vector with NA (missing) observations
x <- c(2.3, 7.7, NA, 3.8, 4.0, NA, 3.3, 4.8)
obs(x)
obs(x, sepNA=TRUE)

## -- Setup for example: Create a data frame with NA (missing) observations
df <- df <- data.frame(col1=c(1,2,3,4,5), col2=c(1,2,1,NA,1), col3=c(41,NA,33,29,NA))
### -- Variable from data frame with no NAs
obs(df$col1)
### -- Variables from data frame with NAs
obs(df$col2)
obs(df$col2, sepNA=TRUE)
### -- Data frame only (no variable specified)
obs(df)
obs(df, sepNA=TRUE)
}

