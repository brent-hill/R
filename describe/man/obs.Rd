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
obs(data.obj, nonNA = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.obj}{
	Data object name; must be a vector, factor, or data frame.
  }
  \item{nonNA}{
	Logical (\code{TRUE}, \code{FALSE}); if set to \code{TRUE}, there will be separate counts for non-NA (non-missing) and NA (missing) observations. The default setting is \code{FALSE}.
  }
}
\details{
If a data frame is specified without referencing a specific variable, and \code{nonNA = TRUE}, then results will be shown for each variable in the data frame.
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
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or standard data sets, see data().
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
