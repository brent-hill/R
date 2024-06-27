\name{dsnv}
\alias{dsnv}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Descriptive Statistics for Numerical Variables
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
dsnv(data.frame, ..., by = NULL, stats = "n mean", dec = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data.frame}{
	Name of a data frame
  }
  \item{...}{
	List of numerical variables (columns) from the data frame (separated by commas)
  }
  \item{stats}{
	A character string specifying the descriptive statistics to be computed for each of the numerical variables. Current options are \code{n} (count of non-NA observations), \code{mean}, and \code{sd}. One or more can be specified in this list (must be enclosed in quotation marks).
  }
  \item{by}{Specifies a grouping variable.}
  \item{dec}{Number of decimal places. ==[not operational yet]==}
  }
\details{
%%  ~~ If necessary, more details than the description above ~~
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
\code{\link{mean}}, \code{\link{sd}}
}
\examples{
##-- Examples use the built-in dataset -mtcars-.
dsnv(mtcars, mpg, wt)
dsnv(mtcars, mpg, wt, stats="n mean sd")
dsnv(mtcars, mpg, wt, stats="n mean sd", by=cyl)
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
