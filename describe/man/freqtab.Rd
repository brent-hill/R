\name{freqtab}
\alias{freqtab}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Basic Frequency Table for a Single Variable
}
\description{
This function provides a frequency table for a single variable. The frequency (f) and relative frequency (rf) are computed for each distinct observed value in the specified variable. Optionally, the cumulative frequency (cf) and cumulative relative frequency (crf) are also available.
}
\usage{
freqtab(data, cml = FALSE, countNA = FALSE, pct = FALSE, dec = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
	The input variable (as a vector or factor).
  }
  \item{cml}{
	Logical (\code{TRUE} or \code{FALSE}). If \code{TRUE}, then the cf and crf columns are displayed. Default setting is \code{FALSE}.
  }
  \item{countNA}{
Logical (\code{TRUE} or \code{FALSE}). If \code{TRUE}, then the results for missing (NA) values are also shown. Default setting is \code{FALSE}.
  }
  \item{pct}{
Logical (\code{TRUE} or \code{FALSE}). If \code{TRUE}, then rf and crf are displayed as percentages; otherwise, these are shown as proportions. Default setting is \code{FALSE}.
  }
  \item{dec}{
	Number of decimal places displayed in the rf and crf results.
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns the frequancy table--i.e., the results of the \code{table()} function.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Freund & Wilson (20??)
}
\author{
Brent Hill
}
\note{
%%  ~~further notes~~
}

\seealso{
\code{\link{table}},
\code{\link{prop.table}}
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
