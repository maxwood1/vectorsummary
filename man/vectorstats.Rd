\name{vectorstats}
\alias{vectorstats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Output summary statistics for numeric vector
}
\description{
  This function outputs the quantiles, mean, standard deviation, and standard error of a given numeric vector.
}
\usage{
vectorstats(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
  A numeric vector
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
  The return value is a statsummary class object with two sub-parts. The first contains a vector of quantiles and the second contains a vector with the mean, standard deviation, and standard error.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
  Max Woodbury
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
x <- rnorm(100)
vectorstats(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
