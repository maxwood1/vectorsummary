\name{vectorplots}
\alias{vectorplots}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Outputs plots for given numeric vector
}
\description{
  This function outputs summary plots for a numeric vector including a scatterplot, a boxplot, a histogram, and a kernel density plot
}
\usage{
vectorplots(x)
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
  No value is returned
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
vectorplots(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
