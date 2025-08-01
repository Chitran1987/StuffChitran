\name{success.prob}
\alias{success.prob}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Probability of at least one succesful event
}
\description{
Returns the (binomial)probability of AT LEAST one succesful event in a series of \eqn{n} independent events, where in the probability \eqn{p} of an independently succesful event is known.\cr
The probability is calculated as \eqn{P(r \geq 1) = 1 - (1 - p)^n}.\cr
This is calculated from the binomial probability distribution \eqn{P(r) = \binom{n}{r}p^r (1-p)^{n-r}}.\cr
In this case, the variable is \eqn{n}.\cr
The function returns probability values from \eqn{X = \{0, 1, 2, .... , n\}}.
}
\usage{
success.prob(p, n, pl)
success.prob(p, n, pl = F)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{p}{
  The probability \eqn{p} of the success of a single event
  }
  \item{n}{
  The no of trials till which you want the probability \eqn{X = \{0, 1, 2, ... , n\}}
  }
  \item{pl}{
  Whether a plot is needed or not.\cr
  Defaults to true.
  }
}
\details{
The parameter \eqn{n} has to be a positive integer.\cr
The parameter \eqn{p \leq 1}
}
\value{
The returned value is always a dataframe
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Chitran Ghosal
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
df <- success.prob(p = 1/10, n = 20)
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
