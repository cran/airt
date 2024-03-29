% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/irt_model_polytomous.R
\name{pirtmodel}
\alias{pirtmodel}
\title{Fits a polytomous IRT model.}
\usage{
pirtmodel(dat, ncycle = NULL, vpara = TRUE)
}
\arguments{
\item{dat}{The performance data in a matrix or dataframe.}

\item{ncycle}{The number of cycles for \code{mirt}. The default is 500.}

\item{vpara}{It \code{TRUE} the \code{verbose} parameter for the \code{mirt} would be set to true.}
}
\value{
A list with the following components:
\item{\code{model}}{The IRT model using the R package \code{mirt}.  }
\item{\code{anomalous}}{A binary value for each algorithm. It is set to 1 if an algorithm is anomalous. Otherwise it is set to 0.  }
 \item{\code{consistency}}{The consistency of each algorithm.}
 \item{\code{difficulty_limit}}{The difficulty limits for each algorithm.  A higher threshold indicates that the algorithm can tackle harder problems.}
}
\description{
This function fits a polytomous Item Response Theory (IRT) model using the R package mirt
to the algorithm performance data.
}
\examples{
set.seed(1)
x1 <- sample(1:5, 100, replace = TRUE)
x2 <- sample(1:5, 100, replace = TRUE)
x3 <- sample(1:5, 100, replace = TRUE)
X <- cbind.data.frame(x1, x2, x3)
mod <- pirtmodel(X)

}
\references{
R. Philip Chalmers (2012). mirt: A Multidimensional Item Response Theory Package for the R
Environment. Journal of Statistical Software, 48(6), 1-29. doi:10.18637/jss.v048.i06
}
