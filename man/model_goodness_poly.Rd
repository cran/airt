% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model_goodness_poly.R
\name{model_goodness_poly}
\alias{model_goodness_poly}
\title{Computes the goodness of IRT model for all algorithms.}
\usage{
model_goodness_poly(mod)
}
\arguments{
\item{mod}{A fitted \code{mirt} model using the function \code{irtmodel} or \code{R} package \code{mirt}.}
}
\value{
A list with the following components:
\item{\code{goodnessAUC}}{The area under the model goodness curve for each algorithm. }
\item{\code{mse}}{The mean squared error. }
\item{\code{curves}}{The \code{x,y} coodinates for the model goodness curves for each algorithm. }
}
\description{
This function computes the goodness of the IRT model for all algorithms using the empirical cumulative distribution function of errors.
}
\examples{
set.seed(1)
x1 <- sample(1:5, 100, replace = TRUE)
x2 <- sample(1:5, 100, replace = TRUE)
x3 <- sample(1:5, 100, replace = TRUE)
X <- cbind.data.frame(x1, x2, x3)
mod <- pirtmodel(X)
out <- model_goodness_poly(mod$model)
out
}
