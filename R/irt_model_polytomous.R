#' Fits a polytomous IRT model.
#'
#' This function fits a polytomous Item Response Theory (IRT) model using the R package mirt
#' to the algorithm performance data.
#'
#' @param dat The performance data in a matrix or dataframe.
#' @param ncycle The number of cycles for \code{mirt}. The default is 500.
#' @param vpara It \code{TRUE} the \code{verbose} parameter for the \code{mirt} would be set to true.
#'
#' @return A list with the following components:
#' \item{\code{model}}{The IRT model using the R package \code{mirt}.  }
#' \item{\code{anomalous}}{A binary value for each algorithm. It is set to 1 if an algorithm is anomalous. Otherwise it is set to 0.  }
#'  \item{\code{consistency}}{The consistency of each algorithm.}
#'  \item{\code{difficulty_limit}}{The difficulty limits for each algorithm.  A higher threshold indicates that the algorithm can tackle harder problems.}
#'
#'@examples
#'set.seed(1)
#'x1 <- sample(1:5, 100, replace = TRUE)
#'x2 <- sample(1:5, 100, replace = TRUE)
#'x3 <- sample(1:5, 100, replace = TRUE)
#'X <- cbind.data.frame(x1, x2, x3)
#'mod <- pirtmodel(X)
#'
#'@references R. Philip Chalmers (2012). mirt: A Multidimensional Item Response Theory Package for the R
#'Environment. Journal of Statistical Software, 48(6), 1-29. doi:10.18637/jss.v048.i06
#'@export
pirtmodel <- function(dat, ncycle=NULL, vpara= TRUE){
  # CHECK IF DATA IS IN A DATA FRAME OR MATRIX
  if(!(is.data.frame(dat)|is.matrix(dat))){
    stop("Data needs to be a matrix or a dataframe!")
  }
  # CHECK FOR NAs, NANs
  na_sums <- sum(apply(dat, 2, function(x) sum(is.na(x))))
  nan_sums <- sum(apply(dat, 2, function(x) sum(is.nan(x))))
  if(na_sums > 0 ){
    stop("Data contains NA. Please fix!")
  }
  if(nan_sums>0){
    stop("Data contains NaN. Please fix!")
  }

  # CHECK IF NUMBER OF UNIQUE VALUES IS TOO HIGH
  unique_vals <- length(unique(as.vector(dat)))
  if(unique_vals > 10){
    stop("Data contains more than 10 levels for polytomous IRT. Please fix!")
  }

  # CHECK IF ALL ARE INTEGERS
  int_cols <- apply(dat, 2, function(x) all.equal(x, as.integer(x)))
  if(sum(int_cols=="TRUE") < dim(dat)[2]){
    stop("Data contains non-integer values. Please fix!")
  }

  # CHECK FOR COLUMN NAMES
  if(is.null(colnames(dat))){
    stop("Column names are empty! Please fix!")
  }

  # DATA IS GOOD FOR A mirt POLYTOMOUS MODEL
  if(is.null(ncycle)){
    mod <- mirt::mirt(dat, 1, itemtype = 'gpcm', verbose = vpara)
  }else if(is.numeric(ncycle)){
    mod <- mirt::mirt(dat, 1, itemtype = 'gpcm', verbose = vpara, technical=list(NCYCLES=ncycle))
  }


  a_vals <- coef(mod, IRTpars = TRUE, simplify=TRUE)$items[ ,1]
  flipped <- sign(a_vals)
  anomalous <- rep(0, length(flipped))
  anomalous[flipped==-1] <- 1
  stability <-  1/abs(a_vals)

  # easiness threshold parameters
  # all_coeffs <- coef(mod, IRTpars=TRUE, simplify=TRUE)$items[ ,-1]
  # coefdim <- dim(all_coeffs)[2]
  # diff_start <- 2
  # easiness_threshold <- coef(mod, IRTpars=TRUE, simplify=TRUE)$items[ ,-1]
  difficulty_limit <- -1*coef(mod, IRTpars=TRUE, simplify=TRUE)$items[ ,-1]

  out <- list()
  out$model <- mod
  out$anomalous <- anomalous
  out$consistency <- stability
  out$difficulty_limit <- difficulty_limit
  return(out)
}
