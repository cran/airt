## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning=FALSE,
  comment = "#>",
  fig.width=8, fig.height=6
)

## ----load2, echo=FALSE, eval=TRUE, message=FALSE------------------------------
if (!requireNamespace("airt", quietly = TRUE)) {
    stop("Package airt is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package ggplot2 is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package tidyr is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package gridExtra is needed for the vignette. Please install it.",
      call. = FALSE)
}
if (!requireNamespace("scales", quietly = TRUE)) {
    stop("Package scales is needed for the vignette. Please install it.",
      call. = FALSE)
}

## ----load, message=FALSE------------------------------------------------------
library(airt)
library(ggplot2)
library(tidyr)
library(gridExtra)

## ----example2-----------------------------------------------------------------
data("classification_cts")
df <- classification_cts
head(df)

## ----classificationairt-------------------------------------------------------
modout <- cirtmodel(df)

## ----irtparas-----------------------------------------------------------------
paras <- modout$model$param
paras

## ----airtparas----------------------------------------------------------------

## ----heatmaps-----------------------------------------------------------------
obj <- heatmaps_crm(modout) 
autoplot(obj)

## ----latenttrait--------------------------------------------------------------
obj <- latent_trait_analysis(df, modout$model$param, epsilon = 0 )
autoplot(obj, plottype = 1)

## ----latent2------------------------------------------------------------------
autoplot(obj, plottype = 2)

## ----latent3------------------------------------------------------------------
autoplot(obj, plottype = 3)

## ----lto----------------------------------------------------------------------
obj$strengths$proportions


## ----weaknesses---------------------------------------------------------------
obj$weakness$proportions

## ----latent4------------------------------------------------------------------
autoplot(obj, plottype = 4)

## ----latent5------------------------------------------------------------------
obj2 <- latent_trait_analysis(df, modout$model$param, epsilon = 0.02 )
autoplot(obj2, plottype = 4)

## ----modelgoodness------------------------------------------------------------

modelgood <- model_goodness_crm(modout)
autoplot(modelgood)


## ----modelgoodness2-----------------------------------------------------------
cbind.data.frame(AUC = modelgood$goodnessAUC, MSE = modelgood$mse)


## ----effectiveness1-----------------------------------------------------------
modeleff <- effectiveness_crm(modout)
autoplot(modeleff, plottype = 1)

## ----effectiveness2-----------------------------------------------------------
autoplot(modeleff, plottype = 2)
autoplot(modeleff, plottype = 3)

## ----example------------------------------------------------------------------
data("classification_poly")
modout <- pirtmodel(classification_poly, vpara=FALSE)

obj <- tracelines_poly(modout)
autoplot(obj)

## ----poly2--------------------------------------------------------------------
cbind.data.frame(consistency = modout$consistency, anomalousness = modout$anomalous, difficulty_level = modout$difficulty_limit[, 1])

## ---- goodnesspoly------------------------------------------------------------

modelgoodness <- model_goodness_poly(modout)
autoplot(modelgoodness)


## ----effectivenesspoly--------------------------------------------------------
effpoly <- effectiveness_poly(modout)
autoplot(effpoly, plottype = 3)


