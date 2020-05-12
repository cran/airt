## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  warning=FALSE,
  comment = "#>",
  fig.width=8, fig.height=6
)

## ----load2, echo=FALSE, eval=TRUE, message=FALSE-------------------------
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

## ----load, message=FALSE-------------------------------------------------
library(airt)
library(ggplot2)
library(tidyr)

## ----example-------------------------------------------------------------
data("classification")
modout <- irtmodel(classification, vpara=FALSE)

gdf <- prepare_for_plots(modout$model)
ggplot(gdf, aes(Theta, value)) + geom_line(aes(color=Level)) + facet_wrap(.~Algorithm) + ylab("Probability") + ggtitle("Classification Algorithm Trace Lines") + theme_bw()

## ---- goodness, echo=TRUE------------------------------------------------
# Model Goodness and Algorithm effectiveness
good <- model_goodness(modout$model)

good_curves <- as.data.frame(good$curves)
print(good_curves)
good_df <- good_curves %>% pivot_longer(cols=2:dim(good_curves)[2], names_to=c("Algorithm"))
ggplot(good_df, aes(x,value)) + geom_point() + geom_line(aes(color = Algorithm), size=1) + xlab("Goodness Tolerance")  + ylab("Model Goodness Curve") + theme_bw()

## ----AUMGC---------------------------------------------------------------
good$goodnessAUC

## ---- effectiveness1, echo=TRUE------------------------------------------
eff <- effectiveness(modout$model)

eff_curves <- as.data.frame(eff$actcurves)
eff_df1 <- eff_curves %>% pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))

eff_curves <- as.data.frame(eff$prdcurves)
eff_df2 <- eff_curves %>% pivot_longer(cols=2:dim(eff_curves)[2], names_to=c("Algorithm"))

eff_df <- rbind.data.frame(eff_df1, eff_df2)
eff_df <- cbind.data.frame( eff_df, c( rep( "Actual Effectiveness", dim(eff_df1)[1]), rep("Predicted Effectiveness", dim(eff_df2)[1]) ) )
colnames(eff_df)[4] <- "Act_Or_Pred"
ggplot(eff_df, aes(x, value)) + geom_point() + geom_line(aes(color = Algorithm), size=1)  + facet_wrap(~Act_Or_Pred) + theme_bw()

## ---- effectiveness2, echo=TRUE------------------------------------------
df_eff <- cbind.data.frame(as.data.frame(eff$effectivenessAUC), rownames(eff$effectivenessAUC) )
colnames(df_eff)[3] <- "Algorithm"

ggplot(df_eff, aes(Actual, Predicted)) + geom_jitter(aes(color=Algorithm), size=3) + geom_abline(aes(intercept=0,slope=1), linetype="dotted") + xlim(c(0,1)) + ylim(c(0,1)) + xlab("Area under Actual Effectiveness Curve (AUAEC)") + ylab("Area under Predicted Effectiveness Curve (AUPEC)") +  theme_bw()

measures <- cbind.data.frame(good$goodnessAUC, eff$effectivenessAUC)
print(measures)

## ----moremeasures--------------------------------------------------------
stab <- modout$stability
anomalous <- modout$anomalous
cbind.data.frame(stab, anomalous)

