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

## ----example2------------------------------------------------------------
data("classification_cts")
df2 <- classification_cts
max_item  <- max(df2)
min_item <- 0
max.item <- rep(max_item, dim(df2)[2])
min.item <- rep(min_item, dim(df2)[2])
df2 <- as.data.frame(df2)
modout <- cirtmodel(df2, max.item, min.item)
paras <- modout$model$param

gdf <- prepare_for_plots_crm(modout$model) 
ggplot(gdf, aes(theta, z)) + geom_raster(aes(fill=pdf))  + xlab("theta") + facet_wrap(~Algorithm, nrow=2) + coord_fixed(ratio=1) + theme_bw()  +  scale_fill_viridis_c(option = "plasma")  

## ----latenttrait---------------------------------------------------------

obj <- latent_trait_analysis(df2,modout$model$param,min.item,max.item )

dfl <- obj$longdf

g1 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_point(aes(color=Algorithm)) + xlab("Latent Trait (Dataset Easiness)") + ylab("Performance")  + theme_bw() 
g1

## ----latent2-------------------------------------------------------------

g3 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_point(aes(color=Algorithm)) + xlab("Latent Trait (Dataset Easiness)") + facet_wrap(~Algorithm, nrow=2) + coord_fixed(ratio=6) + ylab("Performance") + theme_bw() 
g3


## ----latent3-------------------------------------------------------------

### Curve fitting - smoothing splines - latent trait
g2 <- ggplot(dfl, aes(Latent_Trait, value)) +  geom_smooth(aes(color=Algorithm), se = FALSE, method = "gam", formula = y ~s(x, bs="cs"))+  xlab("Latent Trait (Dataset Easiness)") + ylab("Performance")  + theme_bw()  +theme(legend.position="bottom", legend.box = "horizontal")
g2

## ----latent 4------------------------------------------------------------
latent <- obj$latent
latent$proportions

setColors <- setNames( latent$proportions$colour, latent$proportions$algorithm)

df2 <- latent$latent
df3 <- cbind(df2, y=1)
df3 <- df3[ ,c(1,3,2)]
g4 <- ggplot(df3, aes(x,y)) + geom_point(aes(color=Algorithm),size=2, shape=15) + ylab("") + coord_fixed(ratio = 2) + theme(axis.title.y=element_blank(), axis.text.y=element_blank(),axis.ticks.y=element_blank()) + scale_color_manual(values = setColors) + xlab("Latent Trait") + theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())  +guides(group=guide_legend(nrow=3))
g4


## ----example-------------------------------------------------------------
data("classification_poly")
modout <- pirtmodel(classification_poly, vpara=FALSE)

gdf <- prepare_for_plots_poly(modout$model)
ggplot(gdf, aes(Theta, value)) + geom_line(aes(color=Level)) + facet_wrap(.~Algorithm) + ylab("Probability") + ggtitle("Classification Algorithm Trace Lines") + theme_bw()

## ---- goodness, echo=TRUE------------------------------------------------
# Model Goodness and Algorithm effectiveness
good <- model_goodness_poly(modout$model)

good_curves <- as.data.frame(good$curves)
print(good_curves)
good_df <- good_curves %>% pivot_longer(cols=2:dim(good_curves)[2], names_to=c("Algorithm"))
ggplot(good_df, aes(x,value)) + geom_point() + geom_line(aes(color = Algorithm), size=1) + xlab("Goodness Tolerance")  + ylab("Model Goodness Curve") + theme_bw()

## ----AUMGC---------------------------------------------------------------
good$goodnessAUC

## ---- effectiveness1, echo=TRUE------------------------------------------
eff <- effectiveness_poly(modout$model)

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

