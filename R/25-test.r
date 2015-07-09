################################################################################
###
### Estimate the fitted ensemble's out-of-sample prediction error by applying it
### to the previously untouched test sample
###
################################################################################

library("caret")
library("dplyr")
library("foreach")
library("ggtern")
library("tikzDevice")
library("xtable")

Sys.unsetenv("TEXINPUTS")

source("20-predict-from-ensemble.r")

load("results-data-test.rda")
load("results-full-ensemble.rda")


###-----------------------------------------------------------------------------
### Calculate metrics
###-----------------------------------------------------------------------------

## Calculate predicted probabilities for the null model and capability ratio
predict_single <- function(model) {
    pred <- foreach (fit = full_ensemble) %do% {
        predict(fit$models[[model]], newdata = data_test, type = "prob")
    }
    pred <- Reduce("+", pred)
    pred <- pred / length(full_ensemble)

    pred
}

pred_test_null <- predict_single("null")
pred_test_capratio <- predict_single("polr_capratio")

## Calculate predicted probabilities for the ensemble
pred_test_ensemble <- predict_from_ensemble(data_test, full_ensemble)

## Calculate log loss for each
log_loss <- function(pred) {
    ## mnLogLoss() wants the true outcome stored in a variable called `obs`
    pred$obs <- data_test$Outcome

    mnLogLoss(data = pred, lev = levels(data_test$Outcome))
}

(ll_test_null <- log_loss(pred_test_null))
(ll_test_capratio <- log_loss(pred_test_capratio))
(ll_test_ensemble <- log_loss(pred_test_ensemble))

## Calculate proportional reductions in loss
(prl_test_capratio <- (ll_test_null - ll_test_capratio) / ll_test_null)
(prl_test_ensemble <- (ll_test_null - ll_test_ensemble) / ll_test_null)

## Calculate accuracy and kappa for each
classify <- function(pred) {
    ## Calculate maximal prediction in each row
    cl <- apply(pred, 1, which.max)

    ## Convert to factor with appropriate levels
    factor(cl,
           levels = seq_len(ncol(pred)),
           labels = colnames(pred))
}

class_metrics <- function(pred) {
    postResample(pred = classify(pred),
                 obs = data_test$Outcome)
}

(cm_test_null <- class_metrics(pred_test_null))
(cm_test_capratio <- class_metrics(pred_test_capratio))
(cm_test_ensemble <- class_metrics(pred_test_ensemble))


###-----------------------------------------------------------------------------
### Make table of output
###-----------------------------------------------------------------------------

test_table <- data.frame(
    model = c("Null Model", "Capability Ratio", "Super Learner"),
    logLoss = c(ll_test_null, ll_test_capratio, ll_test_ensemble),
    PRL = c(NA, prl_test_capratio, prl_test_ensemble),
    Accuracy = c(cm_test_null[1], cm_test_capratio[1], cm_test_ensemble[1]),
    Kappa = c(cm_test_null[2], cm_test_capratio[2], cm_test_ensemble[2]),
    stringsAsFactors = FALSE
)

## Prettify numeric values
test_table <- test_table %>%
    mutate_each(funs(sprintf("%.3f", .)),
                -model) %>%
    mutate_each(funs(ifelse(. == "NA", "", .)))

## Convert to LaTeX
test_xtable <- xtable(test_table,
                      align = c("l", "l", "r", "r", "r", "r"))
colnames(test_xtable) <- c("Model",
                           "Log Loss",
                           "P.R.L.",
                           "Accuracy",
                           "Kappa")

## Write to file
print(test_xtable,
      file = file.path("..", "latex", "tab-test.tex"),
      floating = FALSE,
      include.rownames = FALSE)


###-----------------------------------------------------------------------------
### Ternary plot of predicted probabilities
###-----------------------------------------------------------------------------

## Combine predictions from the two models
pred_test_capratio$model <- "Capability Ratio"
pred_test_ensemble$model <- "Super Learner"
plot_data <- rbind(pred_test_capratio,
                   pred_test_ensemble)
plot_data$obs <- factor(data_test$Outcome,
                        levels = c("Stalemate", "VictoryA", "VictoryB"),
                        labels = c("$\\emptyset$", "$A$", "$B$"))

## Separate category without stalemates
plot_data_victories <- filter(plot_data,
                              obs != "$\\emptyset$")
plot_data$sample <- "All Disputes"
plot_data_victories$sample <- "No Stalemates"
plot_data <- rbind(plot_data, plot_data_victories)

tikz(file = file.path("..", "latex", "fig-tern.tex"),
     width = 5,
     height = 6)
ggtern(plot_data,
       aes(x = VictoryA,
           y = Stalemate,
           z = VictoryB)) +
    geom_point(aes(shape = obs,
                   colour = obs),
              alpha = 0.5) +
    scale_colour_brewer("Observed Outcome",
                        palette = "Set1",
                        labels = c("Stalemate", "A Wins", "B Wins")) +
    scale_shape("Observed Outcome",
                labels = c("Stalemate", "A Wins", "B Wins")) +
    scale_T_continuous("$\\emptyset$") +
    scale_L_continuous("$A$") +
    scale_R_continuous("$B$") +
    facet_grid(sample ~ model) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_grey(base_size = 10) +
    theme(axis.tern.ticks = element_blank(),
          axis.tern.text = element_text(size = rel(0.8)),
          legend.position = "bottom")
dev.off()
