################################################################################
###
### Compare DOE scores with capability ratios
###
################################################################################

library("MASS")
library("caret")
library("dplyr")
library("foreach")
library("ggplot2")
library("ggtern")
library("RColorBrewer")
library("tidyr")
library("tikzDevice")

Sys.unsetenv("TEXINPUTS")

load("results-data-nmc.rda")
load("results-imputations-train.rda")
load("results-trained-weights.rda")

pred_dir_dyad <- read.csv("results-predict-dir-dyad.csv")
pred_dyad <- read.csv("results-predict-dyad.csv")


###-----------------------------------------------------------------------------
### Basic correlations
###-----------------------------------------------------------------------------

## Calculate capability ratios for all dyads
make_capratio <- function(pred) {
    pred %>%
        left_join(data_NMC %>% select(ccode, year, cinc_a = cinc),
                  by = c(ccode_a = "ccode", year = "year")) %>%
        left_join(data_NMC %>% select(ccode, year, cinc_b = cinc),
                  by = c(ccode_b = "ccode", year = "year")) %>%
        mutate(capratio = exp(cinc_a) / (exp(cinc_a) + exp(cinc_b)))
}

pred_dir_dyad <- make_capratio(pred_dir_dyad)
pred_dyad <- make_capratio(pred_dyad)

## Calculate correlations
with(pred_dir_dyad, cor(VictoryA, capratio, use = "complete"))
with(pred_dir_dyad, cor(VictoryB, capratio, use = "complete"))
with(pred_dyad, cor(VictoryA, capratio, use = "complete"))
with(pred_dyad, cor(VictoryB, capratio, use = "complete"))

## Calculate canonical correlations
with(pred_dir_dyad[!is.na(pred_dir_dyad$capratio), ],
     cancor(cbind(VictoryA, VictoryB), capratio))
with(pred_dyad[!is.na(pred_dyad$capratio), ],
     cancor(cbind(VictoryA, VictoryB), capratio))


###-----------------------------------------------------------------------------
### Out-of-fold predicted probabilities
###-----------------------------------------------------------------------------

dat <- imputations_train[[1]]

## Calculate out-of-fold predictions for the capability ratio model
capratio_folds <- createFolds(y = dat$Outcome,
                              k = 10,
                              list = TRUE,
                              returnTrain = TRUE)
pp_capratio <- foreach (fold = capratio_folds, .combine = "rbind") %do% {
    ## Fit the capability ratio model
    polr_capratio <- polr(Outcome ~ capratio,
                          data = dat[fold, , drop = FALSE])

    ## Calculate out-of-fold predictions
    pred <- predict(polr_capratio,
                    newdata = dat[-fold, , drop = FALSE],
                    type = "prob")

    ## Convert to data frame and include observed outcome
    pred <- data.frame(pred)
    pred$method <- "capratio"
    pred$Outcome <- dat$Outcome[-fold]

    pred
}

## Calculate out-of-fold predictions from the Super Learner
##
## This uses the tuning parameters that were trained by CV on the full sample,
## but the candidate model predictions being averaged are purely out-of-fold
pp_doe <- foreach (i = seq_along(trained_weights)) %do% {
    imp <- trained_weights[[i]]

    ## The `all_probs` object is a list with n_fold elements, each of which is a
    ## list of n_model data frames of predictions and observed outcomes for each
    ## fold.
    ##
    ## First we'll switch the order, so we have a list of folds by model instead
    ## of vice versa
    all_models <- foreach (model = names(imp$all_probs[[1]])) %do% {
        foreach (fold = imp$all_probs) %do% {
            fold[[model]]
        }
    }

    ## Now we want to put the observations back in their original order, since
    ## the fold assignments are different across imputations.  We foolishly
    ## forgot to save the fold assignments, so we'll need to reverse-engineer
    ## them from the code in `15-train-weights.r`.
    set.seed(100 * i)
    cv_folds <- createFolds(y = dat$Outcome,
                            k = length(imp$all_probs),
                            list = TRUE,
                            returnTrain = TRUE)
    all_models <- foreach(model = all_models) %do% {
        ## Piece back together the matrix, fold by fold
        ##
        ## Need to convert to matrix so we can assemble the storage in advance
        n_obs <- sum(sapply(model, nrow))
        pred_mat <- matrix(NA, nrow = n_obs, ncol = 4)
        for (fold in seq_along(model))
            pred_mat[-cv_folds[[fold]], ] <- data.matrix(model[[fold]])
        colnames(pred_mat) <- c("VictoryB", "Stalemate", "VictoryA", "obs")

        pred_mat
    }

    ## Double-check ordering of the matrices by making sure the outcomes are
    ## consistent with those in the original data
    ##
    ## This also allows us not to save the outcome variable, and just work with
    ## the probabilities, since we can re-retrieve the outcome from the source
    ## data
    for (model in all_models) {
        obs <- ordered(model[, "obs"],
                       levels = 1:3,
                       labels = c("VictoryB", "Stalemate", "VictoryA"))
        stopifnot(obs == dat$Outcome)
    }

    ## Calculate weighted average of predictions
    wts <- imp$weights$par
    wts <- c(wts, 1 - sum(wts))
    weighted_preds <- Map("*", all_models, wts)
    weighted_preds <- Reduce("+", weighted_preds)

    weighted_preds[, 1:3]
}

## Average across imputations and validate
pp_doe <- Reduce("+", pp_doe)
pp_doe <- pp_doe / length(trained_weights)
stopifnot(all.equal(range(rowSums(pp_doe)), c(1, 1)))

## Convert to data frame
pp_doe <- pp_doe %>%
    data.frame() %>%
    mutate(method = "doe")
pp_doe$Outcome <- dat$Outcome

## Make data to pass to ggplot
pp_data <- rbind(pp_capratio, pp_doe) %>%
    mutate(Outcome = factor(
               Outcome,
               levels = c("VictoryA", "Stalemate", "VictoryB"),
               labels = c("Outcome: A Wins", "Outcome: Stalemate", "Outcome: B Wins")
           ),
           method = factor(
               method,
               levels = c("capratio", "doe"),
               labels = c("Ordered Logit on Capability Ratio",
                          "Super Learner")
           ))
pp_data <- rbind(
    pp_data[pp_data$Outcome == "Stalemate", ],
    pp_data[pp_data$Outcome != "Stalemate", ]
)

tikz(file = file.path("..", "latex", "fig-oof-pred.tex"),
     width = 5.25,
     height = 6.75)
ggtern(pp_data,
       aes(x = VictoryA,
           y = Stalemate,
           z = VictoryB)) +
    geom_point(alpha = 0.3) +
    scale_T_continuous("$\\emptyset$") +
    scale_L_continuous("$A$") +
    scale_R_continuous("$B$") +
    facet_grid(Outcome ~ method) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_grey(base_size = 10) +
    theme(axis.tern.ticks = element_blank(),
          axis.tern.text = element_text(size = rel(0.8)),
          legend.position = "bottom")
dev.off()

## Versions for slides
tikz(file = file.path("..", "slides", "fig-oof-pred-capratio.tex"),
     width = 4.25,
     height = 3.25)
ggtern(pp_data %>% filter(method == "Ordered Logit on Capability Ratio"),
       aes(x = VictoryA,
           y = Stalemate,
           z = VictoryB)) +
    geom_point(alpha = 0.3) +
    scale_T_continuous("$\\emptyset$") +
    scale_L_continuous("$A$") +
    scale_R_continuous("$B$") +
    facet_wrap(~ Outcome, nrow = 2) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_grey(base_size = 8) +
    theme(axis.tern.ticks = element_blank(),
          axis.tern.text = element_text(size = rel(0.8)),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.position = "bottom")
dev.off()

tikz(file = file.path("..", "slides", "fig-oof-pred-sl.tex"),
     width = 4.25,
     height = 3.25)
ggtern(pp_data %>% filter(method == "Super Learner"),
       aes(x = VictoryA,
           y = Stalemate,
           z = VictoryB)) +
    geom_point(alpha = 0.3) +
    scale_T_continuous("$\\emptyset$") +
    scale_L_continuous("$A$") +
    scale_R_continuous("$B$") +
    facet_wrap(~ Outcome, nrow = 2) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme_grey(base_size = 8) +
    theme(axis.tern.ticks = element_blank(),
          axis.tern.text = element_text(size = rel(0.8)),
          plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA),
          legend.position = "bottom")
dev.off()

###-----------------------------------------------------------------------------
### Dyadic predictions over time
###-----------------------------------------------------------------------------

## Extract the USA-Russia dyadic data
##
## Using the undirected scores since we don't want to presume an initiator
pp_usa_russia_doe <- pred_dyad %>%
    filter(ccode_a == 2,
           ccode_b == 365) %>%
    mutate(capratio = log(capratio))

## Calculate predicted probabilities under the capability ratio model
##
## Need to do it twice to average, same way the undirected DOE scores are
## calculated
polr_capratio <- polr(Outcome ~ capratio,
                      data = dat)
pp_usa_russia_cr_a <- predict(polr_capratio,
                              newdata = pp_usa_russia_doe,
                              type = "prob")
pp_usa_russia_cr_b <- predict(polr_capratio,
                              newdata = pp_usa_russia_doe %>%
                                  mutate(capratio = log(1 - exp(capratio))),
                              type = "prob")
pp_usa_russia_cr <- 0.5 * pp_usa_russia_cr_a + 0.5 * pp_usa_russia_cr_b[, 3:1]
stopifnot(all.equal(range(rowSums(pp_usa_russia_cr)), c(1, 1)))

## Format data for ggplot
dat_usa_russia_doe <- pp_usa_russia_doe %>%
    select(year, VictoryUS = VictoryA, Stalemate, VictoryRussia = VictoryB) %>%
    mutate(method = "doe")
dat_usa_russia_cr <- pp_usa_russia_cr %>%
    data.frame() %>%
    select(VictoryUS = VictoryA, Stalemate, VictoryRussia = VictoryB) %>%
    mutate(year = pp_usa_russia_doe$year, method = "capratio")

dat_usa_russia <- rbind(dat_usa_russia_cr, dat_usa_russia_doe) %>%
    gather(quantity, probability, VictoryUS:VictoryRussia) %>%
    mutate(method = factor(method,
                           levels = c("capratio", "doe"),
                           labels = c("Ordered Logit on Capability Ratio",
                                      "Super Learner")),
           quantity = factor(quantity,
                             levels = c("VictoryUS", "Stalemate", "VictoryRussia"),
                             labels = c("USA Wins", "Stalemate", "Russia Wins")))

tikz(file = file.path("..", "latex", "fig-vs.tex"),
     width = 5,
     height = 3.25,
     packages = c(getOption("tikzLatexPackages"),
                  "\\usepackage{amsmath}"))
ggplot(dat_usa_russia, aes(x = year, y = probability)) +
    geom_area(aes(fill = quantity), alpha = 0.8) +
    facet_wrap(~ method) +
    scale_x_continuous("Year") +
    scale_y_continuous("Probability") +
    scale_fill_manual("",
                      values = rev(brewer.pal(3, "Blues"))) +
    theme_grey(base_size = 10) +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "bottom",
          legend.box = "horizontal")
dev.off()

## Versions for slides
tikz(file = file.path("..", "slides", "fig-usa-russia-capratio.tex"),
     width = 4.25,
     height = 3.25,
     packages = c(getOption("tikzLatexPackages"),
                  "\\usepackage{amsmath}"))
ggplot(dat_usa_russia %>% filter(method == "Ordered Logit on Capability Ratio"),
       aes(x = year, y = probability)) +
    geom_area(aes(fill = quantity), alpha = 0.8) +
    scale_x_continuous("Year") +
    scale_y_continuous("Predicted Probability") +
    scale_fill_manual("Outcome",
                      values = rev(brewer.pal(3, "Blues")),
                      guide = FALSE) +
    annotate("text", x = 1911.5, y = 0.04, label = "USA Wins", size = 2.5) +
    annotate("text", x = 1911.5, y = 0.5, label = "Stalemate", size = 2.5) +
    annotate("text", x = 1911.5, y = 0.96, label = "Russia Wins", size = 2.5) +
    ggtitle("Capability Ratio Model") +
    theme_grey(base_size = 8) +
    theme(axis.tern.ticks = element_blank(),
          axis.tern.text = element_text(size = rel(0.8)),
          plot.background = element_rect(fill = "transparent", colour = NA))
dev.off()

tikz(file = file.path("..", "slides", "fig-usa-russia-sl.tex"),
     width = 4.25,
     height = 3.25,
     packages = c(getOption("tikzLatexPackages"),
                  "\\usepackage{amsmath}"))
ggplot(dat_usa_russia %>% filter(method == "Super Learner"),
       aes(x = year, y = probability)) +
    geom_area(aes(fill = quantity), alpha = 0.8) +
    scale_x_continuous("Year") +
    scale_y_continuous("Predicted Probability") +
    scale_fill_manual("Outcome",
                      values = rev(brewer.pal(3, "Blues")),
                      guide = FALSE) +
    annotate("text", x = 1911.5, y = 0.04, label = "USA Wins", size = 2.5) +
    annotate("text", x = 1911.5, y = 0.5, label = "Stalemate", size = 2.5) +
    annotate("text", x = 1911.5, y = 0.96, label = "Russia Wins", size = 2.5) +
    ggtitle("DOE Scores") +
    theme_grey(base_size = 8) +
    theme(axis.tern.ticks = element_blank(),
          axis.tern.text = element_text(size = rel(0.8)),
          plot.background = element_rect(fill = "transparent", colour = NA))
dev.off()
