################################################################################
###
### Train a Super Learner on the imputed CINC data
###
### Basic algorithm:
### 1. For each imputation m = 1, ..., M
###   a. Assign each data point to a fold k = 1, ..., K
###   b. For each fold k = 1, ..., K:
###     i. Train a bunch of models on the data excluding fold k (this may
###        entail another layer of cross-validation, for models with tuning
###        parameters)
###     ii. Calculate each model's predicted probabilities for the observations
###         in fold k
###   c. Use the out-of-fold predictions to determine optimal weights on each
###      model, via a log-likelihood criterion
###   d. Train all the models on the full data
###   e. Final model for imputation m is a weighted average of the models, where
###      the weights are those from step (c)
### 2. Final model overall is the average of the M models fit to each imputed
###    dataset
###
################################################################################

library("caret")
library("foreach")

## ML packages
library("MASS")                         # polr

source("10-fn-train.r")

load("results-imputations-train.rda")


###-----------------------------------------------------------------------------
### Set up list of models to train
###-----------------------------------------------------------------------------

## train() arguments to pass to every model
common_args <- list(
    metric = "logLik",
    maximize = TRUE
)

## Control parameters for tuning via cross-validation
##
## Passed to every train() call except those for which `doNotTune` is indicated
## (and TRUE)
tr_control <- trainControl(
    method = "cv",
    number = 10,
    ## returnData = FALSE,
    classProbs = TRUE,
    summaryFunction = loglik_metric,
    allowParallel = FALSE
)

## Different model formulas
##
## `caret` tends to prefer working with matrices instead of formulas, but the
## issue here is that the model matrix may vary across models.  That'll be
## annoying to deal with when calculating out-of-sample predictions if we're
## supplying the matrix directly.
f_capratio <- Outcome ~ capratio
f_components <- Outcome ~ irst_a + milex_a + milper_a + pec_a + tpop_a +
    upop_a + irst_b + milex_b + milper_b + pec_b + tpop_b + upop_b
f_components_times_t <- update(f_components,
                               . ~ . * year)

## List of method-specific arguments that vary across train() calls
##
## Each must include "form", the relevant model formula
method_args <- list(
    ## Ordered logit just on the capability ratio
    polr_capratio = list(
        form = f_capratio,
        method = "polr",
        doNotTune = TRUE
    ),
    ## Ordered logit on the capability index components
    polr_components = list(
        form = f_components,
        method = "polr",
        doNotTune = TRUE
    ),
    ## Ordered logit on the capability index components, interacted with time
    polr_components_times_t = list(
        form = f_components_times_t,
        method = "polr",
        doNotTune = TRUE
    )
)


###-----------------------------------------------------------------------------
### Perform training
###-----------------------------------------------------------------------------

## TODO: set seed, %dorng%, parallelize
full_ensemble <- foreach (dat = imputations_train) %do% {
    ## Cross-validation folds for the middle loop
    ##
    ## We're going to train each model *within* each fold as well, in order to
    ## get the out-of-sample probabilities we need to learn the ensemble weights
    n_folds <- 10
    cv_folds <- createFolds(y = dat$Outcome,
                            k = n_folds,
                            list = TRUE,
                            returnTrain = TRUE)

    out_probs <- foreach (fold = seq_len(n_folds), .combine = "rbind") %do% {
        ## Retrieve the training and test sets corresponding to the given fold
        fold_train <- dat[cv_folds[[fold]], , drop = FALSE]
        fold_test <- dat[-cv_folds[[fold]], , drop = FALSE]

        ## Train each specified model and calculate the predicted probability of
        ## each out-of-fold outcome
        fold_probs <- args_to_train(arg_list = method_args,
                                    common_args = common_args,
                                    tr_control = tr_control,
                                    data_train = fold_train,
                                    data_test = fold_test,
                                    for_probs = TRUE,
                                    allow_no_tune = TRUE)

        fold_probs
    }

    ## Calculate optimal ensemble weights
    learn_ensemble(out_probs)
}
