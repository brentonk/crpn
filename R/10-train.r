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
library("dplyr")
library("foreach")
library("tidyr")

load("results-imputations-train.rda")

###-----------------------------------------------------------------------------
### Set up list of models to train
###-----------------------------------------------------------------------------

## Log-likelihood metric for multinomial models
##
## See <http://topepo.github.io/caret/training.html#metrics>
loglik_metric <- function(data, lev = NULL, model = NULL)
{
    ## Retrieve the estimated probability of the observed outcome for each
    ## observation
    prob_observed <- lapply(lev, function(outcome) {
        ## Set of observations ending in the specified level
        idx_outcome <- data$obs == outcome

        ## Estimated probabilities are a column in the data frame whose name is
        ## the specified level
        data[idx_outcome, outcome]
    })

    ## Metric is the average of the log-probabilities
    ##
    ## Doing the average instead of the sum since some of the folds may not have
    ## the same sample size
    ll <- mean(log(unlist(prob_observed)))

    ## Also retrieve the standard metrics
    standard_metrics <- defaultSummary(data, lev, model)
    
    c(standard_metrics,
      "logLik" = ll)
}

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
    returnData = FALSE,
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
### Tune ensemble weights via k-fold cross-validation
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
        fold_probs <- foreach (args = method_args, .combine = "cbind") %do% {
            ## Assemble arguments to pass to train()
            train_args <- c(args,
                            common_args,
                            list(trControl = tr_control))
            train_args$data <- fold_train

            ## Don't use cross-validation if `doNotTune` is indicated (i.e., for
            ## models with no tuning parameters)
            if (isTRUE(train_args$doNotTune)) {
                train_args$doNotTune <- NULL
                train_args$trControl$method <- "none"
            }

            ## Train the model
            fit <- do.call(train.formula, train_args)

            ## Calculate out-of-fold predicted probabilities
            pred <- predict(fit, newdata = fold_test, type = "prob")

            ## `predict()` gives us three columns of output, one per class.  We
            ## want this to be in key-value format instead, to ease the process
            ## of backing out the estimated probability of the *observed*
            ## outcome.
            pred$obs <- fold_test$Outcome
            pred <- gather_(pred,
                            key_col = "Outcome",
                            value_col = "prob",
                            gather_cols = levels(pred$obs))

            ## Extract the predicted values for the observed outcomes
            pred <- filter(pred,
                           as.character(obs) == as.character(Outcome))

            pred$prob
        }

        colnames(fold_probs) <- names(method_args)
        fold_probs
    }

    ## TODO: fit each model to full sample

    ## Objective function for choosing weights on each model to maximize the
    ## log-likelihood of out-of-fold predicted probabilities
    ll_ensemble_weights <- function(w, probs)
    {
        ## Compute the last weight -- not including in `w` because constrOptim()
        ## doesn't do equality constraints and must be initialized on the
        ## interior
        w <- c(w, 1 - sum(w))

        ## Multiply each column by its corresponding weight
        probs <- t(t(probs) * w)

        ## Log-likelihood of each observation is log(w * p_i)
        ##
        ## Multiplied by -1 since constrOptim() minimizes
        -1 * sum(log(rowSums(probs)))
    }

    ## Derivative w.r.t. w_j is the sum of
    ##   (p_j - p_J) / (w * p_i)
    ## across observations i
    grad_ensemble_weights <- function(w, probs)
    {
        ## Calculate each w * p_i
        w <- c(w, 1 - sum(w))
        pred <- drop(probs %*% w)

        ## Subtract p_J from the preceding columns
        probs <- sweep(probs,
                       1,
                       probs[, ncol(probs)],
                       "-")
        probs <- probs[, -ncol(probs)]

        ## Divide by each w * p_i
        probs <- sweep(probs,
                       1,
                       pred,
                       "/")

        ## Multiplied by -1 again since constrOptim() minimizes
        -1 * colSums(probs)
    }

    ## Constraints for weight selection:
    ##   w_j >= 0 for all j
    ##   -w_1 - ... - w_(J-1) + 1 >= 0 [ensures w_J is positive]
    n_models <- ncol(out_probs)
    ui <- rbind(diag(n_models - 1),
                -1)
    ci <- c(rep(0, n_models - 1), -1)

    ## TODO: Calculate out-of-fold log-likelihood for each model on its own

    ## Perform constrained out-of-fold log-likelihood maximization
    theta_start <- rep(1/n_models, n_models - 1)
    names(theta_start) <- head(colnames(out_probs), n_models - 1)
    ensemble_optim <- constrOptim(theta = theta_start,
                                  f = ll_ensemble_weights,
                                  grad = grad_ensemble_weights,
                                  ui = ui,
                                  ci = ci,
                                  probs = out_probs)

    ensemble_optim
}
