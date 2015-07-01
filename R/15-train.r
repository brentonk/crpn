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

sessionInfo()

## Set up cluster
##
## For local use, comment out this block and the next one
library("Rmpi", quietly = TRUE)
library("doMPI", quietly = TRUE)
cl <- startMPIcluster()
registerDoMPI(cl)

## Set up error handling with Rmpi
##
## Without this code, the MPI session may fail to shut down properly in case an
## error occurs, and then the script won't terminate until it hits the walltime
options(error=quote(assign(".mpi.err", FALSE, env = .GlobalEnv)))

## Analysis packages
library("caret")
library("foreach")
library("doRNG")
library("methods")

## ML packages
library("kernlab")                      # svm, gausspr
library("MASS")                         # polr
library("nnet")                         # nnet
library("randomForest")                 # rf

source("10-fn-train.r")

load("results-imputations-train.rda")


###-----------------------------------------------------------------------------
### Set up list of models to train
###-----------------------------------------------------------------------------

## train() arguments to pass to every model
common_args <- list(
    metric = "logLoss",
    maximize = FALSE
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
    summaryFunction = mnLogLoss,
    trim = TRUE,
    allowParallel = FALSE
)

## Different model formulas
##
## `caret` tends to prefer working with matrices instead of formulas, but the
## issue here is that the model matrix may vary across models.  That'll be
## annoying to deal with when calculating out-of-sample predictions if we're
## supplying the matrix directly.
f_capratio <- Outcome ~ capratio
f_null <- Outcome ~ 1
f_components <-
    Outcome ~ irst_a + milex_a + milper_a + pec_a + tpop_a + upop_a +
        irst_b + milex_b + milper_b + pec_b + tpop_b + upop_b
f_props <-
    Outcome ~ prop_irst_a + prop_milex_a + prop_milper_a + prop_pec_a +
        prop_tpop_a + prop_upop_a + prop_irst_b + prop_milex_b + prop_milper_b +
        prop_pec_b + prop_tpop_b + prop_upop_b
f_components_t <- update(f_components, . ~ . + year)
f_props_t <- update(f_props, . ~ . + year)
f_components_times_t <- update(f_components, . ~ . * year)
f_props_times_t <- update(f_props, . ~ . * year)

## List of method-specific arguments that vary across train() calls
##
## Each must include "form", the relevant model formula
method_args <- list(
    ## Null model
    null = list(
        form = f_null,
        method = "polr",
        doNotTune = TRUE
    ),
    ## Ordered logit just on the capability ratio
    polr_capratio = list(
        form = f_capratio,
        method = "polr",
        doNotTune = TRUE
    ),
    ## Ordered logit on the capability index components
    polr = list(
        form = f_components,
        method = "polr",
        doNotTune = TRUE
    ),
    ## Ordered logit on the capability index components, interacted with time
    polr_t = list(
        form = f_components_times_t,
        method = "polr",
        doNotTune = TRUE
    ),
    ## Ordered logit on capability proportions
    polr_props = list(
        form = f_props,
        method = "polr",
        doNotTune = TRUE
    ),
    ## Ordered logit on capability proportions, interacted with time
    polr_props_t = list(
        form = f_props_times_t,
        method = "polr",
        doNotTune = TRUE
    ),
    ## k-nearest neighbors on the capability index components
    knn = list(
        form = f_components,
        method = "knn",
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(
            k = seq(25, 250, by = 25)
        )
    ),
    ## k-nearest neighbors on the capability index components and time
    knn_t = list(
        form = f_components_t,
        method = "knn",
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(
            k = seq(25, 250, by = 25)
        )
    ),
    ## k-nearest neighbors on the capability proportions
    knn_props = list(
        form = f_props,
        method = "knn",
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(
            k = seq(25, 250, by = 25)
        )
    ),
    ## k-nearest neighbors on the capability proportions and time
    knn_props_t = list(
        form = f_props_t,
        method = "knn",
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(
            k = seq(25, 250, by = 25)
        )
    ),
    ## Random forest on the capability index components
    rf = list(
        form = f_components,
        method = "rf",
        tuneGrid = data.frame(
            mtry = seq(2, 12, by = 2)
        ),
        ntree = 1000
    ),
    ## Random forest on the capability index components and time
    rf_t = list(
        form = f_components_t,
        method = "rf",
        tuneGrid = data.frame(
            mtry = c(2, seq(3, 13, by = 2))
        ),
        ntree = 1000
    ),
    ## Random forest on the capability proportions
    rf_props = list(
        form = f_props,
        method = "rf",
        tuneGrid = data.frame(
            mtry = seq(2, 12, by = 2)
        ),
        ntree = 1000
    ),
    ## Random forest on the capability proportions and time
    rf_props_t = list(
        form = f_props_t,
        method = "rf",
        tuneGrid = data.frame(
            mtry = c(2, seq(3, 13, by = 2))
        ),
        ntree = 1000
    ),
    ## Neural network on the capability index components
    nnet = list(
        form = f_components,
        method = "nnet",
        tuneLength = 5,
        maxit = 1000,
        trace = FALSE
    ),
    ## Neural network on the capability index components and time
    nnet_t = list(
        form = f_components_t,
        method = "nnet",
        tuneLength = 5,
        maxit = 1000,
        trace = FALSE
    ),
    ## Neural network on the capability proportions
    nnet_props = list(
        form = f_props,
        method = "nnet",
        tuneLength = 5,
        maxit = 1000,
        trace = FALSE
    ),
    ## Neural network on the capability proportions and time
    nnet_props_t = list(
        form = f_props_t,
        method = "nnet",
        tuneLength = 5,
        maxit = 1000,
        trace = FALSE
    ),
    ## Gaussian process with radial basis function kernel on the capability
    ## index components
    gpr = list(
        form = f_components,
        method = "gaussprRadial",
        preProcess = c("center", "scale")
    ),
    ## Gaussian process with radial basis function kernel on the capability
    ## index components and time
    gpr_t = list(
        form = f_components_t,
        method = "gaussprRadial",
        preProcess = c("center", "scale")
    ),
    ## Gaussian process with radial basis function kernel on the capability
    ## proportions
    gpr_props = list(
        form = f_props,
        method = "gaussprRadial",
        preProcess = c("center", "scale")
    ),
    ## Gaussian process with radial basis function kernel on the capability
    ## proportions and time
    gpr_props_t = list(
        form = f_props_t,
        method = "gaussprRadial",
        preProcess = c("center", "scale")
    ),
    ## Support vector machine with radial basis function kernel on the
    ## capability index components
    svm = list(
        form = f_components,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        tuneLength = 15
    ),
    ## Support vector machine with radial basis function kernel on the
    ## capability index components and time
    svm_t = list(
        form = f_components_t,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        tuneLength = 15
    ),
    ## Support vector machine with radial basis function kernel on the
    ## capability proportions
    svm_props = list(
        form = f_props,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        tuneLength = 15
    ),
    ## Support vector machine with radial basis function kernel on the
    ## capability index components
    svm_props_t = list(
        form = f_props_t,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        tuneLength = 15
    )
)

## Approximate timings per fold, in seconds:
timings <- c(
    null          = 1,
    polr_capratio = 1,
    polr          = 2,
    polr_t        = 4,
    polr_props    = 2,
    polr_props_t  = 4,
    knn           = 4,
    knn_t         = 4,
    knn_props     = 5,
    knn_props_t   = 5,
    rf            = 112,
    rf_t          = 142,
    rf_props      = 129,
    rf_props_t    = 146,
    nnet          = 90,
    nnet_t        = 54,
    nnet_props    = 106,
    nnet_props_t  = 48,
    gpr           = 122,
    gpr_t         = 102,
    gpr_props     = 488,
    gpr_props_t   = 454,
    svm           = 48,
    svm_t         = 49,
    svm_props     = 73,
    svm_props_t   = 76
)

## Estimated total hours: about 70
##
## Relatedly, estimated size of output is about 650 MB
length(imputations_train) * (10 + 1) * sum(timings) / 3600

time_start <- proc.time()

set.seed(1980)                          # For exact replicability

full_ensemble <- foreach (dat = imputations_train, .packages = c("caret")) %dorng% {
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
    imputation_weights <- learn_ensemble(out_probs)

    ## Train each model on the full imputed dataset
    imputation_models <- args_to_train(arg_list = method_args,
                                       common_args = common_args,
                                       tr_control = tr_control,
                                       data_train = dat,
                                       data_test = NULL,
                                       for_probs = FALSE,
                                       allow_no_tune = FALSE)

    list(models = imputation_models,
         out_probs = out_probs,
         weights = imputation_weights)
}

time_end <- proc.time()
print(time_end - time_start)

save(full_ensemble,
     file = "results-full-ensemble.rda")

closeCluster(cl)
mpi.quit()
