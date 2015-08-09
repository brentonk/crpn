################################################################################
###
### Model arguments for Super Learner training
###
### Placing these in a separate file from the main trainer so we can have a
### separate benchmark script
###
################################################################################

library("caret")

## ML packages
library("kernlab")                      # svm, gausspr
library("MASS")                         # polr
library("nnet")                         # nnet
library("randomForest")                 # rf

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
