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
library("MASS")                         # polr
library("C50")                          # C5.0
library("kernlab")                      # svm
library("rpart")                        # rpart
library("randomForest")                 # rf
library("nnet")                         # nnet

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
f_time_trend <- Outcome ~ I(year > 1945) * poly(year, 3)

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
    ## Ordered logit just on time trend
    polr_time_trend = list(
        form = f_time_trend,
        method = "polr",
        doNotTune = TRUE
    ),
    ## Ordered logits
    polr = list(
        form = f_components,
        method = "polr",
        doNotTune = TRUE
    ),
    polr_t = list(
        form = f_components_times_t,
        method = "polr",
        doNotTune = TRUE
    ),
    polr_props = list(
        form = f_props,
        method = "polr",
        doNotTune = TRUE
    ),
    polr_props_t = list(
        form = f_props_times_t,
        method = "polr",
        doNotTune = TRUE
    ),
    ## C5.0
    c5 = list(
        form = f_components,
        method = "C5.0",
        tuneLength = 6
    ),
    c5_t = list(
        form = f_components_t,
        method = "C5.0",
        tuneLength = 6
    ),
    c5_props = list(
        form = f_props,
        method = "C5.0",
        tuneLength = 6
    ),
    c5_props_t = list(
        form = f_props_t,
        method = "C5.0",
        tuneLength = 6
    ),
    ## Support vector machine with radial basis kernel
    svm = list(
        form = f_components,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        tuneGrid = expand.grid(
            sigma = seq(0.2, 1, by = 0.2),
            C = 2^seq(-2, 2)
        )
    ),
    svm_t = list(
        form = f_components_t,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        tuneGrid = expand.grid(
            sigma = seq(0.2, 1, by = 0.2),
            C = 2^seq(-2, 2)
        )
    ),
    svm_props = list(
        form = f_props,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        tuneGrid = expand.grid(
            sigma = seq(0.2, 1, by = 0.2),
            C = 2^seq(-2, 2)
        )
    ),
    svm_props_t = list(
        form = f_props_t,
        method = "svmRadial",
        preProcess = c("center", "scale"),
        tuneGrid = expand.grid(
            sigma = seq(0.2, 1, by = 0.2),
            C = 2^seq(-2, 2)
        )
    ),
    ## k-nearest neighbors
    knn = list(
        form = f_components,
        method = "knn",
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(
            k = seq(25, 250, by = 25)
        )
    ),
    knn_t = list(
        form = f_components_t,
        method = "knn",
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(
            k = seq(25, 250, by = 25)
        )
    ),
    knn_props = list(
        form = f_props,
        method = "knn",
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(
            k = seq(25, 250, by = 25)
        )
    ),
    knn_props_t = list(
        form = f_props_t,
        method = "knn",
        preProcess = c("center", "scale"),
        tuneGrid = data.frame(
            k = seq(25, 250, by = 25)
        )
    ),
    ## CART (tuning over maximum tree depth)
    cart = list(
        form = f_components,
        method = "rpart",
        tuneLength = 25
    ),
    cart_t = list(
        form = f_components_t,
        method = "rpart",
        tuneLength = 25
    ),
    cart_props = list(
        form = f_props,
        method = "rpart",
        tuneLength = 25
    ),
    cart_props_t = list(
        form = f_props_t,
        method = "rpart",
        tuneLength = 25
    ),
    ## Random forests
    rf = list(
        form = f_components,
        method = "rf",
        tuneGrid = data.frame(
            mtry = seq(2, 12, by = 2)
        ),
        ntree = 1000
    ),
    rf_t = list(
        form = f_components_t,
        method = "rf",
        tuneGrid = data.frame(
            mtry = seq(2, 12, by = 2)
        ),
        ntree = 1000
    ),
    rf_props = list(
        form = f_props,
        method = "rf",
        tuneGrid = data.frame(
            mtry = seq(2, 12, by = 2)
        ),
        ntree = 1000
    ),
    rf_props_t = list(
        form = f_props_t,
        method = "rf",
        tuneGrid = data.frame(
            mtry = seq(2, 12, by = 2)
        ),
        ntree = 1000
    ),
    ## Neural network ensemble
    avnnet = list(
        form = f_components,
        method = "avNNet",
        tuneLength = 5,
        maxit = 1000,
        repeats = 10,
        trace = FALSE,
        allowParallel = FALSE
    ),
    avnnet_t = list(
        form = f_components_t,
        method = "avNNet",
        tuneLength = 5,
        maxit = 1000,
        repeats = 10,
        trace = FALSE,
        allowParallel = FALSE
    ),
    avnnet_props = list(
        form = f_props,
        method = "avNNet",
        tuneLength = 5,
        maxit = 1000,
        repeats = 10,
        trace = FALSE,
        allowParallel = FALSE
    ),
    avnnet_props_t = list(
        form = f_props_t,
        method = "avNNet",
        tuneLength = 5,
        maxit = 1000,
        repeats = 10,
        trace = FALSE,
        allowParallel = FALSE
    )
)
