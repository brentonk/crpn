################################################################################
###
### Benchmark the model training
###
### Runs each model, including cross-validation, once on one imputation of the
### full training data.  We can then estimate the total procedure time for a
### single run as
###
###   imputations * (super learner CV folds + 1) * benchmark time
###
################################################################################

library("foreach")

source("10-fn-train.r")
source("11-defs-train.r")

load("results-imputations-train.rda")

## Fit each model to a full imputation of the training data, including
## cross-validation
##
## Using the full data rather than 9/10, and performing cross-validation even
## for models without tuning parameters, so that the resulting benchmark timings
## are conservative
fits <- args_to_train(arg_list = method_args,
                      common_args = common_args,
                      tr_control = tr_control,
                      data_train = imputations_train[[1]],
                      data_test = NULL,
                      for_probs = FALSE,
                      allow_no_tune = FALSE,
                      logfile = NULL)

## Extract time to run each model
timings <- foreach (fit = fits, .combine = "c") %do% {
    fit$times$everything[3] / 60
}

print(data.frame(model = c(names(fits), "TOTAL"),
                 minutes = c(timings, sum(timings))),
      digits = 2)

## Estimate total time for a single run
n_imputations <- length(imputations_train)
n_folds <- 10
cat("\nEstimated hours for single run:",
    n_imputations * (n_folds + 1) * sum(timings) / 60,
    "\n")
