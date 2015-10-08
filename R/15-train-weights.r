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

## Load plyr before dplyr -- forcing this here since some of the ML packages
## call plyr
library("plyr")
library("dplyr")

## Analysis packages
library("caret")
library("foreach")
library("iterators")
library("methods")

source("10-fn-train.r")
source("11-defs-train.r")

load("results-imputations-train.rda")


###-----------------------------------------------------------------------------
### Read in imputation to calculate weights for
###
### This assumes the script is called like
###   Rscript 15-train-weights.r i
### where i is the index of the imputation
###-----------------------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
    i <- as.integer(args[1])
} else {
    stop("No imputation specified at command line")
}


###-----------------------------------------------------------------------------
### Calculate out-of-fold predictions and corresponding ensemble weights for the
### given imputation
###-----------------------------------------------------------------------------

dat <- imputations_train[[i]]

time_start <- proc.time()

## We want the seed to be deterministic (for replicability) but to differ across
## imputations
set.seed(100 * i)

## Generate cross-validation folds
##
## We're going to train each model *within* each fold as well, in order to
## get the out-of-sample probabilities we need to learn the ensemble weights
n_folds <- 10
cv_folds <- createFolds(y = dat$Outcome,
                        k = n_folds,
                        list = TRUE,
                        returnTrain = TRUE)

out_probs <- foreach (fold = seq_len(n_folds)) %do% {
    cat("FOLD", fold, as.character(Sys.time()), "\n")

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
                                allow_no_tune = TRUE,
                                logfile = "")

    fold_probs
}

## Calculate optimal ensemble weights
out_mat <- do.call("rbind", out_probs)
imputation_weights <- learn_ensemble(out_mat)

## To estimate the bias of the minimum CV error via Tibshirani &
## Tibshirani's method, calculate the difference in CV error on each fold
## for the chosen weights versus the weights that would be optimal for that
## fold alone
loss_global <- sapply(out_probs,
                      function(x) ll_ensemble_weights(w = imputation_weights$par,
                                                      pr_mat = x))
loss_local <- sapply(out_probs,
                     function(x) learn_ensemble(x)$value)
bias_min_cv <- mean(loss_global - loss_local)

time_end <- proc.time()
print(time_end - time_start)


## Collect the key components of the results as a list
##
## Technically no longer necessary now that we're saving separate .rda's for
## each imputation, but will help reduce rewriting of older code
trained_weights <- list(out_probs = out_mat,
                        weights = imputation_weights,
                        bias_min_cv = bias_min_cv)

save_dir <- "results-weights"
save_file <- paste0("imp",
                    sprintf("%.2d", i),
                    ".rda")

if (!dir.exists(save_dir))
    dir.create(save_dir)
save(trained_weights,
     file = file.path(save_dir, save_file))
