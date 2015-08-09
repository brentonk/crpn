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
library("iterators")
library("methods")

source("10-fn-train.r")
source("11-defs-train.r")

load("results-imputations-train.rda")


time_start <- proc.time()

set.seed(1980)                          # For exact replicability

full_ensemble <- foreach (dat = imputations_train, i = icount(), .packages = c("caret", "tidyr", "dplyr")) %dorng% {
    ## Separate logs across each node
    logfile <- paste0("logs/",
                      "imp",
                      sprintf("%.2d", i),
                      ".log")

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
        cat("FOLD", fold, as.character(Sys.time()), "\n",
            file = logfile, append = TRUE)

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
                                    logfile = logfile)

        fold_probs
    }

    ## Calculate optimal ensemble weights
    imputation_weights <- learn_ensemble(out_probs)

    ## Train each model on the full imputed dataset
    cat("FINAL FIT", as.character(Sys.time()), "\n",
        file = logfile, append = TRUE)
    imputation_models <- args_to_train(arg_list = method_args,
                                       common_args = common_args,
                                       tr_control = tr_control,
                                       data_train = dat,
                                       data_test = NULL,
                                       for_probs = FALSE,
                                       allow_no_tune = FALSE,
                                       logfile = logfile)

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
