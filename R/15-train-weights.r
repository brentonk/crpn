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
library("tidyr")

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

trained_weights <- train_weights(dat = dat,
                                 n_folds = 10,
                                 arg_list = method_args,
                                 common_args = common_args,
                                 tr_control = tr_control)

time_end <- proc.time()
print(time_end - time_start)

save_dir <- "results-weights"
save_file <- paste0("imp",
                    sprintf("%.2d", i),
                    ".rda")

if (!dir.exists(save_dir))
    dir.create(save_dir)
save(trained_weights,
     file = file.path(save_dir, save_file))
