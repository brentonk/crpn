################################################################################
###
### Functions to train a Super Learner on the imputed CINC data
###
################################################################################

library("caret")
library("dplyr")
library("foreach")
library("tidyr")

## Take a list of arguments and return either a list of trained models or a
## matrix of out-of-fold predicted probabilities
##
## Used in both the inner loop and the middle loop, which is why we need the
## flexibility in terms of the return value -- only need probabilities in the
## inner loop, but need the full model in the outer loop
args_to_train <- function(arg_list,
                          common_args,
                          tr_control = trainControl(),
                          data_train,
                          data_test = NULL,
                          id = NULL,
                          for_probs = FALSE,
                          allow_no_tune = TRUE,
                          logfile = NULL)
{
    ## Not allowing parallelization, since this function is always inside an
    ## imputation loop
    ans <- foreach (args = arg_list) %do% {
        ## Assemble arguments to pass to train()
        ##
        ## First element of each element of `args` must be `form`, containing
        ## the model formula
        train_args <- c(args,
                        common_args,
                        list(trControl = tr_control))
        train_args$data <- data_train

        ## Don't resample if the model doesn't need to be tuned
        ##
        ## Can be switched off (e.g., if we want CV estimates of accuracy for
        ## non-tuned models) via the `allow_no_tune` argument
        if (allow_no_tune && isTRUE(train_args$doNotTune)) {
            train_args$trControl$method <- "none"
        }
        train_args$doNotTune <- NULL

        ## Train the model
        fit <- do.call("train.formula", train_args)

        ## Remove cross-validation indices, which aren't needed for further
        ## prediction and may take up a lot of space
        fit$control$index <- NULL
        fit$control$indexOut <- NULL

        ## Calculate out-of-fold probabilities, if requested
        ##
        ## In this case, the probabilities will be returned as output, not the
        ## trained model
        if (for_probs) {
            pred <- predict(fit, newdata = data_test, type = "prob")

            ## Hack to normalize predicted probabilities from avNNet models --
            ## should be fixed in an upcoming version of caret
            ##
            ## See https://github.com/topepo/caret/issues/261
            if (fit$method == "avNNet") {
                pred <- pred / rowSums(pred)
            }

            ## Include observation IDs for mapping back into original dataset
            pred$id <- id

            fit <- pred
        }

        if (!is.null(logfile)) {
            cat(args$method, as.character(Sys.time()), "\n",
                file = logfile, append = TRUE)
        }

        fit
    }

    ## Process output
    names(ans) <- names(arg_list)

    ans
}

## Objective function for choosing weights on each model to minimize the
## log-loss of out-of-fold predicted probabilities
ll_ensemble_weights <- function(w, pr_mat)
{
    ## Compute the last weight -- not including in `w` because constrOptim()
    ## doesn't do equality constraints and must be initialized on the
    ## interior
    w <- c(w, 1 - sum(w))

    ## Calculate predicted probability of the observed outcome for each
    ## observation
    pred <- drop(pr_mat %*% w)

    ## Log-likelihood of each observation is log(w * p_i)
    ##
    ## Multiplied by -1 since constrOptim() minimizes, and divided by number
    ## of observations for consistency with mnLogLoss()
    -1 * sum(log(pred)) / length(pred)
}

## Derivative w.r.t. w_j is the sum of
##   (p_j - p_J) / (w * p_i)
## across observations i
grad_ensemble_weights <- function(w, pr_mat)
{
    ## Calculate each w * p_i
    w <- c(w, 1 - sum(w))
    pred <- drop(pr_mat %*% w)

    ## Subtract p_J from the preceding columns
    pr_mat <- sweep(pr_mat,
                    1,
                    pr_mat[, ncol(pr_mat)],
                    "-")
    pr_mat <- pr_mat[, -ncol(pr_mat)]

    ## Divide by each w * p_i
    pr_mat <- sweep(pr_mat,
                    1,
                    pred,
                    "/")

    ## Multiplied by -1 again since constrOptim() minimizes
    -1 * colSums(pr_mat) / length(pred)
}

## Learn optimal ensemble weights for a matrix of out-of-fold predicted
## probabilities of observed outcomes
learn_ensemble <- function(probs, outer.eps = 1e-8)
{
    ## Constraints for weight selection:
    ##   w_j >= 0 for all j
    ##   -w_1 - ... - w_(J-1) + 1 >= 0 [ensures w_J is positive]
    n_models <- ncol(probs)
    ui <- rbind(diag(n_models - 1),
                -1)
    ci <- c(rep(0, n_models - 1), -1)

    ## Perform constrained out-of-fold log-likelihood maximization
    ##
    ## Starting value gives equal weight to all models
    theta_start <- rep(1/n_models, n_models - 1)
    names(theta_start) <- head(colnames(probs), n_models - 1)
    ensemble_optim <- constrOptim(theta = theta_start,
                                  f = ll_ensemble_weights,
                                  grad = grad_ensemble_weights,
                                  ui = ui,
                                  ci = ci,
                                  outer.eps = outer.eps,
                                  pr_mat = probs)

    ensemble_optim
}

train_weights <- function(dat,
                          n_folds,
                          arg_list,
                          common_args,
                          tr_control)
{
    ## Generate cross-validation folds
    ##
    ## We're going to train each model *within* each fold as well, in order to
    ## get the out-of-sample probabilities we need to learn the ensemble weights
    cv_folds <- createFolds(y = dat$Outcome,
                            k = n_folds,
                            list = TRUE)

    ## Calculate out-of-fold predicted probabilities for each candidate model
    all_probs <- foreach (fold = seq_len(n_folds)) %do% {
        cat("FOLD", fold, as.character(Sys.time()), "\n")

        ## Retrieve the training and test sets corresponding to the given fold
        fold_train <- dat[-cv_folds[[fold]], , drop = FALSE]
        fold_test <- dat[cv_folds[[fold]], , drop = FALSE]

        ## Train each specified model and calculate the predicted probability of
        ## each out-of-fold outcome
        fold_probs <- args_to_train(arg_list = arg_list,
                                    common_args = common_args,
                                    tr_control = tr_control,
                                    data_train = fold_train,
                                    data_test = fold_test,
                                    id = cv_folds[[fold]],
                                    for_probs = TRUE,
                                    allow_no_tune = TRUE,
                                    logfile = "")

        fold_probs
    }

    ## Collapse the out-of-fold predictions from each model into a single data
    ## frame, each with observations in the same order as the original data
    ##
    ## The result is a list of said data frames, the same length as the number
    ## of models being trained
    all_probs_collapse <- foreach (model = seq_along(arg_list)) %do% {
        ## Retrieve relevant components of `all_probs`
        oof_preds <- foreach (fold = seq_len(n_folds), .combine = "rbind") %do% {
            all_probs[[fold]][[model]]
        }

        ## Place in order of original data using the id variable
        oof_preds <- oof_preds[order(oof_preds$id), ]

        oof_preds
    }
    names(all_probs_collapse) <- names(arg_list)

    ## Form a matrix of out-of-fold probabilities of *observed* outcomes
    obs_probs <- foreach (model = all_probs_collapse, .combine = "cbind") %do% {
        model$obs <- dat$Outcome

        ## Translate relevant element of `all_probs_collapse` into key-value
        ## format
        pred <- gather_(model,
                        key_col = "Outcome",
                        value_col = "prob",
                        gather_cols = levels(dat$Outcome))

        ## Extract the predicted values for the observed outcomes only
        pred <- filter(pred,
                       as.character(obs) == as.character(Outcome))

        ## Once again place in original order
        pred <- pred[order(pred$id), ]

        pred$prob
    }
    colnames(obs_probs) <- names(all_probs_collapse)

    ## Calculate optimal ensemble weights
    imputation_weights <- learn_ensemble(obs_probs)

    ## To estimate the bias of the minimum CV error via Tibshirani &
    ## Tibshirani's method, calculate the difference in CV error on each fold
    ## for the chosen weights versus the weights that would be optimal for that
    ## fold alone
    losses <- foreach (fold = seq_len(n_folds), .combine = "rbind") %do% {
        ## Subset the matrix of out-of-fold probabilities of observed outcomes
        ## to the relevant fold only
        obs_probs_fold <- obs_probs[cv_folds[[fold]], , drop = FALSE]

        ## Log loss using the weights chosen on the full dataset
        loss_global <- ll_ensemble_weights(w = imputation_weights$par,
                                           pr_mat = obs_probs_fold)

        ## Log loss using the weights that would be optimal for this fold alone
        loss_local <- learn_ensemble(obs_probs_fold)$value

        c(global = loss_global,
          local = loss_local)
    }
    bias_min_cv <- mean(losses[, "global"] - losses[, "local"])

    list(out_probs = obs_probs,
         weights = imputation_weights,
         bias_min_cv = bias_min_cv,
         all_probs = all_probs_collapse)
}
