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
                          for_probs = FALSE,
                          allow_no_tune = TRUE)
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

        ## Calculate out-of-fold probabilities, if requested
        ##
        ## In this case, the probabilities will be returned as output, not the
        ## trained model
        if (for_probs) {
            pred <- predict(fit, newdata = data_test, type = "prob")

            ## `predict()` gives us three columns of output, one per class.  We
            ## want this to be in key-value format instead, to ease the process
            ## of backing out the estimated probability of the *observed*
            ## outcome.
            pred$obs <- data_test$Outcome
            pred <- gather_(pred,
                            key_col = "Outcome",
                            value_col = "prob",
                            gather_cols = levels(pred$obs))

            ## Extract the predicted values for the observed outcomes
            pred <- filter(pred,
                           as.character(obs) == as.character(Outcome))

            fit <- pred$prob
        }

        fit
    }

    ## Process output
    if (for_probs) {
        ans <- do.call("cbind", ans)
        colnames(ans) <- names(arg_list)
    } else {
        names(ans) <- names(arg_list)
    }

    ans
}

## Learn optimal ensemble weights for a matrix of out-of-fold predicted
## probabilities of observed outcomes
learn_ensemble <- function(probs, outer.eps = 1e-8)
{
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
