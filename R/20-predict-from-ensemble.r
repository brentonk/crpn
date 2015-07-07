################################################################################
###
### Function to calculate predicted outcome probabilities for out-of-sample data
### from a fitted ensemble model
###
################################################################################

library("caret")
library("foreach")
library("iterators")

predict_from_ensemble <- function(dat, ensemble)
{
    ## Loop through each of the 10 imputed ensembles
    ans <- foreach (fit = ensemble, i = icount()) %do% {
        ## Calculate predicted probabilities from each individual model
        pred <- foreach (model = fit$models, j = icount()) %do% {
            predict(model, newdata = dat, type = "prob")
        }

        ## Multiply each set of predicted probabilities by the corresponding
        ## ensemble weight
        wt <- fit$weights$par
        wt <- c(wt, 1 - sum(wt))
        pred <- Map("*", pred, wt)

        ## Sum the weighted probabilities
        pred <- Reduce("+", pred)

        cat("Finished imputation", i, date(), "\n")

        pred
    }

    ## Average across the imputations
    ans <- Reduce("+", ans)
    ans <- ans / length(ensemble)

    ans
}
