################################################################################
###
### Convenience function for saving pertinent quantities, including
### repeated cross-validation results, from a logit or probit
###
################################################################################

library("caret")

## ARGUMENTS:
##   form: model formula
##   data: model data
##   probit: use a probit instead of logit?
##   number: number of CV folds
##   repeats: number of times to repeat CV
glm_and_cv <- function(form,
                       data,
                       probit = FALSE,
                       number = 10,
                       repeats = 10)
{
    fit_family <- binomial(link = if (probit) "probit" else "logit")

    ## Fit the original model
    fit <- glm(formula = form,
               data = data,
               family = fit_family)

    ## Extract individual log-likelihoods
    pred_probs <- predict(fit, type = "response")
    pred_probs <- ifelse(fit$y == 1, pred_probs, 1 - pred_probs)
    log_lik <- log(pred_probs)

    ## Cross-validate via caret
    cv <- train(form = form,
                data = data,
                method = "glm",
                metric = "logLoss",
                trControl = trainControl(
                    method = "repeatedcv",
                    number = number,
                    repeats = repeats,
                    returnData = FALSE,
                    summaryFunction = mnLogLoss,
                    classProbs = TRUE,
                    trim = TRUE
                ),
                family = fit_family)

    ## Only return pertinent information:
    ##   * Individual log-likelihoods
    ##   * Model results (regression table)
    ##   * Cross-validation results (estimated out-of-sample log loss)
    ##   * Model formula
    ##   * Data used to fit the model
    ##   * Dependent variable in 0-1 format
    list(log_lik = log_lik,
         summary = coef(summary(fit)),
         cv = cv$results,
         formula = form,
         data = fit$model,
         y = fit$y)
}
