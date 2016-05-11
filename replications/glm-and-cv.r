################################################################################
###
### Convenience function for saving pertinent quantities, including
### repeated cross-validation results, from a logit or probit
###
################################################################################

library("car")
library("caret")
library("multiwayvcov")
library("sandwich")

## ARGUMENTS:
##   form: model formula
##   data: model data
##   se_cluster:
##     NULL for ordinary SEs
##     1:nrow(data) for robust SEs
##     cluster IDs for clustered SEs
##   hyp_main: character string of main null hypothesis
##   hyp_power: character string of power null hypothesis
##   probit: use a probit instead of logit?
##   number: number of CV folds
##   repeats: number of times to repeat CV
glm_and_cv <- function(form,
                       data,
                       se_cluster = NULL,
                       hyp_main,
                       hyp_power,
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

    ## Calculate variance-covariance matrix
    if (is.null(se_cluster)) {
        vcv <- vcov(fit)
    } else {
        vcv <- cluster.vcov(fit, se_cluster)
    }

    ## Assemble regression table
    tab <- data.frame(term = names(coef(fit)),
                      estimate = coef(fit),
                      std.error = sqrt(diag(vcv)),
                      stringsAsFactors = FALSE)
    tab$statistic <- tab$estimate / tab$std.error
    tab$p.value <- 2 * pnorm(-abs(tab$statistic))
    rownames(tab) <- NULL

    ## Run hypothesis tests
    test_main <- linearHypothesis(fit,
                                  hyp_main,
                                  vcov = vcv)
    test_power <- linearHypothesis(fit,
                                   hyp_power,
                                   vcov = vcv)

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
    ##   * p-values from hypothesis tests
    ##   * Model formula
    ##   * Data used to fit the model
    ##   * Dependent variable in 0-1 format
    list(log_lik = log_lik,
         summary = tab,
         cv = cv$results,
         test_main = test_main,
         test_power = test_power,
         formula = form,
         y = fit$y)
}
