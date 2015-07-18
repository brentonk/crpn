################################################################################
###
### Functions for collecting replication results
###
################################################################################

library("caret")
library("foreach")

## Calculate the (in-sample) AIC
aic <- function(fit) {
    ll <- sum(fit$log_lik)
    n_param <- nrow(fit$summary)
    2 * (n_param - ll)
}

## Jackknife estimate of null model out-of-sample loss
cv_null <- function(fit) {
    obs <- table(fit$y)
    loss <- foreach (j = seq_along(obs), .combine = "c") %do% {
        obs_jack <- obs
        obs_jack[j] <- obs_jack[j] - 1  # "Drop" observation
        pred_jack <- obs_jack / sum(obs_jack)
        log(pred_jack[j])
    }

    -1 * sum(obs * loss) / sum(obs)
}

## Calculate the proportional reduction in out-of-sample loss
prl <- function(fit) {
    loss_null <- cv_null(fit)
    (loss_null - fit$cv$logLoss) / loss_null
}

## Calculate the Vuong test statistic for Model 1 - Model 2
vuong <- function(fit1, fit2) {
    if (!identical(as.vector(fit1$y), as.vector(fit2$y)))
        stop("Models must have same dependent variable")

    ## Calculate overall log-likelihoods
    ll1 <- sum(fit1$log_lik)
    ll2 <- sum(fit2$log_lik)

    ## Calculate Schwarz/BIC correction
    k1 <- nrow(fit1$summary)
    k2 <- nrow(fit2$summary)
    n <- length(fit1$y)
    bic_correction <- 0.5 * (k1 - k2) * log(n)

    ## Vuong test statistic numerator
    num <- (ll1 - ll2) - bic_correction

    ## Vuong test statistic denominator: standard deviation of differences in
    ## observation-wise log-likelihoods, scaled by root-n
    denom <- sd(fit1$log_lik - fit2$log_lik) * sqrt(n)

    num / denom
}
