## Calculate out-of-sample class probabilities and predictions for each model

library("caret")

source("20-split-data.r")
source("40-load-results.r")

## Class probabilities for 3-class models
makeProbs <- function(fit)
    predict(fit, newdata = work_test, type = "prob")

probs_logit <- makeProbs(fit_logit)
probs_rf <- makeProbs(fit_rf)
probs_rf_time <- makeProbs(fit_rf_time)
probs_svm <- makeProbs(fit_svm)
probs_svm_time <- makeProbs(fit_svm_time)

## Class predictions for 3-class models
makePreds <- function(fit)
    predict(fit, newdata = work_test)

preds_logit <- makePreds(fit_logit)
preds_rf <- makePreds(fit_rf)
preds_rf_time <- makePreds(fit_rf_time)
preds_svm <- makePreds(fit_svm)
preds_svm_time <- makePreds(fit_svm_time)

## Class probabilities for 2-class models

## NOT WORKING NOW
## Need to use real factor names for VictoryA
## see <http://stackoverflow.com/questions/18402016/error-when-i-try-to-predict-class-probabilities-in-r-caret>

probs_logit_victory <- makeProbs(fit_logit_victory)
probs_rf_victory <- makeProbs(fit_rf_victory)
probs_rf_victory_time <- makeProbs(fit_rf_victory_time)
probs_svm_victory <- makeProbs(fit_svm_victory)
probs_svm_victory_time <- makeProbs(fit_svm_victory_time)

