################################################################################
###
### Replication of Huth, Croco, Appel 2012, "Law and the Use of Force in World
### Politics," replacing CINC ratios with DOE scores
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("nnet")

raw_huth_2012 <- read.dta("huth-2012.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Convert response to factor and drop unused cases
data_huth_2012 <- raw_huth_2012 %>%
    mutate(dvsqb = factor(
               dvsqb,
               levels = 2:0,
               labels = c("Force",
                          "Negotiate",
                          "SQ")
           )) %>%
    filter(begmopog == 1)

## Convert date variable to year
##
## As coded currently, it represents months relative to January 1960, since
## midnight on 1960-01-01 is Stata's epoch
data_huth_2012 <- data_huth_2012 %>%
    mutate(year = date / 12,
           year = 1960 + year,
           year = floor(year))

## Merge in DOE scores
data_huth_2012 <- left_join(data_huth_2012,
                            doe_dir_dyad,
                            by = c(cowcntry = "ccode_a",
                                   opponent = "ccode_b",
                                   year = "year"))

## Check for dyad-years not covered by DOE (can happen when working with EUGene
## data since EUGene massages country codes)
data_huth_2012 %>%
    filter(is.na(VictoryA)) %>%
    select(cowcntry, opponent, year)

## Missing values:
## * Syria (652) vs Israel (666) in 1959-1960, part of a gap in Syria's
##   international system membership
## * Three observations involving Japan (740) in 1951, last year before its
##   re-entry to international system
## * North Korea (731) vs South Korea (732) in 1948, the year before the
##   latter's entry in international system
## * Austria (305) vs Italy (325) in 1945-1946, part of a gap in Austria's
##   international system membership
## * USA (2) vs East Germany (265) in 1948-1953, before East Germany's system
##   entry in 1954
##
## What we'll do is replicate the original model to make sure we have the
## specification right, then re-run it without these observations so that it's
## apples-to-apples when comparing with DOE scores

## Exact replication of original model
f_huth_2012 <- dvsqb ~ slc3b + demdum + milratio + alliance + strvalue +
                   ethvalue1 + endriv5b + sqtime1
orig_huth_2012 <- multinom(
    f_huth_2012,
    data = data_huth_2012,
    decay = 0,
    trace = FALSE
)
summary(orig_huth_2012)
                                        # Same as first two columns of Table 2

## Remove cases with missing DOE scores
data_huth_2012 <- data_huth_2012 %>%
    filter(!is.na(VictoryA))

## Analogue of glm_and_cv() for multinomial models
multinom_and_cv <- function(form,
                            data,
                            number = 10,
                            repeats = 10)
{
    ## Fit the original model
    fit <- multinom(formula = form,
                    data = data,
                    model = TRUE,
                    decay = 0,
                    trace = FALSE)

    ## Extract individual log-likelihoods
    pred_probs <- predict(fit, type = "probs")
    y <- model.response(model.frame(fit))
    y_col <- match(y, colnames(pred_probs))
    pred_probs <- pred_probs[cbind(seq_along(y), y_col)]
    log_lik <- log(pred_probs)

    ## Construct regression table
    summ_raw <- summary(fit)
    summ <-
        data_frame(est = c(t(summ_raw$coefficients)),
                   se = c(t(summ_raw$standard.errors)),
                   z = est/se,
                   p = 2 * pnorm(-abs(z))) %>%
        rename("Estimate" = est,
               "Std. Error" = se,
               "z value" = z,
               "Pr(>|z|)" = p)
    rownames(summ) <- paste(rep(rownames(summ_raw$coefficients),
                                each = ncol(summ_raw$coefficients)),
                            colnames(summ_raw$coefficients),
                            sep = ":")
    class(summ) <- "data.frame"

    ## Cross-validate via caret
    cv <- train(form = form,
                data = data,
                method = "multinom",
                preProcess = c("center", "scale"),
                metric = "logLoss",
                trControl = trainControl(
                    method = "repeatedcv",
                    number = number,
                    repeats = repeats,
                    summaryFunction = mnLogLoss,
                    classProbs = TRUE
                ),
                tuneGrid = data.frame(decay = 0),
                trace = FALSE)

    list(log_lik = log_lik,
         summary = summ,
         cv = cv$results,
         formula = form,
         data = fit$model,
         y = y)
}

## Re-run original model and cross-validate on reduced data
set.seed(812)
cr_huth_2012 <- multinom_and_cv(
    form = f_huth_2012,
    data = data_huth_2012,
    number = 10,
    repeats = 100
)
printCoefmat(cr_huth_2012$summary)

## Replace CINC ratio with DOE scores
set.seed(218)
doe_huth_2012 <- multinom_and_cv(
    form = update(f_huth_2012,
                  . ~ . - milratio + VictoryA + VictoryB),
    data = data_huth_2012,
    number = 10,
    repeats = 100
)
printCoefmat(doe_huth_2012$summary)

save(cr_huth_2012,
     doe_huth_2012,
     file = "results-huth-2012.rda")
