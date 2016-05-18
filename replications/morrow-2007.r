################################################################################
###
### Replication of Morrow 2007, "When Do States Follow the Laws of War?,"
### replacing CINC ratio with DOE scores
###
### Replicating Table 1, Column 1 (unweighted)
###
################################################################################

library("car")
library("caret")
library("dplyr")
library("foreign")
library("MASS")

## custom ordered probit function for caret call
source("ordered-probit.r")

raw_morrow_2007 <- read.dta("morrow-2007.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")


###-----------------------------------------------------------------------------
### Helper functions
###-----------------------------------------------------------------------------

## Analogue of glm_and_cv() for polr models
polr_and_cv <- function(form,
                        data,
                        hyp_main,
                        hyp_power,
                        number = 10,
                        repeats = 10)
{
    ## Fit the original model
    fit <- polr(formula = form,
                data = data,
                method = "probit",
                model = TRUE,
                Hess = TRUE)

    ## Extract individual log-likelihoods
    pred_probs <- predict(fit, type = "probs")
    y <- model.response(model.frame(fit))
    y_col <- match(y, colnames(pred_probs))
    pred_probs <- pred_probs[cbind(seq_along(y), y_col)]
    log_lik <- log(pred_probs)

    ## Make coefficient table
    tab <- data.frame(term = c(names(fit$coefficients),
                               names(fit$zeta)),
                      estimate = c(fit$coefficients, fit$zeta),
                      std.error = sqrt(diag(vcov(fit))),
                      stringsAsFactors = FALSE)
    tab$statistic <- tab$estimate / tab$std.error
    tab$p.value <- 2 * pnorm(-abs(tab$statistic))
    rownames(tab) <- NULL

    ## Test hypotheses
    test_main <- linearHypothesis(fit, hyp_main)
    test_power <- linearHypothesis(fit, hyp_power)

    ## Cross-validate via caret
    cv <- train(form = form,
                data = data,
                method = caret_oprobit,
                metric = "logLoss",
                trControl = trainControl(
                    method = "repeatedcv",
                    number = number,
                    repeats = repeats,
                    returnData = FALSE,
                    summaryFunction = mnLogLoss,
                    classProbs = TRUE)
                )

    list(log_lik = log_lik,
         summary = tab,
         cv = cv$results,
         test_main = test_main,
         test_power = test_power,
         formula = form,
         y = y)
}


###-----------------------------------------------------------------------------
### Analysis
###-----------------------------------------------------------------------------

## remove cases dropped in the analysis
data_morrow_2007 <- raw_morrow_2007 %>%
    filter(!is.na(data_quality),
           data_quality > 0,
           WarDeclaration == 0)

## turn DV into a factor for polr
data_morrow_2007$factorDV <- factor(data_morrow_2007$violator_4_ordinal_comply,
                                    levels = 1:4,
                                    labels = c("None", "Low", "Medium", "High"),
                                    ordered = TRUE)

## replication on the full data to establish the result.
f_morrow_2007 <-
    factorDV ~ victim_4_ordinal_comply + victim_clar_4_ordinal_comply +
        joint_ratify_4_ordinal_recip + joint_ratify_clar_recip +
        victim_individual_comply + victim_state_comply +   joint_ratify +
        violator_ratified + violator_democracy7 +   joint_ratify_democracy7 +
        violator_ratified_democracy7 + powerratio +   joint_ratify_power + Aerial +
        Armistice + CBW + Civilians + Cultural +   HighSeas + POWs +
        violator_initiate + violator_deathsper1000population +   victim_victor +
        victim_victor_power

## check that results are identical to reported model
reported_model <- polr(formula = f_morrow_2007,
                       data = data_morrow_2007,
                       method = "probit")
summary(reported_model)

## drop observations for which we do not have DOE scores (coalitions)
data_morrow_2007 <- data_morrow_2007 %>%
    filter(violatorccode < 1000,
           victimccode < 1000)
nrow(data_morrow_2007)

## merge in the DOE data
data_morrow_2007 <- left_join(data_morrow_2007,
                              doe_dir_dyad,
                              by = c(violatorccode = "ccode_a",
                                     victimccode = "ccode_b",
                                     startyear = "year"))

## check the merge
stopifnot(with(data_morrow_2007, !any(is.na(VictoryA))))

## Null hypothesis: no effect of reciprocal variables
hyp_main <- c("victim_4_ordinal_comply = 0",
              "victim_clar_4_ordinal_comply = 0",
              "joint_ratify_4_ordinal_recip = 0",
              "joint_ratify_clar_recip = 0",
              "victim_individual_comply = 0",
              "victim_state_comply = 0")

## re-run the replication on these smaller data
set.seed(8675309)
cr_morrow_2007 <- polr_and_cv(
    form = f_morrow_2007,
    data = data_morrow_2007,
    hyp_main = hyp_main,
    hyp_power = c("powerratio = 0",
                  "joint_ratify_power = 0",
                  "victim_victor_power = 0"),
    number = 10,
    repeats = 100
)
print(cr_morrow_2007$summary)

## now run it with the DOE scores, including both interactions
doe_morrow_2007 <- polr_and_cv(
    form = update(f_morrow_2007,
                  . ~ . - powerratio - joint_ratify_power -
                      victim_victor_power + (VictoryA + VictoryB) *
                      (joint_ratify + victim_victor)),
    data = data_morrow_2007,
    hyp_main = hyp_main,
    hyp_power = c("VictoryA = 0",
                  "VictoryB = 0",
                  "joint_ratify:VictoryA = 0",
                  "victim_victor:VictoryA = 0",
                  "joint_ratify:VictoryB = 0",
                  "victim_victor:VictoryB = 0"),
    number = 10,
    repeats = 100
)
print(doe_morrow_2007$summary)

save(cr_morrow_2007,
     doe_morrow_2007,
     file = "results-morrow-2007.rda")
