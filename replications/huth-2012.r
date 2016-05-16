################################################################################
###
### Replication of Huth, Croco, Appel 2012, "Law and the Use of Force in World
### Politics," replacing CINC ratios with DOE scores
###
################################################################################

library("car")
library("caret")
library("dplyr")
library("foreign")
library("mlogit")
library("nnet")
library("sandwich")

raw_huth_2012 <- read.dta("huth-2012.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")


###-----------------------------------------------------------------------------
### Helper functions
###-----------------------------------------------------------------------------

## Clustered standard errors for multinomial logistic regression models fit via
## nnet::mlogit()
##
## Original code by Mahmood Ara and Justin Esarey
## See https://thepoliticalmethodologist.com/2015/03/03/some-code-for-estimating-clustered-ses-in-mlogit-models/
cluster_vcov_mlogit <- function(fm, cluster)
{
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- length(coefficients(fm))
    dfc <- M/(M-1)
    uj  <- apply(estfun(fm), 2, function(x) tapply(x, cluster, sum))
    vcovCL <- dfc * sandwich(fm, meat. = crossprod(uj) / N)
    vcovCL
}

## Analogue of glm_and_cv() for multinomial models
##
## Uses mlogit() for main fitting and hypothesis testing since it plays nicely
## with the clustered SE function above, but multinom() for cross-validation
## since it's baked into caret -- shouldn't be an issue since the two give the
## same results
##
## Formula should be in the form:
##   y ~ 0 | x1 + x2 + ...
multinom_and_cv <- function(form,
                            data,
                            se_cluster = NULL,
                            hyp_main,
                            hyp_power,
                            number = 10,
                            repeats = 10)
{
    ## Fit the original model
    fit <- mlogit(formula = form,
                  data = data,
                  shape = "wide")

    ## Extract individual log-likelihoods
    log_lik <- log(fit$fitted.values)

    ## Calculate variance-covariance matrix
    if (is.null(se_cluster)) {
        vcv <- vcov(fit)
    } else {
        vcv <- cluster_vcov_mlogit(fit, se_cluster)
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

    ## Collapse formula into something caret can use
    form_collapse <- formula(Formula(form), rhs = 2)

    ## Cross-validate via caret
    cv <- train(form = form_collapse,
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
         summary = tab,
         cv = cv$results,
         test_main = test_main,
         test_power = test_power,
         formula = form,
         y = cv$trainingData$.outcome)
}


###-----------------------------------------------------------------------------
### Analysis
###-----------------------------------------------------------------------------

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

## Remove observations not included in the original analysis
##
## Doing this explicitly since mlogit() appears not to return the na.action
## object, making it hard to work with missingness
data_huth_2012 <- data_huth_2012 %>%
    filter(!is.na(dvsqb),
           !is.na(slc3b),
           !is.na(demdum),
           !is.na(milratio),
           !is.na(alliance),
           !is.na(strvalue),
           !is.na(ethvalue1),
           !is.na(ethvalue1),
           !is.na(endriv5b),
           !is.na(sqtime1))
nrow(data_huth_2012)                    # 3840

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
    select(cowcntry, opponent, year) %>%
    arrange(cowcntry, opponent, year)

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
f_huth_2012 <- dvsqb ~ 0 | slc3b + demdum + milratio + alliance + strvalue +
                   ethvalue1 + endriv5b + sqtime1
orig_huth_2012 <- mlogit(
    f_huth_2012,
    data = data_huth_2012,
    shape = "wide"
)
summary(orig_huth_2012)
                                        # Same as first two columns of Table 2

## Clustered SEs
sqrt(diag(cluster_vcov_mlogit(orig_huth_2012, data_huth_2012$dispno2)))

## Remove cases with missing DOE scores
data_huth_2012 <- data_huth_2012 %>%
    filter(!is.na(VictoryA))

## Null hypothesis: no effect of strong legal claims
hyp_main <- c("Negotiate:slc3b = 0", "SQ:slc3b = 0")

## Re-run original model and cross-validate on reduced data
set.seed(812)
cr_huth_2012 <- multinom_and_cv(
    form = f_huth_2012,
    data = data_huth_2012,
    se_cluster = data_huth_2012$dispno2,
    hyp_main = hyp_main,
    hyp_power = c("Negotiate:milratio = 0", "SQ:milratio = 0"),
    number = 10,
    repeats = 100
)
print(cr_huth_2012$summary)

## Replace CINC ratio with DOE scores
f_doe_huth_2012 <- dvsqb ~ 0 | slc3b + demdum + VictoryA + VictoryB +
    alliance + strvalue + ethvalue1 + endriv5b + sqtime1
doe_huth_2012 <- multinom_and_cv(
    form = f_doe_huth_2012,
    data = data_huth_2012,
    se_cluster = data_huth_2012$dispno2,
    hyp_main = hyp_main,
    hyp_power = c("Negotiate:VictoryA = 0", "Negotiate:VictoryB = 0",
                  "SQ:VictoryA = 0", "SQ:VictoryB = 0"),
    number = 10,
    repeats = 100
)
print(doe_huth_2012$summary)

save(cr_huth_2012,
     doe_huth_2012,
     file = "results-huth-2012.rda")
