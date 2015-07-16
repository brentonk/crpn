################################################################################
###
### Replication of Fuhrmann & Sechser 2014, "Signalling Alliance Commitments," 
### replacing CINC ratio with DOE scores
###
### Replicating Table 2, Model 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("MASS")

data_fuhrmann_sechser_2014 <- read.dta("fuhrmann-sechser-2014.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## only keep the politically-relevant ones
data_fuhrmann_sechser_2014 <- data_fuhrmann_sechser_2014[
  data_fuhrmann_sechser_2014$politically_relevant == 1,]

## replication formula
f_fuhrmann_sechser_2014 <- 
  military_conflict ~ defense_pact_nuclear_cow + nuclear_deployment + 
  defense_pact_nonnuclear_cow + us_troops + challenger_nuclear_weapons + 
  target_nuclear_weapons + contiguity + alliance_with_challenger_cow + 
  foreign_policy_similarity + power_ratio + challenger_polity +
  target_polity + polity_interaction + time_conflict + time_conflict2 + 
  time_conflict3 

## replicate
reported_model <- glm(formula = f_fuhrmann_sechser_2014,
                      data = data_fuhrmann_sechser_2014,
                      family = binomial(link = "probit"))

## check
summary(reported_model)
                                    ## this is exact

## convert DV to factor
data_fuhrmann_sechser_2014$military_conflict <- 
    factor(data_fuhrmann_sechser_2014$military_conflict,
           levels = 0:1,
           labels = c("No", "Yes"))

## run the reported model with cross validation
set.seed(90210)
cr_fuhrmann_sechser_2014 <- train(
  f_fuhrmann_sechser_2014,
  data = data_fuhrmann_sechser_2014,
  method = "glm",
  metric = "logLoss",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 100,
    returnData = FALSE,
    summaryFunction = mnLogLoss,
    classProbs = TRUE,
    trim = TRUE
  ),
  family = binomial(link = "probit")
)

## clean out the big-N stuff
cr_fuhrmann_sechser_2014$control$index <- NULL
cr_fuhrmann_sechser_2014$control$indexOut <- NULL

prettyNum(coef(cr_fuhrmann_sechser_2014$finalModel))

## merge in DOE
data_fuhrmann_sechser_2014 <- left_join(data_fuhrmann_sechser_2014,
                                doe_dir_dyad,
                                by = c(state_a = "ccode_a",
                                       state_b = "ccode_b",
                                       year = "year"))

## every row has a DOE
length(which(is.na(data_fuhrmann_sechser_2014$VictoryA)))

## run it again with DOE
set.seed(8032)
doeForm <- update(f_fuhrmann_sechser_2014,
                  . ~ . - power_ratio + VictoryA + VictoryB)
doe_fuhrmann_sechser_2014 <-  train(
  doeForm,
  data = data_fuhrmann_sechser_2014,
  method = "glm",
  metric = "logLoss",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 100,
    returnData = FALSE,
    summaryFunction = mnLogLoss,
    classProbs = TRUE,
    trim = TRUE
  ),
  family = binomial(link = "probit")
)

## clean again
doe_fuhrmann_sechser_2014$control$index <- NULL
doe_fuhrmann_sechser_2014$control$indexOut <- NULL

prettyNum(coef(doe_fuhrmann_sechser_2014$finalModel))

## save it
save(cr_fuhrmann_sechser_2014,
     doe_fuhrmann_sechser_2014,
     data_fuhrmann_sechser_2014,
     file = "results-fuhrmann-sechser-2014.rda")
