################################################################################
###
### Replication of Bennett 2006, "Toward a Continuous Specification...," 
### replacing CINC ratio with DOE scores
###
### Replicating Table 1, Column 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("MASS")

data_bennett_2006 <- read.dta("bennett-2006.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## drop observations with ongoing war
data_bennett_2006 <- data_bennett_2006[data_bennett_2006$cwongo != 1,]

## construct some variables, as per Bennett's .do file
data_bennett_2006$demint1orig <- data_bennett_2006$demint1
data_bennett_2006$demint2orig <- data_bennett_2006$demint2
data_bennett_2006$demi1simorig <- data_bennett_2006$demi1sim
data_bennett_2006$di1di2orig <- data_bennett_2006$di1di2
data_bennett_2006$d1d2isqrorig <- data_bennett_2006$d1d2isqr

data_bennett_2006$demint1 <- data_bennett_2006$polity21
data_bennett_2006$demint2 <- data_bennett_2006$polity22
data_bennett_2006$demi1sim <- data_bennett_2006$demi1simpol
data_bennett_2006$di1di2 <- data_bennett_2006$di1di2pol
data_bennett_2006$d1d2isqr <- data_bennett_2006$d1d2isqrpol

## replicate Bennett's table 1
f_bennett_2006 <- 
  cwinit ~ demint1 + demint2 + demi1sim + di1di2 + d1d2isqr + cap_1 +   cap_2 +
  lndist + anypact + priordisputeproportion + parity + cwpceyrs + cwpceyr1 +
  cwpceyr2 + cwpceyr3
reported_model <- glm(formula = f_bennett_2006,
                      data = data_bennett_2006,
                      family = binomial(link = "logit"))
summary(reported_model)
                                      ### identical results


## make a factor DV
data_bennett_2006$cwinit <- factor(data_bennett_2006$cwinit,
                                   levels = 0:1,
                                   labels = c("No", "Yes"))

## run the reported model with cross validation
set.seed(90210)
cr_bennett_2006 <- train(
  f_bennett_2006,
  data = data_bennett_2006,
  method = "glm",
  metric = "logLoss",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10,
    returnData = FALSE,
    summaryFunction = mnLogLoss,
    classProbs = TRUE,
    trim = TRUE
  )
)

## Don't save cross-validation indices (takes tons of space with large N)
cr_bennett_2006$control$index <- NULL
cr_bennett_2006$control$indexOut <- NULL

prettyNum(coef(cr_bennett_2006$finalModel))

## merge in the DOE scores
data_bennett_2006 <- left_join(data_bennett_2006,
                                doe_dir_dyad,
                                by = c(ccode1 = "ccode_a",
                                       ccode2 = "ccode_b",
                                       year = "year"))

## check the merge
summary(!is.na(data_bennett_2006$parity) & is.na(data_bennett_2006$VictoryA))

## run it with DOE
doeForm <- update(f_bennett_2006,
                  . ~ . - parity - cap_1 - cap_2 + VictoryA + VictoryB)
doe_bennett_2006 <- train(
  doeForm,
  data = data_bennett_2006,
  method = "glm",
  metric = "logLoss",
  trControl = trainControl(
    method = "repeatedcv",
    number = 10,
    repeats = 10,
    returnData = FALSE,
    summaryFunction = mnLogLoss,
    classProbs = TRUE,
    trim = TRUE
  )
)

## Don't save cross-validation indices (takes tons of space with large N)
doe_bennett_2006$control$index <- NULL
doe_bennett_2006$control$indexOut <- NULL

prettyNum(coef(doe_bennett_2006$finalModel))

save(data_bennett_2006,
     cr_bennett_2006,
     doe_bennett_2006,
     file = "results-bennett-2006.rda")
