################################################################################
###
### Replication of Park and Colaresi 2013, "Safe Across the Borders," 
### replacing CINC ratio with DOE scores
###
### Replicating Table 1, Model 2
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("MASS")

load("park-colaresi-2014.RData")
data_park_colaresi_2014 <- data1.subset
rm(data1.subset, data1.subset.nomiss)
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## replicate their results
f_park_colaresi_2014 <- 
  cwmid ~ lowdem + lowgdp + landcontig + nlparity + beckpeace + cvwonset + 
  dyaddur + samecolony + ethborder + terrsim + landcontig*nlparity + 
  landcontig*beckpeace + landcontig*cvwonset + landcontig*dyaddur + 
  landcontig*samecolony + landcontig*ethborder + landcontig*terrsim + 
  .spline1 + .spline2 +.spline3

reported_model <- glm(formula = f_park_colaresi_2014,
                      data = data_park_colaresi_2014,
                      family = binomial(link = "logit"))

summary(reported_model)
                              ## exact.  also large.  so:
rm(reported_model)

## run with cross-validation
data_park_colaresi_2014$cwmid <- factor(data_park_colaresi_2014$cwmid,
                                        levels = 0:1,
                                        labels = c("No", "Yes"))

set.seed(90210)
cr_park_colaresi_2014 <- train(
  f_park_colaresi_2014,
  data = data_park_colaresi_2014,
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
cr_park_colaresi_2014$control$index <- NULL
cr_park_colaresi_2014$control$indexOut <- NULL

prettyNum(coef(cr_park_colaresi_2014$finalModel))

## merge in DOE scores
data_park_colaresi_2014 <- left_join(data_park_colaresi_2014,
                                doe_dyad,
                                by = c(ccode1 = "ccode_a",
                                       ccode2 = "ccode_b",
                                       year = "year"))

## every row has its DOE
length(which(is.na(data_park_colaresi_2014$VictoryA)))

## they use a parity variable (0 is no parity, 1 is perfect parity).  
data_park_colaresi_2014$doeParity <- abs(data_park_colaresi_2014$VictoryA - 
                                           data_park_colaresi_2014$VictoryB)

doeForm <- update(f_park_colaresi_2014,
                  . ~ . - nlparity + doeParity - landcontig * nlparity + 
                    landcontig * doeParity)
doe_park_colaresi_2014 <-  train(
  doeForm,
  data = data_park_colaresi_2014,
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
doe_park_colaresi_2014$control$index <- NULL
doe_park_colaresi_2014$control$indexOut <- NULL

prettyNum(coef(doe_park_colaresi_2014$finalModel))

save(cr_park_colaresi_2014,
     doe_park_colaresi_2014,
     data_park_colaresi_2014,
     file = "results-park-colaresi-2014.rda")
