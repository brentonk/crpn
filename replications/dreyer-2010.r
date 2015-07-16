################################################################################
###
### Replication of Dreyer 2010, "Issue Conflict Accumulation and the Dynamics of
### Strategic Rivalry," replacing CINC ratios with DOE scores
###
### Replicating Table 2, Model 2
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

raw_dreyer_2010 <- read.dta("dreyer-2010.dta")
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## Convert response to factor
data_dreyer_2010 <- raw_dreyer_2010 %>%
    mutate(mid = factor(
               mid,
               levels = 0:1,
               labels = c("No", "Yes")
           ))

## Merge in DOE scores and compute max and min (since undirected)
data_dreyer_2010 <- data_dreyer_2010 %>%
    left_join(doe_dyad,
              by = c(ccode1 = "ccode_a",
                     ccode2 = "ccode_b",
                     year = "year")) %>%
    mutate(VictoryMax = pmax(VictoryA, VictoryB),
           VictoryMin = pmin(VictoryA, VictoryB))
stopifnot(with(data_dreyer_2010, sum(is.na(VictoryA)) == 0))

## Reproduce original model
set.seed(410)
f_dreyer_2010 <-
    mid ~ rap_iss_accum + grad_iss_accum + cap_rat + maj_power + alliance +
        democ + contig + spline_1 + spline_2 + spline_3 + spline_4
cr_dreyer_2010 <- train(
    form = f_dreyer_2010,
    data = data_dreyer_2010,
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
    )    
)

## Don't save cross-validation indices (takes tons of space with large N)
cr_dreyer_2010$control$index <- NULL
cr_dreyer_2010$control$indexOut <- NULL

prettyNum(coef(cr_dreyer_2010$finalModel))

## Replicate, replacing CINC scores with DOE scores
##
## Logging DOE scores for consistency with original analysis
set.seed(14)
doe_dreyer_2010 <- train(
    form = update(f_dreyer_2010,
                  . ~ . - cap_rat + log(VictoryMax) + log(VictoryMin)),
    data = data_dreyer_2010,
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
    )    
)

## Don't save cross-validation indices (takes tons of space with large N)
doe_dreyer_2010$control$index <- NULL
doe_dreyer_2010$control$indexOut <- NULL

prettyNum(coef(doe_dreyer_2010$finalModel))

save(data_dreyer_2010,
     cr_dreyer_2010,
     doe_dreyer_2010,
     file = "results-dreyer-2010.rda")