################################################################################
###
### Replication of Zawahri and Mitchell 2011, "Fragmented Governance of
### International Rivers," replacing CINC scores with DOE scores
###
### Replicating Table 2, Model 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

raw_zawahri_mitchell_2011 <- read.dta("zawahri-mitchell-2011.dta")
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## Convert outcome to factor and drop multilateral basins
data_zawahri_mitchell_2011 <- raw_zawahri_mitchell_2011 %>%
    mutate(bilattreaty = factor(
               bilattreaty,
               levels = 0:1,
               labels = c("No", "Yes")
           )) %>%
    filter(bilatbasin == 1)

## Merge in DOE scores
##
## `upstreamstate` has three codings: A, B, and AB; we have upstreampower =
## cinc1 if upstreamstate = A or AB, and upstreampower = cinc2 otherwise
data_zawahri_mitchell_2011 <- data_zawahri_mitchell_2011 %>%
    left_join(doe_dyad,
              by = c(ccode1 = "ccode_a",
                     ccode2 = "ccode_b",
                     year = "year")) %>%
    mutate(VictoryUp = ifelse(upstreamstate == "B", VictoryB, VictoryA),
           VictoryDown = ifelse(upstreamstate == "B", VictoryA, VictoryB))
stopifnot(with(data_zawahri_mitchell_2011, sum(is.na(VictoryUp)) == 0))

## Reproduce original model
set.seed(2611)
f_zawahri_mitchell_2011 <-
    bilattreaty ~ lowpolity + samelegal + downstreampower + upstreampower +
        percentarealow + waterdependlow + avgpreciplow
cr_zawahri_mitchell_2011 <- train(
    form = f_zawahri_mitchell_2011,
    data = data_zawahri_mitchell_2011,
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
cr_zawahri_mitchell_2011$control$index <- NULL
cr_zawahri_mitchell_2011$control$indexOut <- NULL

prettyNum(coef(cr_zawahri_mitchell_2011$finalModel))

## Replace raw CINC scores with corresponding DOE scores and run again
set.seed(1162)
doe_zawahri_mitchell_2011 <- train(
    form = update(f_zawahri_mitchell_2011,
                  . ~ . - downstreampower - upstreampower + VictoryUp + VictoryDown),
    data = data_zawahri_mitchell_2011,
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
doe_zawahri_mitchell_2011$control$index <- NULL
doe_zawahri_mitchell_2011$control$indexOut <- NULL

prettyNum(coef(doe_zawahri_mitchell_2011$finalModel))

save(data_zawahri_mitchell_2011,
     cr_zawahri_mitchell_2011,
     doe_zawahri_mitchell_2011,
     file = "results-zawahri-mitchell.rda")
