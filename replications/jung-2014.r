################################################################################
###
### Replication of Jung 2014, "Foreign Targets and Diversionary Conflict,"
### replacing CINC ratios with DOE scores
###
### Replicating Table 1, Model 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

raw_jung_2014 <- read.dta("jung-2014.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Convert outcome to factor
data_jung_2014 <- raw_jung_2014 %>%
    mutate(initiate = factor(
               initiate,
               levels = 0:1,
               labels = c("No", "Yes")
           ))

## Merge in DOE data
##
## Year of capability ratios is lagged, so want to lag DOE scores too
data_jung_2014 <- left_join(data_jung_2014,
                            doe_dir_dyad %>% mutate(year = year + 1),
                            by = c(ccode1 = "ccode_a",
                                   ccode2 = "ccode_b",
                                   year = "year"))
stopifnot(with(data_jung_2014, sum(is.na(VictoryA) & !is.na(caprat_1)) == 0))

## Reproduce original model and cross-validate
f_jung_2014 <-
    initiate ~ dom_powtg + dom_identg + dom_tertg + dom_hegtg + dom + powtg +
        identg + tertg + hegtg + caprat_1 + cowally_1 + depend1_1 + depend2_1 +
        jointdemo_1 + jointminor_1 + border + lndistance + mid1peace + mid1sp1 +
        mid1sp2 + mid1sp3
cr_jung_2014 <- train(
    form = f_jung_2014,
    data = data_jung_2014,
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
cr_jung_2014$control$index <- NULL
cr_jung_2014$control$indexOut <- NULL

prettyNum(coef(cr_jung_2014$finalModel))

## Replicate, replacing CINC ratio with DOE scores
doe_jung_2014 <- train(
    form = update(f_jung_2014,
                  . ~ . - caprat_1 + VictoryA + VictoryB),
    data = data_jung_2014,
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
doe_jung_2014$control$index <- NULL
doe_jung_2014$control$indexOut <- NULL

prettyNum(coef(doe_jung_2014$finalModel))

save(data_jung_2014,
     cr_jung_2014,
     doe_jung_2014,
     file = "results-jung-2014.rda")
