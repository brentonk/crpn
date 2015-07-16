################################################################################
###
### Replication of Weeks 2012, "Strongmen and Straw Men," replacing CINC ratios
### with DOE scores
###
### Replicating Table 1, Model 2
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

raw_weeks_2012 <- read.dta("weeks-2012.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Convert response to factor and drop unused cases (those with a missing value
## for country 1's democracy score)
data_weeks_2012 <- raw_weeks_2012 %>%
    mutate(mzinit = factor(
               mzinit,
               levels = 0:1,
               labels = c("No", "Yes")
           )) %>%
    filter(!is.na(democracy_1))

## Merge in DOE scores
data_weeks_2012 <- left_join(data_weeks_2012,
                             doe_dir_dyad,
                             by = c(ccode1 = "ccode_a",
                                    ccode2 = "ccode_b",
                                    year = "year"))
stopifnot(with(data_weeks_2012, sum(is.na(VictoryA)) == 0))

## Replicate original model and cross-validate
set.seed(2312)
f_weeks_2012 <- 
    mzinit ~ machinejlw_1 + juntajlw_1 + bossjlw_1 + strongmanjlw_1 +
        allotherauts_1 + newregime_1 + democracy_2 + cap_1 + cap_2 + initshare +
        dependlow + majmaj + minmaj + majmin + contigdum + logdist + s_wt_glo +
        s_lead_1 + s_lead_2 + pcyrsmzinit + pcyrsmzinits1 + pcyrsmzinits2 +
        pcyrsmzinits3
cr_weeks_2012 <- train(
    form = f_weeks_2012,
    data = data_weeks_2012,
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
cr_weeks_2012$control$index <- NULL
cr_weeks_2012$control$indexOut <- NULL

## Make sure results replicated
prettyNum(coef(cr_weeks_2012$finalModel))

## Replicate, replacing all functions of the CINC score (the two raw scores and
## the ratio) with DOE scores
set.seed(2132)
doe_weeks_2012 <- train(
    form = update(f_weeks_2012,
                  . ~ . - cap_1 - cap_2 - initshare + VictoryA + VictoryB),
    data = data_weeks_2012,
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
doe_weeks_2012$control$index <- NULL
doe_weeks_2012$control$indexOut <- NULL

## Look at results
prettyNum(coef(doe_weeks_2012$finalModel))

save(data_weeks_2012,
     cr_weeks_2012,
     doe_weeks_2012,
     file = "results-weeks-2012.rda")
