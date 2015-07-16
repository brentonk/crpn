################################################################################
###
### Replication of Uzonyi, Souva and Golder 2012, "Domestic Institutions and 
### Credible Signals," replacing CINC scores with DOE scores
###
### Replicating Table 3, Model 3 (Weeks and ACC in same model)
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

data_usg_2012 <- read.dta("uzonyi-souva-golder-2012.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## drop droppables
keeper <- data_usg_2012$year > 1945 & 
  data_usg_2012$year <= 2000 & 
  data_usg_2012$originator == 1
data_usg_2012 <- data_usg_2012[keeper,]
rm(keeper)

## replicate
f_usg_2012 <- 
  cowrecipx ~ acc1nomvx + personal1w + single1w + military1w + hybrid1w +
  other1w + dynastic1w + nondynastic1w + interregnaw + interregnademw + majmaj +
  minmaj + majmin + powerratio + allies + s_wt_glo + s_ld_1 + s_ld_2 + 
  contig400 + terr1 + regime1 + policy1 + other1 

reported_model <- glm(formula = f_usg_2012,
                      data = data_usg_2012,
                      family = binomial(link = "logit"))

summary(reported_model)
                                    ## exact

## convert DV to factor
data_usg_2012$cowrecipx <- factor(data_usg_2012$cowrecipx,
                                  levels = 0:1,
                                  labels = c("No", "Yes"))

## run it with CV
## run the reported model with cross validation
set.seed(90210)
cr_usg_2012 <- train(
  f_usg_2012,
  data = data_usg_2012,
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
cr_usg_2012$control$index <- NULL
cr_usg_2012$control$indexOut <- NULL

## see the model
prettyNum(coef(cr_usg_2012$finalModel))

## merge in the DOE data
data_usg_2012 <- left_join(data_usg_2012,
                                doe_dir_dyad,
                                by = c(ccode1 = "ccode_a",
                                       ccode2 = "ccode_b",
                                       year = "year"))

## DOE in every row
length(which(is.na(data_usg_2012$VictoryA)))

## run it with DOE
set.seed(8032)
doeForm <- update(f_usg_2012,
                  . ~ . - powerratio + VictoryA + VictoryB)
doe_usg_2012 <-  train(
  doeForm,
  data = data_usg_2012,
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
doe_usg_2012$control$index <- NULL
doe_usg_2012$control$indexOut <- NULL

prettyNum(coef(doe_usg_2012$finalModel))

save(cr_usg_2012,
     doe_usg_2012,
     data_usg_2012,
     file = "results-uzonyi-souva-golder-2012.rda")
