################################################################################
###
### Replication of Weeks 2008, "Autocratic Audience Costs," replacing CINC ratio
### with DOE scores
###
### Replicating Model 3 (bilateral disputes only)
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

raw_weeks_2008 <- read.dta("weeks-2008.dta")
doe_dir_dyad <- read.csv("../R/results-predict-dir-dyad.csv")

## Transform data
##
## Same as in the .do file provided (and edited by Bryan)
data_weeks_2008 <- raw_weeks_2008 %>%
    mutate(recip = ifelse(cwhost2 > 1, 1, 0),
           recip = factor(
               recip,
               levels = 0:1,
               labels = c("No", "Yes")
           ),
           capshare1 = cap_1 / (cap_1 + cap_2),
           alliance = ifelse(alliance == -9, NA, alliance),
           ally = ifelse(alliance == 4, 0, 1),
           revtype = factor(
               cwrevt11,
               levels = 0:4,
               labels = c("None", "Territory", "Policy", "Government", "Other")
           ),
           contig = ifelse(contig < 6, 1, 0),
           bilateral = ifelse(cwnumst1 == 1 & cwnumst2 == 1, 1, 0),
           regimetype1 = factor(
               regimetype1,
               levels = 1:11,
               labels = c("Democracy",
                          "Personalist",
                          "Military",
                          "Single",
                          "Hybrid",
                          "Dynastic Monarchy",
                          "Non-dynastic Monarchy",
                          "Interregna",
                          "No Info",
                          "Other",
                          "Interregna-Dem")
           ))

## Remove cases dropped from analysis
data_weeks_2008 <- data_weeks_2008 %>%
    filter(year != 1947 | abbrev1 != "IND",
           year != 1965 | abbrev1 != "ZIM",
           year != 1969 | abbrev1 != "ZIM",
           year > 1945,
           year < 2000,
           bilateral == 1) %>%
    droplevels()                        # Prevent spurious singularity warnings

## Merge in DOE scores
data_weeks_2008 <- left_join(data_weeks_2008,
                             doe_dir_dyad,
                             by = c(ccodecow = "ccode_a",
                                    ccodecow2 = "ccode_b",
                                    year = "year"))

## Check for dyad-years covered by CINC but not DOE
data_weeks_2008 %>%
    filter(!is.na(capshare1) & is.na(VictoryA)) %>%
    select(ccodecow, ccodecow2, year)
                                        # None

## Replicate model 3 and estimate out-of-sample predictive power via repeated
## 10-fold cross-validation
set.seed(2308)
f_weeks_2008 <-
    recip ~ regimetype1 + majpow1*majpow2 + capshare1 + contig +
        ally + s_wt_glo + s_ld_1 + s_ld_2 + revtype
cr_weeks_2008 <- train(
    f_weeks_2008,
    data = data_weeks_2008,
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
cr_weeks_2008$control$index <- NULL
cr_weeks_2008$control$indexOut <- NULL

## Confirm that coefficients are the same as in the paper
prettyNum(coef(cr_weeks_2008$finalModel))

## Replace capability ratio with DOE scores
set.seed(8032)
doe_weeks_2008 <-  train(
    update(f_weeks_2008,
           . ~ . - capshare1 + VictoryA + VictoryB),
    data = data_weeks_2008,
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
doe_weeks_2008$control$index <- NULL
doe_weeks_2008$control$indexOut <- NULL

## Look at model results
prettyNum(coef(cr_weeks_2008$finalModel))

save(data_weeks_2008,
     cr_weeks_2008,
     doe_weeks_2008,
     file = "results-weeks-2008.rda")
