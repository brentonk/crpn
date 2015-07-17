################################################################################
###
### Replication of Salehyan 2008, "No Shelter Here," 
### replacing CINC ratio with DOE scores
###
### Replicating Table 1, Column 1
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("MASS")

source("glm-and-cv.r")

data_salehyan_2008 <- read.dta("salehyan-2008.dta")
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## add the correct dependent variable
data_salehyan_2008$DV <- ifelse(data_salehyan_2008$cwhostd >= 4, 1, 0)

## switch DV to factor for caret
data_salehyan_2008$DV <- factor(data_salehyan_2008$DV,
                                levels = 0:1,
                                labels = c("No", "Yes"))

## replication formula
f_salehyan_2008 <- 
  DV ~ extbase + intriv + lcaprat + defpact + politylo + peaceyears + 
  s1 + s2 + s3

## run the reported model with cross validation
set.seed(90210)
cr_salehyan_2008 <- glm_and_cv(
  form = f_salehyan_2008,
  data = data_salehyan_2008,
  number = 10,
  repeats = 100
)
printCoefmat(cr_salehyan_2008$summary)

## merge in the DOE scores and compute pairwise max and min (since A and B are
## exchangeable in undirected data)
data_salehyan_2008 <- left_join(data_salehyan_2008,
                                doe_dyad,
                                by = c(ccode1 = "ccode_a",
                                       ccode2 = "ccode_b",
                                       year = "year")) %>%
    mutate(VictoryMax = pmax(VictoryA, VictoryB),
           VictoryMin = pmin(VictoryA, VictoryB))

## DOE scores in every row:
length(which(is.na(data_salehyan_2008$VictoryA)))

## he logs, so we log
data_salehyan_2008$logVictoryMax <- log(data_salehyan_2008$VictoryMax)
data_salehyan_2008$logVictoryMin <- log(data_salehyan_2008$VictoryMin)

set.seed(8032)
doeForm <- update(f_salehyan_2008,
                   . ~ . - lcaprat + logVictoryMax + logVictoryMin)
doe_salehyan_2008 <- glm_and_cv(
  form = doeForm,
  data = data_salehyan_2008,
  number = 10,
  repeats = 100
)
printCoefmat(doe_salehyan_2008$summary)

save(cr_salehyan_2008,
     doe_salehyan_2008,
     file = "results-salehyan-2008.rda")
