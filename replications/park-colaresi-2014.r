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

source("glm-and-cv.r")

load("park-colaresi-2014.RData")
data_park_colaresi_2014 <- data1.subset
rm(data1.subset, data1.subset.nomiss)
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## Convert response to factor
data_park_colaresi_2014$cwmid <- factor(data_park_colaresi_2014$cwmid,
                                        levels = 0:1,
                                        labels = c("No", "Yes"))

## replicate their results and cross-validate
set.seed(90210)
f_park_colaresi_2014 <- 
  cwmid ~ lowdem + lowgdp + landcontig + nlparity + beckpeace + cvwonset + 
  dyaddur + samecolony + ethborder + terrsim + landcontig*nlparity + 
  landcontig*beckpeace + landcontig*cvwonset + landcontig*dyaddur + 
  landcontig*samecolony + landcontig*ethborder + landcontig*terrsim + 
  .spline1 + .spline2 +.spline3
cr_park_colaresi_2014 <- glm_and_cv(
  form = f_park_colaresi_2014,
  data = data_park_colaresi_2014,
  number = 10,
  repeats = 10
)
printCoefmat(cr_park_colaresi_2014$summary)

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

## replicate with DOE
set.seed(1209)
doeForm <- update(f_park_colaresi_2014,
                  . ~ . - nlparity + doeParity - landcontig * nlparity + 
                    landcontig * doeParity)
doe_park_colaresi_2014 <- glm_and_cv(
  form = doeForm,
  data = data_park_colaresi_2014,
  number = 10,
  repeats = 10
)
printCoefmat(doe_park_colaresi_2014$summary)

save(cr_park_colaresi_2014,
     doe_park_colaresi_2014,
     file = "results-park-colaresi-2014.rda")
