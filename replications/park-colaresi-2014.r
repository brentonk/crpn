################################################################################
###
### Replication of Park and Colaresi 2013, "Safe Across the Borders,"
### replacing CINC ratio with DOE scores
###
### Replicating Table 1, Model 4
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("MASS")

source("glm-and-cv.r")

park_colaresi_obj <- load("park-colaresi-2014.RData")
data_park_colaresi_2014 <- data1.subset
rm(list = park_colaresi_obj)
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## Convert response to factor
data_park_colaresi_2014$cwmid <- factor(data_park_colaresi_2014$cwmid,
                                        levels = 0:1,
                                        labels = c("No", "Yes"))

## merge in DOE scores
data_park_colaresi_2014 <- left_join(data_park_colaresi_2014,
                                     doe_dyad,
                                     by = c(ccode1 = "ccode_a",
                                            ccode2 = "ccode_b",
                                            year = "year"))

## every row has its DOE
stopifnot(with(data_park_colaresi_2014, !any(is.na(VictoryA))))

## they use a parity variable (0 is no parity, 1 is perfect parity).
data_park_colaresi_2014$doeParity <- abs(data_park_colaresi_2014$VictoryA -
                                         data_park_colaresi_2014$VictoryB)

## Table 1, Model 4
f_park_colaresi_2014 <-
    cwmid ~ lowdem + lowgdp + landcontig *
        (nlparity + beckpeace + .spline1 + .spline2 + .spline3 + cvwonset +
         dyaddur + samecolony + ethborder + terrsim) +
        prev.mid.count + PostColdWar + majdyad

## Null hypothesis: no effect of democracy
hyp_main <- "lowdem = 0"

## replicate their results and cross-validate
set.seed(24934)
cr_park_colaresi_2014 <- glm_and_cv(
    form = f_park_colaresi_2014,
    data = data_park_colaresi_2014,
    se_cluster = NULL,
    hyp_main = hyp_main,
    hyp_power = c("nlparity = 0", "landcontig:nlparity = 0"),
    number = 10,
    repeats = 10
)
print(cr_park_colaresi_2014$summary)

## replicate with DOE
doe_park_colaresi_2014 <- glm_and_cv(
    form = update(f_park_colaresi_2014,
                  . ~ . - landcontig * nlparity + landcontig * doeParity),
    data = data_park_colaresi_2014,
    se_cluster = NULL,
    hyp_main = hyp_main,
    hyp_power = c("doeParity = 0", "landcontig:doeParity = 0"),
    number = 10,
    repeats = 10
)
print(doe_park_colaresi_2014$summary)

save(cr_park_colaresi_2014,
     doe_park_colaresi_2014,
     file = "results-park-colaresi-2014.rda")
