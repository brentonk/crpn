################################################################################
###
### Replication of Owsiak 2012, "Signing Up for Peace," 
### replacing CINC ratio with DOE scores
###
### Replicating Table 3, Model 3
###
################################################################################

library("caret")
library("dplyr")
library("foreign")
library("MASS")

source("glm-and-cv.r")

data_owsiak_2012 <- read.dta("owsiak-2012.dta")
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## clean spline names
names(data_owsiak_2012)[names(data_owsiak_2012) == "_spline1"] <- "spline1"
names(data_owsiak_2012)[names(data_owsiak_2012) == "_spline2"] <- "spline2"
names(data_owsiak_2012)[names(data_owsiak_2012) == "_spline3"] <- "spline3"

## switch to factor for caret
data_owsiak_2012$midonset <- factor(data_owsiak_2012$midonset,
                                    levels = 0:1,
                                    labels = c("No", "Yes"))

## merge in the DOE scores and compute pairwise max and min
data_owsiak_2012 <- left_join(data_owsiak_2012,
                              doe_dyad,
                              by = c(ccode1 = "ccode_a",
                                     ccode2 = "ccode_b",
                                     year = "year")) %>%
    mutate(VictoryMax = pmax(VictoryA, VictoryB),
           VictoryMin = pmin(VictoryA, VictoryB))

## DOE in every row
stopifnot(with(data_owsiak_2012, !any(is.na(VictoryA))))

## replication formula
f_owsiak_2012 <- 
    midonset ~ settlem + jtdem6 + majinvolve + atopally + lncincratio + 
        peaceyears + spline1 + spline2 + spline3

## Null hypothesis: no effect of settled borders
hyp_main <- "settlem = 0"

## run the reported model with cross validation
set.seed(93460)
cr_owsiak_2012 <- glm_and_cv(
    form = f_owsiak_2012,
    data = data_owsiak_2012,
    se_cluster = NULL,
    hyp_main = hyp_main,
    hyp_power = "lncincratio = 0",
    number = 10,
    repeats = 100
)
print(cr_owsiak_2012$summary)

## run it with DOE
doe_owsiak_2012 <- glm_and_cv(
    form = update(f_owsiak_2012,
                  . ~ . - lncincratio + log(VictoryMax) + log(VictoryMin)),
    data = data_owsiak_2012,
    se_cluster = NULL,
    hyp_main = hyp_main,
    hyp_power = c("log(VictoryMax) = 0", "log(VictoryMin) = 0"),
    number = 10,
    repeats = 100
)
print(doe_owsiak_2012$summary)

## save it
save(cr_owsiak_2012,
     doe_owsiak_2012,
     file = "results-owsiak-2012.rda")
