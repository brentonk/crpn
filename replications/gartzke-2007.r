################################################################################
###
### Replication of Gartzke 2007, "The Capitalist Peace," replacing CINC ratios
### with DOE scores
###
### Replicating Table 1, Model 4
###
################################################################################

library("caret")
library("dplyr")
library("foreign")

source("glm-and-cv.r")

raw_gartzke_2007 <- read.dta("gartzke-2007.dta")
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## Rename spline variables generated by Stata
##
## R dislikes variable names starting with underscores
data_gartzke_2007 <- raw_gartzke_2007
names(data_gartzke_2007) <- gsub("^\\_",
                                 "T.",
                                 names(data_gartzke_2007))

## Convert response to factor
data_gartzke_2007 <- data_gartzke_2007 %>%
    mutate(maoznewl = factor(
               maoznewl,
               levels = 0:1,
               labels = c("No", "Yes")
           ))

## Merge in DOE scores
##
## Using undirected since the sample is undirected dyads -- also need to compute
## max and min for consistency, since the A/B labels are exchangeable
data_gartzke_2007 <- left_join(data_gartzke_2007,
                               doe_dyad,
                               by = c(statea = "ccode_a",
                                      stateb = "ccode_b",
                                      year = "year")) %>%
    mutate(VictoryMax = pmax(VictoryA, VictoryB),
           VictoryMin = pmin(VictoryA, VictoryB))
stopifnot(with(data_gartzke_2007, sum(is.na(VictoryA)) == 0))

## Drop cases where capability ratio is missing but DOE is not
##
## TODO: What is going on here?
data_gartzke_2007 <- data_gartzke_2007 %>%
    filter(!is.na(lncaprt))

## Replicating Table 1, model 4
f_gartzke_2007 <-
    maoznewl ~ demlo + demhi + deplo + capopenl + rgdppclo + gdpcontg + contig +
        logdstab + majpdyds + alliesr + lncaprt + namerica + samerica + europe +
        africa + nafmeast + asia + T.spline1 + T.spline2 + T.spline3

## Null hypothesis: no effect of GDP (regardless of contiguity)
hyp_main <- c("rgdppclo = 0", "gdpcontg = 0")

## Replicate original model and cross-validate
set.seed(707)
cr_gartzke_2007 <- glm_and_cv(
    form = f_gartzke_2007,
    data = data_gartzke_2007,
    se_cluster = data_gartzke_2007$dyadid,
    hyp_main = hyp_main,
    hyp_power = "lncaprt = 0",
    number = 10,
    repeats = 10
)
print(cr_gartzke_2007$summary)

## Replace (logged) capability ratio with (logged) DOE scores and run again
doe_gartzke_2007 <- glm_and_cv(
    form = update(f_gartzke_2007,
                  . ~ . - lncaprt + log(VictoryMax) + log(VictoryMin)),
    data = data_gartzke_2007,
    se_cluster = data_gartzke_2007$dyadid,
    hyp_main = hyp_main,
    hyp_power = c("log(VictoryMax) = 0", "log(VictoryMin) = 0"),
    number = 10,
    repeats = 10
)
print(doe_gartzke_2007$summary)

## Check that response is the same across runs
stopifnot(identical(as.vector(cr_gartzke_2007$y),
                    as.vector(doe_gartzke_2007$y)))

save(cr_gartzke_2007,
     doe_gartzke_2007,
     file = "results-gartzke-2007.rda")
