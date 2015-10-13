################################################################################
###
### Replication of Hafner-Burton and Montgomery 2006, "Power Positions"
###
### Table 1, Model 2
###
################################################################################

library("dplyr")
library("foreign")

source("glm-and-cv.r")

raw_hb_montgomery_2006 <- read.dta("hb-montgomery-2006.dta")
doe_dyad <- read.csv("../R/results-predict-dyad.csv")

## Compute data transformations
##
## Taken from the file IGODataSetup.do in the original replication archive
data_hb_montgomery_2006 <- raw_hb_montgomery_2006 %>%
    mutate(CentDif = abs(Cent1 - Cent2),
           ClusSame = (Clus1 == Clus2),
           ClusSizeMax = pmax(ClusSize1, ClusSize2),
           smldmat = pmin(demauta, demautb),
           smldep = pmin(dependa, dependb))

## Identify major-power dyads
##
## Also taken from IGODataSetup.do
data_hb_montgomery_2006 <- within(data_hb_montgomery_2006, {
    majpower <- rep(0, nrow(data_hb_montgomery_2006))
    majpower[(statea==2 | stateb==2) & (year>1898)] <- 1
    majpower[(statea==200 | stateb==200)] <- 1
    majpower[(statea==220 | stateb==220) & ((year<1941) | (year>1944))] <- 1
    majpower[(statea==255 | stateb==255) & ((year<1919) | (year>1924 & year<1946) | year>1990)] <- 1
    majpower[(statea==300 | stateb==300) & (year<1919)] <- 1
    majpower[(statea==325 | stateb==325) & (year>1859 & year<1944)] <- 1
    majpower[(statea==365 | stateb==365) & ((year<1918) | (year>1922))] <- 1
    majpower[(statea==710 | stateb==710) & (year>1949)] <- 1
    majpower[(statea==740 | stateb==740) & ((year>1894 & year<1946) | year>1990)] <- 1
})

## Convert response to factor
data_hb_montgomery_2006 <- data_hb_montgomery_2006 %>%
    mutate(disp_l1 = factor(
               disp_l1,
               levels = 0:1,
               labels = c("No", "Yes")
           ))

## Merge in DOE scores and transform
##
## Authors use log(CINC_max/CINC_min), so we'll include log(DOE_max) and
## log(DOE_min) in the replication model
data_hb_montgomery_2006 <- data_hb_montgomery_2006 %>%
    left_join(doe_dyad,
              by = c(statea = "ccode_a",
                     stateb = "ccode_b",
                     year = "year")) %>%
    mutate(VictoryMax = pmax(VictoryA, VictoryB),
           VictoryMin = pmin(VictoryA, VictoryB))

## Double-check for no missingness
stopifnot(!any(is.na(data_hb_montgomery_2006$VictoryA)))

## Replicate original model
set.seed(806)
f_hb_montgomery_2006 <-
    disp_l1 ~ IGOSame + ClusSame + CentDif + ClusSizeMax + smldmat + smldep +
        lcaprat2 + allies + hegdefb + contigkb + logdstab + majpower +
        disp_spl0 + disp_spl1 + disp_spl2 + disp_spl3
cr_hb_montgomery_2006 <- glm_and_cv(
    form = f_hb_montgomery_2006,
    data = data_hb_montgomery_2006,
    number = 10,
    repeats = 10
)
printCoefmat(cr_hb_montgomery_2006$summary)

## Replace logged capability ratio with logged DOE scores and run again
set.seed(608)
doe_hb_montgomery_2006 <- glm_and_cv(
    form = update(f_hb_montgomery_2006,
                  . ~ . - lcaprat2 + log(VictoryMax) + log(VictoryMin)),
    data = data_hb_montgomery_2006,
    number = 10,
    repeats = 10
)
printCoefmat(doe_hb_montgomery_2006$summary)

save(cr_hb_montgomery_2006,
     doe_hb_montgomery_2006,
     file = "results-hb-montgomery-2006.rda")
