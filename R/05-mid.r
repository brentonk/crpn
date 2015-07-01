################################################################################
###
### Merge imputed NMC data into COW data, and split sample into training and
### test sets
###
################################################################################

library("caret")
library("dplyr")
library("stringr")
library("tidyr")

###-----------------------------------------------------------------------------
### Load and clean the MID dispute-level data
###-----------------------------------------------------------------------------

raw_MID_dispute <- read.csv("../data/MIDA_4.01.csv", na.strings = "-9")

## We only need a few variables:
##   * Dispute number (version 3, since version 4 is mostly missing)
##   * Starting and ending years
##   * Number of states on each side
##   * Outcome
data_MID_dispute <- select(raw_MID_dispute,
                           DispNum3,
                           year = StYear,
                           NumA,
                           NumB,
                           Outcome)

## Only want disputes with exactly one state on each side
data_MID_dispute <- filter(data_MID_dispute,
                           NumA == 1,
                           NumB == 1)
dim(data_MID_dispute)                   # 2209 cases

## Only want those that end in victory, yield, or stalemate
data_MID_dispute <- filter(data_MID_dispute,
                           Outcome %in% 1:5)
dim(data_MID_dispute)                   # 1799 cases

## Exclude disputes starting after 2007, the endpoint of the NMC data
##
## Previously, we'd excluded those *ending* after 2007, but if we're using
## start-year values of the capability components, there's no need to exclude
## those that begin early enough
data_MID_dispute <- filter(data_MID_dispute,
                           year <= 2007)
dim(data_MID_dispute)                   # 1740 cases

## Treat yields like victories
##   Yield by Side A -> Victory by Side B
##   Yield by Side B -> Victory by Side A
data_MID_dispute <- mutate(data_MID_dispute,
                           Outcome = ifelse(Outcome == 3,  # A yields
                                            2,             # B wins
                                            Outcome),
                           Outcome = ifelse(Outcome == 4,  # B yields
                                            1,             # A wins
                                            Outcome),
                           Outcome = factor(Outcome,
                                            levels = c(2, 5, 1),
                                            labels = c("VictoryB",
                                                       "Stalemate",
                                                       "VictoryA"),
                                            ordered = TRUE))
with(data_MID_dispute, table(Outcome))  # VictoryA: 201
                                        # VictoryB: 79
                                        # Stalemate: 1460


###-----------------------------------------------------------------------------
### Merge in the MID participant-level data
###-----------------------------------------------------------------------------

raw_MID_participant <- read.csv("../data/MIDB_4.01.csv")

## Variables we need:
##   * Dispute number
##   * "Side A" indicator
##   * Country code
data_MID_participant <- select(raw_MID_participant,
                               DispNum3,
                               SideA,
                               ccode)

## Pare down to same set of MIDs as in the dispute-level data
data_MID_participant <- filter(data_MID_participant,
                               DispNum3 %in% data_MID_dispute$DispNum3)

## If we've done everything right, there should be 1740 each of 1s and 0s for
## the SideA indicator
with(data_MID_participant, table(SideA))
                                        # Yes

## Convert from "long" to "wide"
data_MID_participant <- data_MID_participant %>%
  mutate(SideA = factor(SideA,
                        levels = 1:0,
                        labels = c("ccode_a", "ccode_b"))) %>%
  spread(SideA, ccode)

## And now we should have 1740 rows, same as the dispute-level data
dim(data_MID_participant)               # Yes

## Merge country codes into the dispute-level data
data_MID <- left_join(data_MID_dispute,
                      data_MID_participant,
                      by = "DispNum3")

## Drop the variables we don't need anymore:
##   * Dispute number (not merging with any more MID data)
##   * Number of states on each side
data_MID <- select(data_MID,
                   -DispNum3,
                   -NumA,
                   -NumB)


###-----------------------------------------------------------------------------
### Merge with imputed NMC data
###-----------------------------------------------------------------------------

load("results-data-nmc.rda")
load("results-impute-nmc.rda")

## Function for merging NMC data into MID data for each side, setting variable
## names appropriately
merge_NMC_MID <- function(nmc, mid)
{
    ## Make "Side A" and "Side B" copies of the NMC data
    nmc_a <- nmc
    nmc_b <- nmc

    ## Make variable names in the NMC data country-specific, except year
    names(nmc_a) <- ifelse(names(nmc_a) != "year",
                           paste0(names(nmc_a), "_a"),
                           names(nmc_a))
    names(nmc_b) <- str_replace(names(nmc_a), "_a$", "_b")

    ## Merge in each NMC dataset in sequence
    mid %>%
      left_join(nmc_a) %>%
      left_join(nmc_b)
}

## Identify cases with no missingness in either country-year
completeness_NMC <- select(data_NMC,
                           ccode,
                           year)
completeness_NMC$complete <- apply(impute_NMC$missMatrix,
                                   1,
                                   function(x) !any(x))

## Merge completion indicators for each side into the base MID data, and
## calculate a summary variable for fully observed cases
data_MID <- data_MID %>%
  merge_NMC_MID(completeness_NMC, .) %>%
  mutate(complete = complete_a & complete_b) %>%
  select(-complete_a, -complete_b)
mean(data_MID$complete)                 # 83% of cases are complete

## Create test sample from the complete cases, amounting to 20% of the total
## data
##
## No longer need the complete variable once this is done
data_MID_complete <- data_MID %>%
  filter(complete == TRUE) %>%
  select(-complete)
data_MID_incomplete <- data_MID %>%
  filter(complete == FALSE) %>%
  select(-complete)

set.seed(2004)                          # For exact replicability
p <- 0.2 / mean(data_MID$complete)
obs_test <- createDataPartition(data_MID_complete$Outcome,
                                times = 1,
                                p = p)[[1]]
data_test <- data_MID_complete[obs_test, ]
data_train <- rbind(data_MID_complete[-obs_test, ],
                    data_MID_incomplete)
dim(data_test)
dim(data_train)
table(data_test$Outcome)
table(data_train$Outcome)

## Merge COW components into test data
data_test <- merge_NMC_MID(data_NMC,
                           data_test)
head(data_test)
sum(is.na(data_test))                   # 0 (as expected)

## Calculate logged capability ratio
data_test <- mutate(data_test,
                    capratio = log(exp(cinc_a) / (exp(cinc_a) + exp(cinc_b))))

## Save test data, and don't touch it again until model is trained
save(data_test,
     file = "results-data-test.rda")

## Create 10 imputed training sets by merging imputations of COW components into
## training data
imputations_train <- lapply(imputations_NMC,
                            merge_NMC_MID,
                            mid = data_train)

## Calculate logged capability ratio for training datasets
imputations_train <- lapply(imputations_train,
                            mutate_,
                            capratio = "log(exp(cinc_a) / (exp(cinc_a) + exp(cinc_b)))")
lapply(imputations_train, dim)
lapply(imputations_train, head)

save(imputations_train,
     file = "results-imputations-train.rda")
