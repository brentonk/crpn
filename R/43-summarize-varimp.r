################################################################################
###
### Summarize the results of the variable importance study and make tables
###
################################################################################

library("plyr")
library("dplyr")
library("foreach")
library("iterators")
library("tidyr")
library("xtable")

source("10-fn-train.r")
source("40-fn-varimp.r")

load("results-varimp.rda")
load("results-trained-weights.rda")


## Include the original ensemble
trained_weights <- foreach (fit = trained_weights, i = icount()) %do% {
    fit$combo <- 0
    fit$imputation <- i
    fit
}
results_varimp <- c(results_varimp, trained_weights)

## Gather relevant components of the results into a table:
##   * Log loss, with and without time models included
##   * Variable combination dropped
##   * Imputation
table_varimp <- foreach (fit = results_varimp, .combine = "rbind") %do% {
    ## Calculate log loss when time models are dropped
    bad_cols <- grep("trend|_t$", colnames(fit$out_probs))
    loss_no_time <- learn_ensemble(fit$out_probs[, -bad_cols])$value

    c(loss = fit$weights$value,
      loss_no_time = loss_no_time,
      combo = fit$combo,
      imputation = fit$imputation)
}

## Convert to data frame so we can run dplyr operations and include
## string/factor variables too
table_varimp <- as.data.frame(table_varimp)
rownames(table_varimp) <- NULL

## Prepare list of component combinations for merging
component_combos$combo <- seq_len(nrow(component_combos))
component_combos <- rbind(component_combos,
                          data.frame(variable = "none",
                                     to_drop = "both",
                                     combo = 0))

## Average across imputations and merge in component names
table_varimp <- table_varimp %>%
    gather(drop_time, loss, -combo, -imputation) %>%
    mutate(drop_time = revalue(drop_time, c("loss" = "no",
                                            "loss_no_time" = "yes"))) %>%
    left_join(component_combos, by = "combo") %>%
    group_by(variable, to_drop, drop_time) %>%
    summarise(loss = mean(loss)) %>%
    ungroup()

## Calculate loss of full model
ensemble_loss <- table_varimp %>%
    filter(variable == "none",
           drop_time == "no") %>%
    `[[`("loss")

## Calculate relative losses
table_varimp <- table_varimp %>%
    mutate(rel_loss = (loss - ensemble_loss) / ensemble_loss)

## Make pretty for the paper
xtable_varimp <- table_varimp %>%
    filter(to_drop == "both") %>%
    select(variable, drop_time, rel_loss) %>%
    spread(key = drop_time,
           value = rel_loss) %>%
    mutate(variable = revalue(variable, c("none" = "None",
                                          "irst" = "Iron and Steel Production",
                                          "milex" = "Military Expenditures",
                                          "milper" = "Military Personnel",
                                          "pec" = "Primary Energy Consumption",
                                          "tpop" = "Total Population",
                                          "upop" = "Urban Population"))) %>%
    mutate_each(funs(100 * .), -variable) %>%
    mutate_each(funs(sprintf("%.2f", .)), -variable) %>%
    mutate_each(funs(paste0(., "\\%")), -variable) %>%
    mutate_each(funs(gsub("-", "$-$", ., fixed = TRUE)), -variable) %>%
    rename("Dropped Variable" = variable,
           "With Year" = no,
           "Without Year" = yes)

## Put "none" first
xtable_varimp <- xtable_varimp[c(4, 1:3, 5:7), ]

xtable_varimp %>%
    xtable(align = c("l", "l", "r", "r")) %>%
    print(floating = FALSE,
          hline.after = c(0, nrow(.)),
          include.rownames = FALSE,
          add.to.row = list(
              pos = list(-1),
              command = paste("\\toprule",
                              "& \\multicolumn{2}{c}{Increase in Loss}",
                              "\\\\",
                              "\\cmidrule(lr){2-3}",
                              "\n")
          ),
          sanitize.text.function = identity,
          booktabs = TRUE,
          file = file.path("..", "latex", "tab-varimp.tex"))
