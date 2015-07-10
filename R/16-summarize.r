################################################################################
###
### Summarize the results of the training process, namely the average ensemble
### weight and average log-loss of each method
###
################################################################################

library("dplyr")
library("xtable")
library("yaml")

load("results-data-nmc.rda")
load("results-imputations-train.rda")
load("results-full-ensemble.rda")


###-----------------------------------------------------------------------------
### Big table of ensemble results
###-----------------------------------------------------------------------------

## Calculate the average (across imputations) CV log-loss of each model
model_ll <- sapply(full_ensemble, function(x) {
    sapply(x$models, function(x) {
        min(x$results[, "logLoss"])
    })
})
model_ll <- rowMeans(model_ll)

## Also calculate average (across imputations) CV log-loss of the ensemble
ensemble_ll <- sapply(full_ensemble, function(x) {
    x$weights$value
})
model_ll <- c(model_ll, ensemble = mean(ensemble_ll))

## Calculate the average (across imputations) optimal ensemble weight of each
## model
model_weight <- sapply(full_ensemble, function(x) {
    wt <- x$weights$par
    c(wt, 1 - sum(wt))
})
model_weight <- rowMeans(model_weight)
model_weight <- c(model_weight, ensemble = NA)

## Arrange results into data frame
model_results <- data.frame(
    name = c(names(full_ensemble[[1]]$models), "ensemble"),
    logLoss = model_ll,
    weight = model_weight,
    stringsAsFactors = FALSE
)

## Calculate proportional reduction in loss
model_results <- model_results %>%
    mutate(prl = (logLoss[1] - logLoss) / logLoss[1]) %>%
    select(name, logLoss, prl, weight)

## Retrieve data frame of basic model characteristics
model_info <- yaml.load_file("model-info.yml",
                             handlers = list(
                                 map = function(x) data.frame(x, stringsAsFactors = FALSE),
                                 seq = function(x) do.call(rbind, x)
                             ))

## Merge tables
model_table <- inner_join(model_info,
                          model_results,
                          by = "name")

## Prettify numeric values and truncate small ensemble weights
model_table <- mutate(model_table,
                      logLoss = sprintf("%.2f", logLoss),
                      prl = sprintf("%.2f", prl),
                      prl = gsub("-", "$-$", prl, fixed = TRUE),
                      weight = ifelse(weight > 0.005,
                                      sprintf("%.2f", weight),
                                      "$<$0.01"))

## Use checkmarks for whether year is included
##
## Requires amssymb in the LaTeX file
model_table <- mutate(model_table,
                      year = ifelse(year,
                                    "$\\checkmark$",
                                    ""))

## Blank irrelevant values
model_table[1, "prl"] <- ""
model_table[nrow(model_table), c("data", "year", "weight")] <- ""

## Convert to LaTeX
model_xtable <- model_table %>%
    select(-name) %>%
    xtable(align = c("l", "l", "l", "c", "r", "r", "r"))
colnames(model_xtable) <- c("Method",
                            "Data",
                            "Year",
                            "CV Loss",
                            "P.R.L.",
                            "Weight")

## Write to file
print(model_xtable,
      file = file.path("..", "latex", "tab-ensemble.tex"),
      floating = FALSE,
      include.rownames = FALSE,
      sanitize.text.function = identity,
      hline.after = c(-1, 0, nrow(model_xtable) - 1, nrow(model_xtable)))
