################################################################################
###
### Summarize the results of the training process, namely the average ensemble
### weight and average log-loss of each method
###
################################################################################

library("dplyr")
library("foreach")
library("xtable")
library("yaml")

load("results-trained-weights.rda")


###-----------------------------------------------------------------------------
### Big table of ensemble results
###-----------------------------------------------------------------------------

## Calculate the average (across imputations) nested CV log-loss of each model
##
## It's *nested* CV because we're using the out-of-fold predictions from each
## (i.e., not taking the tuning parameters as fixed at their final values)
model_loss <- foreach (wts = trained_weights, .combine = "rbind") %do% {
    ## Trim the out-of-fold probabilities the same way caret:::mnLogLoss does
    probs <- wts$out_probs
    eps <- 1e-15
    probs[probs < eps] <- eps
    probs[probs > 1 - eps] <- 1 - eps

    -1 * colMeans(log(probs))
}
model_loss <- colMeans(model_loss)

## Average (across imputations) weight of each candidate model and back out the
## weight of the last model
model_weights <- foreach (wts = trained_weights, .combine = "rbind") %do% {
    wts$weights$par
}
model_weights <- colMeans(model_weights)
model_weights <- c(model_weights, 1 - sum(model_weights))

## Arrange results into data frame
model_results <- data.frame(
    name = names(model_loss),
    logLoss = model_loss,
    weight = model_weights,
    stringsAsFactors = FALSE
)

## Calculate proportional reduction in loss
null_loss <- model_loss["null"]
model_results <- model_results %>%
    mutate(prl = (null_loss - logLoss) / null_loss) %>%
    select(name, logLoss, prl, weight)

## Retrieve data frame of basic model characteristics
model_info <- yaml.load_file("model-info.yml",
                             handlers = list(
                                 map = function(x) data.frame(x, stringsAsFactors = FALSE),
                                 main = function(x) do.call(rbind, x)
                             ))

## Merge tables
model_table <- inner_join(model_info,
                          model_results,
                          by = "name")

## Examine splits across different factors
model_table %>% group_by(method) %>% summarise(prl = mean(prl)) %>% arrange(prl)
model_table %>% group_by(data) %>% summarise(prl = mean(prl)) %>% arrange(prl)
model_table %>% group_by(year) %>% summarise(prl = mean(prl)) %>% arrange(prl)

## Examine paired differences in CV loss for time/no time
ll_year_t <- with(model_table[-(1:3), ] %>% filter(year == TRUE), logLoss)
ll_year_f <- with(model_table[-(1:3), ] %>% filter(year == FALSE), logLoss)
t.test(ll_year_t, ll_year_f, paired = TRUE)

## Examine paired differences in CV loss for components/proportions
ll_comps <- with(model_table[-(1:3), ] %>% filter(data == "Components"), logLoss)
ll_props <- with(model_table[-(1:3), ] %>% filter(data == "Proportions"), logLoss)
t.test(ll_comps, ll_props, paired = TRUE)

## Prettify numeric values and truncate small ensemble weights
model_xtable <- mutate(model_table,
                       logLoss = sprintf("%.2f", logLoss),
                       prl = sprintf("%.2f", prl),
                       prl = gsub("-", "$-$", prl, fixed = TRUE),
                       prl = ifelse(name == "null", "", prl),
                       weight = ifelse(weight > 0.005,
                                       sprintf("%.2f", weight),
                                       "$<$0.01"))

## Use checkmarks for whether year is included
##
## Requires amssymb in the LaTeX file
model_xtable <- mutate(model_xtable,
                       year = ifelse(year,
                                     "$\\checkmark$",
                                     ""))

## Average (across imputations) CV log-loss of the ensemble
ensemble_loss <- foreach (wts = trained_weights, .combine = "c") %do% {
    wts$weights$value
}
ensemble_loss <- mean(ensemble_loss)
ensemble_prl <- (null_loss - ensemble_loss) / null_loss

## Average (across imputations) bias of the CV log-loss of the ensemble
ensemble_bias <- foreach (wts = trained_weights, .combine = "c") %do% {
    wts$bias_min_cv
}
ensemble_bias <- mean(ensemble_bias)
corrected_loss <- ensemble_loss + ensemble_bias
corrected_prl <- (null_loss - corrected_loss) / null_loss

## Add Super Learner line to table
model_xtable <- rbind(model_xtable,
                      list(
                          name = "ensemble",
                          method = "Super Learner",
                          data = "",
                          year = "",
                          logLoss = sprintf("%.2f", ensemble_loss),
                          prl = sprintf("%.2f", ensemble_prl),
                          weight = ""
                      ),
                      list(
                          name = "",
                          method = "(bias-corrected)",
                          data = "",
                          year = "",
                          logLoss = sprintf("%.2f", corrected_loss),
                          prl = sprintf("%.2f", corrected_prl),
                          weight = ""
                      ))

## Convert to LaTeX
model_xtable <- model_xtable %>%
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
      booktabs = TRUE,
      file = file.path("..", "latex", "tab-ensemble.tex"),
      floating = FALSE,
      include.rownames = FALSE,
      sanitize.text.function = identity,
      hline.after = c(-1, 0, nrow(model_xtable) - 2, nrow(model_xtable)))
