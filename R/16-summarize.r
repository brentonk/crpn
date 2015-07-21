################################################################################
###
### Summarize the results of the training process, namely the average ensemble
### weight and average log-loss of each method
###
################################################################################

library("dplyr")
library("ggplot2")
library("RColorBrewer")
library("scales")
library("tikzDevice")
library("xtable")
library("yaml")

Sys.unsetenv("TEXINPUTS")

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

## Weight on the capability ratio
print(model_weight["null"])             # 2.3e-7
print(model_weight["polr_capratio"])    # 7.5e-7

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

## Examine splits across different factors
model_table %>% group_by(method) %>% summarise(logLoss = mean(logLoss))
model_table %>% group_by(data) %>% summarise(logLoss = mean(logLoss))
model_table %>% group_by(year) %>% summarise(logLoss = mean(logLoss))

## Prettify numeric values and truncate small ensemble weights
model_xtable <- mutate(model_table,
                       logLoss = sprintf("%.2f", logLoss),
                       prl = sprintf("%.2f", prl),
                       prl = gsub("-", "$-$", prl, fixed = TRUE),
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

## Blank irrelevant values
model_xtable[1, "prl"] <- ""
model_xtable[nrow(model_xtable), c("data", "year", "weight")] <- ""

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
      file = file.path("..", "latex", "tab-ensemble.tex"),
      floating = FALSE,
      include.rownames = FALSE,
      sanitize.text.function = identity,
      hline.after = c(-1, 0, nrow(model_xtable) - 1, nrow(model_xtable)))


###-----------------------------------------------------------------------------
### Plot ensemble results for slides
###-----------------------------------------------------------------------------

plot_data <- model_table

## Parameters common across plots
tikz_width <- 4.25
tikz_height <- 3.25
tikz_size <- 8
tikz_theme <-
    theme(plot.background = element_rect(fill = "transparent", colour = NA),
          legend.background = element_rect(fill = "transparent", colour = NA))
tikz_package = c(getOption("tikzLatexPackages"),
                 "\\usepackage{amsmath}",
                 "\\usepackage{amssymb}")

## Histogram of ensemble weights
##
## WARNING: Manually annotated, not robust to changes in the ensemble
tikz(file = file.path("..", "slides", "fig-weight-hist.tex"),
     width = tikz_width,
     height = tikz_height)
print(
    plot_data %>%
        filter(name != "ensemble") %>%
        ggplot(aes(x = weight)) +
        geom_histogram(binwidth = 0.01) +
        annotate("text",
                 x = 0.2,
                 y = 2.5,
                 label = "Neural Network\n(Proportions + Year)",
                 size = 3) +
        annotate("segment",
                 x = 0.265,
                 xend = 0.325,
                 y = 2.5,
                 yend = 1) +
        annotate("text",
                 x = 0.275,
                 y = 7.5,
                 label = "Random Forest\n(Components + Year)",
                 size = 3) +
        annotate("segment",
                 x = 0.275,
                 xend = 0.355,
                 y = 6.2,
                 yend = 1.1) +
        scale_x_continuous("Super Learner Weight") +
        scale_y_continuous("Count") +
        theme_grey(base_size = tikz_size) +
        tikz_theme
)
dev.off()

## Relationship between PRL and ensemble weight
tikz(file = file.path("..", "slides", "fig-weight-by-cvl.tex"),
     width = tikz_width,
     height = tikz_height,
     package = tikz_package)
print(
    plot_data %>%
        filter(name != "ensemble") %>%
        ggplot(aes(x = logLoss, y = weight)) +
        geom_point() +
        stat_smooth(se = FALSE,
                    method = "loess") +
        scale_y_log10("Ensemble Weight") +
        scale_x_continuous("CV Loss") +
        theme_grey(base_size = tikz_size) +
        tikz_theme
)
dev.off()

## Total ensemble weight across each model type
tikz(file = file.path("..", "slides", "fig-weight-by-method.tex"),
     width = tikz_width,
     height = 0.55 * tikz_height,
     package = tikz_package)
print(
    plot_data %>%
        filter(name != "ensemble") %>%
        group_by(method) %>%
        summarise(weight = sum(weight)) %>%
        arrange(desc(weight)) %>%
        mutate(method = factor(method,
                               levels = rev(method))) %>%
        ggplot(aes(x = method, y = weight)) +
        geom_bar(stat = "identity") +
        scale_x_discrete("") +
        scale_y_continuous("") +
        ggtitle("Total Weight by Method") +
        coord_flip() +
        theme_grey(base_size = tikz_size) +
        tikz_theme
)
dev.off()

## Total ensemble weight across each data source
tikz(file = file.path("..", "slides", "fig-weight-by-data.tex"),
     width = tikz_width,
     height = 0.5 * tikz_height,
     package = tikz_package)
print(
    plot_data %>%
        filter(name != "ensemble") %>%
        mutate(data = ifelse(year,
                             paste(data, "+", "Year"),
                             data)) %>%
        group_by(data) %>%
        summarise(weight = sum(weight)) %>%
        arrange(desc(weight)) %>%
        mutate(data = factor(data,
                             levels = rev(data),
                             labels = gsub("only", "", rev(data)))) %>%
        ggplot(aes(x = data, y = weight)) +
        geom_bar(stat = "identity") +
        scale_x_discrete("") +
        scale_y_continuous("") +
        ggtitle("Total Weight by Data Source") +
        coord_flip() +
        theme_grey(base_size = tikz_size) +
        tikz_theme
)
dev.off()


###-----------------------------------------------------------------------------
### Closer look at capability ratio model
###-----------------------------------------------------------------------------

## Extract the fitted model
##
## Since `cinc` has no missing values, the model is the same across all
## imputations
polr_capratio <- full_ensemble[[1]]$models$polr_capratio$finalModel

## Look at the model summary
summary(polr_capratio)

## Highest and lowest predictions within the range of the training data
predict(polr_capratio,
        newdata = data.frame(capratio = range(imputations_train[[1]]$capratio)),
        type = "prob")
predict(polr_capratio,
        newdata = data.frame(capratio = quantile(imputations_train[[1]]$capratio,
                                                 c(0.25, 0.75))),
        type = "prob")

## Make a "regression table"
table_capratio <- data_frame(
    coef = c(polr_capratio$coefficients, polr_capratio$zeta),
    se = sqrt(diag(vcov(polr_capratio))),
    z = coef/se,
    p = 2 * pnorm(-abs(z))
)

## Prettify numeric values
table_capratio <- table_capratio %>%
    mutate_each(funs(sprintf("%.2f", .))) %>%
    mutate_each(funs(gsub("-", "$-$", ., fixed = TRUE))) %>%
    mutate(p = ifelse(p == "0.00", "$<$0.01", p)) %>%
    rename(Estimate = coef,
           SE = se,
           "$Z$" = z,
           "$p$" = p)
table_capratio[2:3, 3:4] <- ""

## Convert to LaTeX
xtable_capratio <- xtable(table_capratio,
                          align = c("l", "r", "r", "r", "r"))
rownames(xtable_capratio) <- c("Capability Ratio (logged)",
                               "Cutpoint: B Wins to Stalemate",
                               "Cutpoint: Stalemate to A Wins")

print(xtable_capratio,
      file = file.path("..", "latex", "tab-capratio.tex"),
      floating = FALSE,
      sanitize.text.function = identity)
