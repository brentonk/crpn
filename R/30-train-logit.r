## Run an ordered logistic regression on the training data

library("caret")
library("ggplot2")
library("reshape2")

source("20-split-data.r")
source("25-polr-caret.r")

## Model with full data
fit_logit <- train(OutcomeCollapsed ~ log(Aratio),
                   data = work_train,
                   method = orderedResponse,
                   trControl = trainControl(
                       ## No tuning parameters, so no need to cross-validate
                       method = "none"
                   ))

## Model only on data with "victory" outcomes
fit_logit_victory <- train(VictoryA ~ log(Aratio),
                           data = work_train,
                           method = "glm",
                           subset = OutcomeCollapsed != "Stalemate",
                           trControl = trainControl(
                               method = "none"
                           ))

save(fit_logit, fit_logit_victory,
     file = "results-logit.RData")

### PLOTS ###

plot_dir <- file.path("..", "poster", "fig")

## Plot in-sample observed vs. predicted frequencies of each category
pred_logit <- predict(fit_logit, newdata = work_train)
plot_data <- data.frame(Outcome = unlist(list(
                            work_train$OutcomeCollapsed,
                            pred_logit)), # Need to use unlist(list(...))
                                          # because calling c() on a factor
                                          # coerces it to an integer
                        Type = factor(
                            c(rep(1L, length(work_train$OutcomeCollapsed)),
                              rep(2L, length(pred_logit))),
                            levels = 2:1,
                            labels = c("Predicted", "Actual"))
                        )
plot_data <- dcast(plot_data, Outcome ~ Type, fun = length)
plot_data <- melt(plot_data)

pdf(file = file.path(plot_dir, "class-logit.pdf"),
    width = 7.25,
    height = 3.5)
print(
    ggplot(plot_data, aes(x = Outcome, y = value))
    + geom_bar(aes(fill = variable),
               stat = "identity",
               position = "dodge")
    + scale_x_discrete("",
                       limits = c("VictoryA", "Stalemate", "VictoryB"),
                       labels = c("A Wins", "Stalemate", "B Wins"))
    + scale_y_continuous("")
    + ggtitle("Outcome Classifications")
    + scale_fill_manual("",
                        limits = c("Predicted", "Actual"),
                        values = c("#6ca9a0", "#887db8"))
    + theme_bw()
)
dev.off()

## Plot densities of predicted probabilities
pp_logit <- predict(fit_logit, newdata = work_train, type = "prob")
pp_logit$Stalemate <- NULL
pp_logit <- melt(pp_logit)

pdf(file = file.path(plot_dir, "pp-logit.pdf"),
    width = 7.25,
    height = 3.5)
print(
    ggplot(pp_logit, aes(x = value))
    + geom_density(aes(fill = variable),
                   alpha = 0.5)
    + scale_x_continuous("Predicted Probability",
                         limits = c(0, 0.5))
    + scale_y_continuous("",
                         labels = NULL)
    + ggtitle("Distribution of Dispute Outcome Probabilities")
    + scale_fill_manual("Outcome",
                        limits = c("VictoryA", "VictoryB"),
                        labels = c("A Wins", "B Wins"),
                        values = c("#778cb1", "#ffdea2"))
    + theme_bw()
)
dev.off()
