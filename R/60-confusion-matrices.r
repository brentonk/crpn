## Confusion matrices for out-of-sample classifications by logit and by the
## best ML model (random forest + time)

library("caret")
library("reshape2")
library("ggplot2")
library("tikzDevice")

Sys.unsetenv("TEXINPUTS")

source("20-split-data.r")
source("51-probs-preds.r")

## Compute confusion matrices
confusion_logit <- confusionMatrix(preds_logit,
                                   work_test$OutcomeCollapsed)
confusion_rf_time <- confusionMatrix(preds_rf_time,
                                     work_test$OutcomeCollapsed)

## TODO: Alter to make faceted plot from list of confusion matrices

## Plot confusion matrices
confusionPlot <- function(...,
                          col_low = "#ffeece",
                          col_high = "#d0a75c")
{
    ## Make ggplot-able data from a list of confusion matrices
    confusionToData <- function(dots)
    {
        ans <- lapply(seq_along(dots), function(i) {
            mat <- dots[[i]]

            ## Convert to row probabilities and put in "long" format
            mat <- prop.table(mat$table, margin = 1)
            mat <- melt(mat)
            ## mat <- na.omit(mat)

            ## Include name of model as variable
            mat$model <- names(dots)[i]

            mat
        })

        ## Extract kappa and accuracy
        acc <- lapply(dots, function(x) x$overall["Accuracy"])
        acc <- sprintf("%.2f", acc)
        acc <- paste("Accuracy:", acc)
        kap <- lapply(dots, function(x) x$overall["Kappa"])
        kap <- sprintf("%.2f", kap)
        kap <- paste("Kappa:", kap)
        lab <- paste0("(", acc, ", ", kap, ")")

        ## Collapse to single data frame
        ans <- do.call(rbind, ans)
        ans$model <- factor(ans$model,
                            levels = names(dots),
                            labels = paste0(names(dots), "\n", lab))

        ans
    }

    ## Create data
    dots <- list(...)
    dat <- confusionToData(dots)

    ## Class labels
    levs <- c("VictoryA", "Stalemate", "VictoryB")
    labs <- c("A Wins", "Stalemate", "B Wins")

    return(
        ggplot(dat, aes(x = Reference, y = Prediction))
        + geom_tile(aes(fill = value),
                    colour = "grey50")
        ## + geom_text(data = dat$stats,
        ##             aes(label = label),
        ##             hjust = 0.5,
        ##             size = 3)
        + scale_x_discrete(name = "Actual Outcome",
                           limits = levs,
                           labels = labs)
        + scale_y_discrete(name = "Predicted Outcome",
                           limits = rev(levs),
                           labels = rev(labs),
                           drop = FALSE)
        + scale_fill_gradient(name = "Pr(Actual | Predicted)",
                              low = col_low,
                              high = col_high,
                              limits = c(0, 1),
                              na.value = "#ffffff")
        + facet_wrap(~ model, ncol = 2)
        + ggtitle("Out-of-Sample Classification Results")
        + theme_bw()
    )
}

## Plot parameters
plot_dir <- file.path("..", "poster", "fig")
plot_width <- 7.25
plot_height <- 3.5

cplot <- confusionPlot(
    "Capability Ratio Alone" = confusion_logit,
    "Random Forest Model\non CINC Components + Year" = confusion_rf_time
    )

## tikzDevice and beamerposter do not play nice, so using pdf()
pdf(file = file.path(plot_dir, "confusion-logit.pdf"),
     width = plot_width,
     height = plot_height)
print(cplot)
dev.off()

## Plot for slides
tikz(file = file.path("..", "slides", "confusion.tex"),
     width = 4.75,
     height = 2.75)
print(cplot
      + theme_grey(base_size = 7) 
      + theme(plot.background = element_rect(fill = "transparent", colour = NA),
              legend.background = element_rect(fill = "transparent", colour = NA),
              axis.title.y = element_text(vjust = 1))
      )
dev.off()
