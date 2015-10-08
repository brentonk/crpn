################################################################################
###
### Closer look at the capability ratio ordered logit
###
################################################################################

library("MASS")
library("dplyr")
library("xtable")

load("results-imputations-train.rda")

## Re-running the model to avoid having to load the huge ensemble into memory
##
## No need to combine across imputations since there's no missingness in the
## capability ratio
polr_capratio <- polr(Outcome ~ capratio,
                      data = imputations_train[[1]])

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
