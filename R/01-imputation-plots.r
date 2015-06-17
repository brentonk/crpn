################################################################################
###
### Plot imputation diagnostics (for eventual supplemental appendix?)
###
################################################################################

library("Amelia")

load("results-impute-nmc.rda")

## Density of imputed values versus observed values
pdf(file = "imputation-plots/density.pdf",
    width = 9.5, height = 6)
plot(impute_NMC,
     which.vars = which(!(impute_NMC$orig.vars %in% c("ccode", "year"))))
dev.off()

## Time series plots of each variable for each country
##
## Takes a long time (2+ hours) and produces a ton of PDFs.  Fine-tuning for the
## output isn't available because of how tscsPlot() is coded, hence only six
## plots per page
for (v in impute_NMC$orig.vars[-(1:2)]) {
    cat("\nNow plotting", v, "\n")
    tscsPlot(impute_NMC,
             var = v,
             cs = NULL,
             pdfstub = paste0("imputation-plots/ts-", v),
             nr = 3,
             nc = 2,
             plotall = TRUE)
}
