################################################################################
### Install packages necessary to replicate "Predictions, Proxies, and Power" by
### Robert J. Carroll and Brenton Kenkel
################################################################################

if (getRversion() < '3.2.0')
    stop("R version 3.2.0 or greater is required.")

if (!require("packrat"))
    install.packages("packrat")

ppp_packages <- packrat:::appDependencies(".")
install.packages(ppp_packages, dependencies = TRUE)
