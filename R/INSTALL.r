################################################################################
###
### Install required packages
###
### NOTE: This does not provide any sort of versioning control, and thus should
### not be mistaken for replacing use of a tool like Packrat.  It's just for
### convenience when working on the code on a new machine.
###
################################################################################

## Use an internal function from Packrat to search the directory for package
## dependencies
if (!require("packrat"))
    install.packages("packrat")
oinc_dependencies <- packrat:::appDependencies(".")

## Take out Packrat, since we just installed it.  Also take out any MPI-related
## dependencies, which should be installed separately on a machine running
## openMPI.
oinc_dependencies <- setdiff(oinc_dependencies,
                             c("packrat", "Rmpi", "doMPI"))

## Install packages
##
## Setting dependencies = TRUE to force installing suggested packages, which
## should ease things with caret
install.packages(pkgs = oinc_dependencies,
                 dependencies = TRUE)
