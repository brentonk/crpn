Prediction, Proxies, and Power: Replication Archive
===================================================

Files
-----

Files in the main directory are:

-   `README.txt`: this file
-   `README.md`: Markdown source for this file
-   `codebook.pdf`: DOE scores codebook
-   `codebook.md`: Markdown source for codebook
-   `Makefile`: instructions to create the README and codebook

Files in the `data`, `R`, `reed-et-al-2008`, and `replications`
subdirectories are described in the corresponding sections below.

Installation
------------

Analysis was conducted using R version 3.2.0.

1.  Install R, version 3.2.0 or greater, from
    <http://cran.r-project.org>.

2.  Install the following R packages and their dependencies, including
    suggested packages. In parentheses we list the version of the
    package used in the analysis. More recent versions ought to work as
    well, to the best of our knowledge.

    -   `Amelia` (1.7.3)
    -   `BH` (1.58.0.1)
    -   `C50` (0.1.0.24)
    -   `DBI` (0.3.1)
    -   `DEoptimR` (1.0.4)
    -   `Formula` (1.2.1)
    -   `MatrixModels` (0.4.0)
    -   `R6` (2.1.1)
    -   `RColorBrewer` (1.1.2)
    -   `RSQLite` (1.0.0)
    -   `Rcpp` (0.12.1)
    -   `RcppArmadillo` (0.5.600.2.0)
    -   `RcppEigen` (0.3.2.5.1)
    -   `RcppRoll` (0.2.2)
    -   `SparseM` (1.7)
    -   `VGAM` (1.0.2)
    -   `assertthat` (0.1)
    -   `bayesm` (3.0.2)
    -   `broom` (0.4.1)
    -   `car` (2.1.0)
    -   `caret` (6.0.52)
    -   `chron` (2.3.47)
    -   `colorspace` (1.2.6)
    -   `compositions` (1.40.1)
    -   `cubature` (1.1.2)
    -   `dichromat` (2.0.0)
    -   `digest` (0.6.8)
    -   `doMC` (1.3.4)
    -   `doRNG` (1.6)
    -   `dplyr` (0.4.3)
    -   `energy` (1.6.2)
    -   `filehash` (2.3)
    -   `foreach` (1.4.3)
    -   `ggplot2` (1.0.1)
    -   `ggtern` (2.1.1)
    -   `glmx` (0.1.0)
    -   `gridExtra` (0.9.1)
    -   `gsubfn` (0.6.6)
    -   `gtable` (0.1.2)
    -   `iterators` (1.0.8)
    -   `kernlab` (0.9.22)
    -   `labeling` (0.3)
    -   `latex2exp` (0.4.0)
    -   `lazyeval` (0.1.10)
    -   `lme4` (1.1.9)
    -   `lmtest` (0.9.34)
    -   `magrittr` (1.5)
    -   `maxLik` (1.3.4)
    -   `minqa` (1.2.4)
    -   `miscTools` (0.6.16)
    -   `mlogit` (0.2.4)
    -   `mnormt` (1.5.4)
    -   `multiwayvcov` (1.2.3)
    -   `munsell` (0.4.2)
    -   `mvtnorm` (1.0.3)
    -   `nloptr` (1.0.4)
    -   `np` (0.60.2)
    -   `packrat` (0.4.4)
    -   `partykit` (1.0.3)
    -   `pbkrtest` (0.4.2)
    -   `pkgmaker` (0.22)
    -   `plyr` (1.8.3)
    -   `png` (0.1.7)
    -   `proto` (0.3.10)
    -   `pscl` (1.4.9)
    -   `psych` (1.6.4)
    -   `quantreg` (5.19)
    -   `randomForest` (4.6.10)
    -   `registry` (0.3)
    -   `reshape2` (1.4.1)
    -   `rngtools` (1.2.4)
    -   `robustbase` (0.92.6)
    -   `sampleSelection` (1.0.4)
    -   `sandwich` (2.3.3)
    -   `scales` (0.3.0)
    -   `sqldf` (0.4.10)
    -   `statmod` (1.4.24)
    -   `stringi` (0.5.5)
    -   `stringr` (1.0.0)
    -   `systemfit` (1.1.18)
    -   `tensorA` (0.36)
    -   `tibble` (1.0)
    -   `tidyr` (0.3.1)
    -   `tikzDevice` (0.10.1)
    -   `xtable` (1.7.4)
    -   `yaml` (2.1.13)
    -   `zoo` (1.7.12)

    You can run the provided script `install.r` to install all of the
    necessary packages.

3.  (Optional) If you wish to compile the codebook or this README from
    their respective Markdown source files, you will need to have
    installed Pandoc version 1.16.0.1 or greater from
    <http://pandoc.org>.

Calculation of DOE Scores
-------------------------

All of the following commands should be run in the `R` subdirectory.

Output labeled `../latex/FILENAME` is created in the `latex`
subdirectory of the main directory. Be sure the `latex` subdirectory
exists before running the following commands.

1.  Run `00-nmc.r`. Output:

    -   `../latex/tab-summary.tex`: Table 7 of the manuscript
    -   `results-data-nmc.rda`: transformed National Material
        Capabilities data
    -   `results-impute-nmc.rda`: imputed National Material Capabilities
        data used for model training
    -   `results-impute-nmc-new.rda`: imputed National Material
        Capabilities data used for DOE score calculation

2.  (Optional) Run `01-imputation-plots.r`. This will create a
    subdirectory called `imputation-plots` containing the time series
    plots of each imputed variable by country.

3.  Run `05-mid.r`. Output:

    -   `../latex/tab-mid.tex`: Table 1 of the manuscript
    -   `results-imputations-train.rda`: merged imputed National
        Material Capabilities and Militarized Interstate Dispute data
        used for model training

4.  Run `07-outcomes-time.r`. Output:

    -   `../latex/fig-outcomes-time.tex`: Figure 1 of the manuscript

5.  (Optional) Run `12-benchmark.r`. This will provide a conservative
    estimate of the total CPU hours required to run the model training.

6.  Run `14-train-models.r`. Output:

    -   `results-trained-models.rda`: trained models fit to the full
        data for each imputation
    -   `logs-models` subdirectory with log output

7.  Run `15-train-weights.r` using each i = 1, ..., 10 as the command
    line argument. This requires running R in the terminal like follows:

        Rscript 15-train-weights.r 1
        Rscript 15-train-weights.r 2
        ...
        Rscript 15-train-weights.r 10

    These can be run in parallel or out of sequence, since their results
    do not depend on each other.

    Output:

    -   `results-weights` subdirectory containing super learner weights
        for each imputation in separate files

8.  Run `16-collect-weights.r`. Output:

    -   `results-trained-weights.rda`: super learner weights collected
        into single R object

9.  Run `17-summarize-weights.r`. Output:

    -   `../latex/tab-ensemble.tex`: Table 3 of the manuscript
    -   `results-ensemble-loss.rda`: information about the uncorrected
        and corrected log loss of the full super learner

10. Run `18-capratio.r`. Output:

    -   `../latex/tab-capratio.tex`: Table 2 of the manuscript

11. Run `30-dir-dyad-year.r`. Output:

    -   `results-dir-dyad-year.rda`: data frame of all directed
        dyad-years

12. Run `31-predict.r` for each year from 1816 to 2007, by running it at
    the command line as follows (the first argument gives the starting
    year, the second argument gives the number of years to calculate
    from the starting year):

        Rscript 31-predict.r 1816 1
        Rscript 31-predict.r 1817 1
        ...
        Rscript 31-predict.r 2007 1

    These can be run in parallel or out of sequence, since their results
    do not depend on each other.

    Output:

    -   `results-predict` subdirectory containing CSV files with each
        year's DOE scores

13. Run `32-collect-predict.r`. Output:

    -   `results-predict-dir-dyad.csv`: DOE scores for directed dyads
    -   `results-predict-dyad.csv`: DOE scores for undirected dyads

14. Run `33-doe-vs-cinc.r`. Output:

    -   `../latex/fig-oof-pred.tex`: Figure 2 in the manuscript

15. Run `41-varimp.r` using each i = 0, ..., 179 as the command line
    argument, as in:

        Rscript 41-varimp.r 0
        Rscript 41-varimp.r 1
        ...
        Rscript 41-varimp.r 179

    These can be run in parallel or out of sequence, since their results
    do not depend on each other.

    Output:

    -   `results-varimp` subdirectory containing output of the variable
        importance analysis

16. Run `42-collect-varimp.r`. Output:

    -   `results-varimp.rda`: variable importance results collected into
        single R object

17. Run `43-summarize-varimp.r`. Output:

    -   `../latex/tab-varimp.tex`: Table 4 in the manuscript

Other files in the `R` subdirectory that aren't to be run directly:

-   `04-merge-nmc-dyad.r`: functions to merge National Material
    Capabilities with Militarized Interstate Dispute data and
    calculating capability ratios
-   `10-fn-train.r`: functions to train individual models and the super
    learner
-   `11-defs-train.r`: setup of candidate models for the super learner
-   `20-predict-from-ensemble.r`: functions to calculate predicted
    probabilities from the super learner
-   `40-fn-varimp.r`: functions to estimate variable importance
-   `model-info.yml`: metadata about each candidate model

Relevant files in the `data` subdirectory:

-   `NMC_v4_0.csv`: National Material Capabilities data (v4.0) from the
    Correlates of War project, downloaded 2015-09-22 from
    <http://correlatesofwar.org/data-sets/national-material-capabilities>
-   `MIDA_4.01.csv`: dispute-level Militarized Interstate Disputes
    data (v4.01) from the Correlates of War project, downloaded
    2015-09-22 from <http://correlatesofwar.org/data-sets/MIDs>
-   `MIDB_4.01.csv`: participant-level Militarized Interstate Disputes
    data (v4.01) from the Correlates of War project, downloaded
    2015-09-22 from <http://correlatesofwar.org/data-sets/MIDs>

Replication of Reed et al. (2008)
---------------------------------

All of the following commands should be run in the `reed-et-al-2008`
subdirectory.

Before running these commands, you must have the file
`results-predict-dyad.csv` in the `R` subdirectory, either by completing
the steps in the "Calculation of DOE Scores" section or by copying them
from our Dataverse.

1.  Run `run-and-plot.r`. Output:

    -   `../latex/fig-rcnw-gull.tex`: Figure 4 of the manuscript
    -   `results-reed-et-al.rda`: fitted model results

2.  Run `cv-and-table.r`. Output:

    -   `../latex/tab-rcnw.tex`: Table 5 of the manuscript

Other files in the `reed-et-al-2008` subdirectory that aren't to be run
directly:

-   `reed-et-al-2008.dta`: original replication data from Reed et al.
    (2008)
-   `idealpoint4600.dta`: ideal point estimates used to construct status
    quo estimates in Reed et al. (2008)

Replication of Studies with Power as Control
--------------------------------------------

All of the following commands should be run in the `replications`
subdirectory.

Before running these commands, you must have the files
`results-predict-dir-dyad.csv` and `results-predict-dyad.csv` in the `R`
subdirectory, either by completing the steps in the "Calculation of DOE
Scores" section or by copying them from our Dataverse.

Output labeled `../latex/FILENAME` is created in the `latex`
subdirectory of the main directory. Be sure the `latex` subdirectory
exists before running the following commands.

1.  Run each of the 18 scripts in the format `author-year.r`. These are:

    -   `arena-palmer-2009.r`
    -   `bennett-2006.r`
    -   `dreyer-2010.r`
    -   `fordham-2008.r`
    -   `fuhrmann-sechser-2014.r`
    -   `gartzke-2007.r`
    -   `huth-2012.r`
    -   `jung-2014.r`
    -   `morrow-2007.r`
    -   `owsiak-2012.r`
    -   `park-colaresi-2014.r`
    -   `salehyan-2008-ajps.r`
    -   `salehyan-2008-jop.r`
    -   `sobek-abouharb-ingram-2006.r`
    -   `uzonyi-souva-golder-2012.r`
    -   `weeks-2008.r`
    -   `weeks-2012.r`
    -   `zawahri-mitchell-2011.r`

    The results of these scripts do not depend on each other, so they
    can be run simultaneously.

    The output of each file is the corresponding
    `results-author-year.rda`, containing the results of the
    replication analysis.

2.  Run `collect.r`. Output:

    -   `../latex/tab-replications.tex`: Table 6 of the manuscript
    -   `../latex/tab-replications-appendix.tex`: Table 8 of the
        manuscript

3.  (Optional) Run `describe.r`. Output:

    -   `../latex/list-replications.tex`: description list in Section
        A.5 of the manuscript

Other files in the `replications` subdirectory that aren't to be run
directly:

-   18 data files of the form `author-year.dta`, each containing in
    Stata format the replication data for the corresponding analysis.
    (The exception is `park-colaresi-2014.RData`, which is in R
    Data format.)
-   `fn-collect.r`: functions to summarize and collect replication
    results
-   `glm-and-cv.r`: functions to run the replication analysis for a
    generalized linear model
-   `ordered-probit.r`: functions to cross-validate ordered probit
    models
-   `replication-info.yml`: metadata about each study being replicated

Data Sources
------------

### DOE Score Construction

-   National Material Capabilities:
    <http://correlatesofwar.org/data-sets/national-material-capabilities>
-   Militarized Interstate Disputes:
    <http://correlatesofwar.org/data-sets/MIDs>

### Replication Data

Each of the following links has been verified to be active as of
2016-10-12.

-   Arena and Palmer (2009):
    <http://www.isanet.org/LinkClick.aspx?fileticket=HbWNetnws5Y%3d&portalid=0>
-   Bennett (2006):
    <http://www.personal.psu.edu/dsb10/data/DemocracyISQ.zip>
-   Dreyer (2010):
    <http://www.isanet.org/LinkClick.aspx?fileticket=B5VokcGkYjw%3d&portalid=0>
-   Fuhrmann and Sechser (2014): <http://dx.doi.org/10.7910/DVN/27466>
-   Gartzke (2007):
    <http://dss.ucsd.edu/~egartzke/data/capitalistpeace_012007.dta>
-   Huth et al. (2012):
    <http://www.isanet.org/LinkClick.aspx?fileticket=-3zEm2WG6io%3d&portalid=0>
-   Jung (2014): <http://hdl.handle.net/1902.1/20327>
-   Morrow (2007): <http://hdl.handle.net/1902.1/10509>
-   Owsiak (2012):
    <http://www.isanet.org/LinkClick.aspx?fileticket=Jnrj8bDvMZ8%3d&portalid=0>
-   Park and Colaresi (2014):
    <https://sites.google.com/site/parkjoha/file/replication.zip?attredirects=0>
-   Reed et al. (2008): <http://web.utk.edu/~whwang/jop08.html>
-   Salehyan (2008, AJPS): <http://hdl.handle.net/1902.1/17905>
-   Salehyan (2008, JOP): <http://hdl.handle.net/1902.1/21469>
-   Sobek et al. (2006): <http://hdl.handle.net/1902.1/10107>
-   Uzonyi et al. (2012):
    <http://www.isanet.org/LinkClick.aspx?fileticket=XDeKBWdFfYI%3d&portalid=0>
-   Weeks (2008):
    <https://users.polisci.wisc.edu/jweeks/WeeksIO2008.zip>
-   Weeks (2012):
    <https://users.polisci.wisc.edu/jweeks/WeeksAPSR2012.zip>
-   Zawahri and Mitchell (2011):
    <http://www.isanet.org/LinkClick.aspx?fileticket=LsjLqz-fY1M%3d&portalid=0>

