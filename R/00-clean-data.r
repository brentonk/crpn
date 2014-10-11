################################################################################
###
###   name:  make_working.R
###     by:  rjc
###   date:  8/15/14
###    why:  this merges various COW data into one set for us.
###
################################################################################

#### introductories ####
library("methods")
library("data.table")
library("Amelia")

if (!(Sys.info()["user"] %in% c("brenton", "kenkelb"))) {

  ## You may need to change this since I (bjk) moved this file to the /R subdir
  setwd("C:/Users/Rob/Dropbox/oinc_project/R")
  
}

### data
midA <- read.csv("../data/MIDA_4.01.csv")
midB <- read.csv("../data/MIDB_4.01.csv")
capab <- read.csv("../data/NMC_v4_0.csv")

#### whittle midA to the relevant conflicts ####

### get the one-on-ones
oneOnOne <- which(midA$NumA == 1 & midA$NumB == 1)

### how many?  2209
length(oneOnOne)

### subset midA to the one-on-ones
work <- midA[oneOnOne, ]

### which ones have clear outcomes?
clearOutcomes <- which(work$Outcome %in% 1:5)

### how many?  1799
work <- work[clearOutcomes,]

### collapse outcomes:
###   yield by side A -> victory for side B
###   yield by side B -> victory for side A
work <- within(work, {
    OutcomeCollapsed <- Outcome
    OutcomeCollapsed[OutcomeCollapsed == 3] <- 2
    OutcomeCollapsed[OutcomeCollapsed == 4] <- 1
    OutcomeCollapsed <- factor(OutcomeCollapsed,
                               levels = c(2L, 5L, 1L),
                               labels = c("VictoryB", "Stalemate",
                                   "VictoryA"),
                               ordered = TRUE)
})

## Indicator for A victory
work <- within(work, {
    VictoryA <- as.numeric(OutcomeCollapsed == "VictoryA")
                                        # Need to code as a factor in order to
                                        # play nice with class predictions
                                        # from `caret`
    VictoryA <- factor(VictoryA,
                       levels = 0:1,
                       labels = c("No", "Yes"))
    VictoryA[OutcomeCollapsed == "Stalemate"] <- NA
})

### clean up this chunk
rm(clearOutcomes, oneOnOne)

#### merge this subset with midB ####
midB_DT <- as.data.table(midB)
midB_DT$DispNum3 <- as.character(midB$DispNum3)
setkey(midB_DT, DispNum3)

work$SideA <- NA
work$SideB <- NA

for(i in 1:nrow(work)){
  
  dispInQ <- as.character(work$DispNum3[i])
  
  foo <- midB_DT[dispInQ]
  
  work$SideA[i] <- foo$ccode[which(foo$SideA == 1)]
  work$SideB[i] <- foo$ccode[which(foo$SideA == 0)]
  
  rm(dispInQ, foo)

}
rm(i)

### add some identifiers
work$A_syear <- paste(as.character(work$SideA),
                      as.character(work$StYear),
                      sep = "_"
                      )

work$A_eyear <- paste(as.character(work$SideA),
                      as.character(work$EndYear),
                      sep = "_"
                      )

work$B_syear <- paste(as.character(work$SideB),
                      as.character(work$StYear),
                      sep = "_"
                      )

work$B_eyear <- paste(as.character(work$SideB),
                      as.character(work$EndYear),
                      sep = "_"
                      )

#### add capability information ####

### address missingness
cincs <- data.frame(ccode = capab$ccode,
                    year = capab$year,
                    cinc = capab$cinc
                    )
droppers <- colnames(capab) %in% c("stateabb", "version", "cinc")
capab <- capab[,!droppers]

capab$irst[capab$irst == -9] <- NA
capab$milex[capab$milex == -9] <- NA
capab$milper[capab$milper == -9] <- NA
capab$pec[capab$pec == -9] <- NA
capab$tpop[capab$tpop == -9] <- NA
capab$upop[capab$upop == -9] <- NA
## capab$cinc[capab$cinc == -9] <- NA    # this line seems not to do anything
                                         # except make Rscript puke

bds <- c(3, 0, .Machine$double.xmax,
         4, 0, .Machine$double.xmax,
         5, 0, .Machine$double.xmax,
         6, 0, .Machine$double.xmax,
         7, 0, .Machine$double.xmax,
         8, 0, .Machine$double.xmax
         )
bds <- matrix(bds,
              ncol = 3,
              byrow = T
              )

numimput <- 100

## Using a random seed so output is stable across runs
set.seed(1991)
imput <- amelia(capab,
                ts = "year",
                cs = "ccode",
                bounds = bds,
                logs = c("milex", "pec"),
                m = numimput
                )

capab_am <- 1/numimput * Reduce('+', imput$imputations)

capab <- merge(capab_am, cincs, by = c("ccode", "year"))

### merge
capab_DT <- as.data.table(capab)
capab_DT[,
         cc_year := paste(as.character(capab$ccode),
                          as.character(capab$year),
                          sep = "_"
                          )
         ]
setkey(capab_DT, cc_year)

work$SideA_irst <- NA
work$SideA_milex <- NA
work$SideA_milper <- NA
work$SideA_pec <- NA
work$SideA_tpop <- NA
work$SideA_upop <- NA
work$SideA_cinc <- NA

work$SideB_irst <- NA
work$SideB_milex <- NA
work$SideB_milper <- NA
work$SideB_pec <- NA
work$SideB_tpop <- NA
work$SideB_upop <- NA
work$SideB_cinc <- NA

for(i in 1:nrow(work)){
  
  AInQ <- work$A_syear[i]
  BInQ <- work$B_syear[i]
  
  ADat <- capab_DT[AInQ]
  BDat <- capab_DT[BInQ]
  
  work$SideA_irst[i] <- ADat$irst
  work$SideA_milex[i] <- ADat$milex
  work$SideA_milper[i] <- ADat$milper
  work$SideA_pec[i] <- ADat$pec
  work$SideA_tpop[i] <- ADat$tpop
  work$SideA_upop[i] <- ADat$upop
  work$SideA_cinc[i] <- ADat$cinc
  
  work$SideB_irst[i] <- BDat$irst
  work$SideB_milex[i] <- BDat$milex
  work$SideB_milper[i] <- BDat$milper
  work$SideB_pec[i] <- BDat$pec
  work$SideB_tpop[i] <- BDat$tpop
  work$SideB_upop[i] <- BDat$upop
  work$SideB_cinc[i] <- BDat$cinc
  
  rm(AInQ, BInQ)

}
rm(i)

work <- work[work$EndYear <= 2007,]
work$Aratio <- work$SideA_cinc / (work$SideA_cinc + work$SideB_cinc)

save(work, file = "working.RData")
