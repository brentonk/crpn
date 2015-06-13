RCMD = Rscript
RFLAGS = --vanilla

# Imputations of NMC data
NMC_OUT = results-impute-nmc.rda results-imputations-nmc.rda
$(NMC_OUT) : 00-nmc.r ../data/NMC_v4_0.csv
	$(RCMD) $(RFLAGS) 00-nmc.r
