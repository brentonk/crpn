RCMD = Rscript
RFLAGS = --vanilla

data : results-imputations-train.rda results-impute-nmc-new.rda
.PHONY: data

dyads : results-dir-dyad-year.rda
.PHONY : dyads

REQ_NMC = ../data/NMC_v4_0.csv
OUT_NMC = results-data-nmc.rda results-impute-nmc.rda results-impute-nmc-new.rda
$(OUT_NMC) : 00-nmc.r $(REQ_NMC)
	$(RCMD) $(RFLAGS) 00-nmc.r

REQ_MID = 04-merge-nmc-dyad.r ../data/MIDA_4.01.csv ../data/MIDB_4.01.csv results-data-nmc.rda results-impute-nmc.rda
OUT_MID = results-imputations-train.rda
$(OUT_MID) : 05-mid.r $(REQ_MID)
	$(RCMD) $(RFLAGS) 05-mid.r

REQ_DDY = results-data-nmc.rda
OUT_DDY = results-dir-dyad-year.rda
$(OUT_DDY) : 30-dir-dyad-year.r $(REQ_DDY)
	$(RCMD) $(RFLAGS) 30-dir-dyad-year.r
