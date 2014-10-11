RAWDATA = $(wildcard ../data/*.csv)
TRAIN_PREREQS = 20-split-data.r 26-full-formula.r 27-seeds.r working.RData
RFLAGS = --vanilla --verbose

all : train data

# Trained models

.PHONY : train
train : results-logit.RData results-rf.RData results-svm.RData

results-logit.RData : 30-train-logit.r 25-polr-caret.r $(TRAIN_PREREQS)
	Rscript $(RFLAGS) 30-train-logit.r

results-rf.RData : 31-train-rf.r $(TRAIN_PREREQS)
	Rscript $(RFLAGS) 31-train-rf.r

results-svm.RData : 32-train-svm.r $(TRAIN_PREREQS)
	Rscript $(RFLAGS) 32-train-svm.r

# Data file

.PHONY : data
data : working.RData

working.RData : 00-clean-data.r $(RAWDATA)
	Rscript $(RFLAGS) 00-clean-data.r
