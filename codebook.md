---
title: "Dispute Outcome Expectations: Codebook"
author:
  - Robert J. Carroll
  - Brenton Kenkel
date: October 13, 2016
fontsize: 12pt
---

The Dispute Outcome Expectations dataset, as constructed in our paper "Prediction, Proxies, and Power," consists of two files in comma-separated values format:

* `results-predict-dir-dyad.csv`: Directed-dyad-year DOE scores
* `results-predict-dyad.csv`: Undirected-dyad-year DOE scores

Each dataset contains observations on all dyad-years from 1816 to 2007, according to the Correlates of War Project's State System Membership data.^[<http://correlatesofwar.org/data-sets/state-system-membership>]

Both datasets contain the same variables:

* `ccode_a`: Correlates of War code for Country A
* `ccode_b`: Correlates of War code for Country B
* `year`: Year of observation
* `VictoryB`: Estimated probability of victory by Country B
* `Stalemate`: Estimated probability of stalemate
* `VictoryA`: Estimated probability of victory by Country A (always equals 1 -- `VictoryB` -- `Stalemate`)

In the directed data, Country A is assumed to be the initiator and Country B is assumed to be the target of the hypothetical dispute for which probabilities are calculated.  In the undirected data, values represent the average of the estimates for when Country A is the initiator and when Country A is the target.
