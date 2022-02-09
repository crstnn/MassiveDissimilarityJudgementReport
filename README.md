# Massive Dissimilarity Judgement Report
Data pipeline, statistical aggregation and Bayesian MDS model selection on massive phenomenological dissimilarity data.
Below will entail a brief outline of what is within each of the files/folders in the directory.

![Data pipeline](https://github.com/crstnn/MassiveDissimilarityJudgementReport/blob/master/img/pipline.png)

## dataAggregation.R
This file contains the steps for (1) a canonical way of saving the data (with adjustments to styling and formats), (2) catch-trial analysis and (3) checking for participants' completion of the experiment.

## 'colourcodes' Folder
A CSV that contains the truth table of `realcomparison` values, indexed by row number (zero starting index in raw data and one starting index in the canonical data format).

## preliminaryGraphsAndAnalysis.R
Contains count matrices/heat maps for colour pairs and an efficient way to bind `realcomparison` to the RGB values from the truth table.
Also contains some preliminary analysis using logistic and probit regression.
Further on in this file, there is some incomplete code of mixed-effects models, whose analysis I did not continue.

## stratifiedDataMethods.R
Outlines 2 ways to perform the 'holdout method' (a cross-validation method).

## RGBtoCIE.R
Functions to convert one colour format to the other (going via XYZ).

## order_stimuli.R and Anything Prefixed with 'demcmc'
Supporting files as part of the analysis found in [Gronau and Lee 2020](https://link.springer.com/article/10.1007/s42113-020-00082-y).

## 'data' Folder
CSVs collected from Dom's experiment AND data used in Gronau and Lee 2020 (stored in non-CSV format).

## bayesianModelSelection.R
From methods outlined in Gronau and Lee 2020, we use our data, but with their methods.


