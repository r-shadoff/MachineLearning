This repo is IN PROGRESS, but will eventually include multiple scripts for computing a variety of biostatistics.

## Project 1: Phenotype Prediction Evaluation (multiclass_classifier.R)
This code is a work in progress!

## Goal
This script takes two .tsv files as input and is meant to evaluate the performance of phenotype predictions that have already been generated. This script also includes code that uses caret to generate a multiclass classifier using a random forest model, and this functionality can be used if genotype data for phenotype-informative SNPs is available for your samples.

## Input Data Format
The data used to test this script will not be released, but you can use this script as long as your data is formatted in the following way:
1. The first file should be a .tsv with these columns: Sample ID, Hair Colour (Actual), Hair Colour (Predicted), Eye Colour (Actual), Eye Colour (Predicted).
2. The second file is not necessary unless you want to test out the multiclass random forest classifier. The second file should include columns containing: Sample ID, SNP1 Genotype, SNP2 Genotype, SNP3 Genotype .... SNPn Genotype.

## Intended Output Metrics and Plots
1. ROC AUC curves for each hair colour and eye colour class
2. Accuracy, sensitivity, specificity, PPV, NPV, Prevalenced, Detection Rate, Detection Prevalence, and Balanced Accuracy for each class

