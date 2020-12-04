# Local Gaussian Correlation (Code for Master's Thesis)

This repository contains code written for my Master's Thesis.
It provides plotting routines for the R package "localgauss",
code to reproduce all figures in the thesis and a data analysis using stock market data.


## About

Estimation of the local Gaussian correlation is done using the R package "localgauss":

https://github.com/cran/localgauss

The latest version of this package does not contain a plotting routine anymore for compatibility reasons.
Therefore, new routines are supplied in this repository.


## Functions Provided

The function "plot_localgauss" from this repository produces local Gaussian correlation heat maps for
a "localgauss" object.

The function "plot_localgauss_diagonal" takes two-dimensional data as input and calculates local Gaussian correlation
estimates along the diagonal, again by using the "localgauss" package. It then outputs a line plot.

More information about these functions is given in the respective code.


## Reproduce Figures from Thesis

The scripts chapter1, chapter3, chapter5 and chapter6 produce figures from the thesis, mostly using the above functions.


## Data Analysis
The script chapter7_data_analysis contains the full code for the data analysis, i.e.,
figures and additional calculations.


## Required Packages

All the scripts (figures and data analysis) load the necessary plotting functions at the beginning.
Required packages are also loaded, but the user might have to install them first.