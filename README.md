# insuree

[![Travis-CI Build Status](https://travis-ci.org/merlinoa/insuree.svg?branch=master)](https://travis-ci.org/merlinoa/insuree)

# Purpose

1. Create a function for running a Monte Carlo simulation for the present value of death benefits for a group of insurees.  Each insuree can have different life insurance policy terms, age, and benefit amounts.

2. Create a function for running a Monte Carlo simulation for the present value of life contingent annuity payments for a group of insurees. Each insuree can have different policy terms, age, and annuity payment amounts.

The simulation will use the probabilities of death from an actuarial lifetable (such as the one available [here](http://www.ssa.gov/oact/STATS/table4c6.html)) to generate the sample.  A different actuarial table can be used for each insuree.

# Installation

```R
# install package
devtools::install_github("merlinoa/insuree", build_vignettes = TRUE)

# read the vignette to get started
browseVignettes("insuree")
```
