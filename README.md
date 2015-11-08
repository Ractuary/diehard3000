# diehard3000

[![Travis-CI Build Status](https://travis-ci.org/merlinoa/insuree.svg?branch=master)](https://travis-ci.org/merlinoa/insuree)

See the [package website](http://merlinoa.github.io/insuree/) for more information. 

# Purpose

This package offers a framework for running simulations on survival contingent benefits.  Each simulation can be highly customized.

# Installation

```R
# install package
devtools::install_github("merlinoa/insuree", build_vignettes = TRUE)
```

# Examples

See the vignette:

```R
library(insuree) # first install the package
browseVignettes("insuree")
```

[This shinydashboard](http://shiny.ractuary.com:3838/insuree-simulation/) provides an example of the `insuree` package used monitor the reserve for a group of individuals.
