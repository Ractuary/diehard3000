# diehard3000

[![Travis-CI Build Status](https://travis-ci.org/merlinoa/diehard3000.svg?branch=master)](https://travis-ci.org/merlinoa/diehard3000)

# Purpose

This package offers a framework for running simulations on survival contingent benefits.  Each simulation can be highly customized.

# Installation

```R
# install package
devtools::install_github("merlinoa/diehard3000", build_vignettes = TRUE)
```

# Examples

See the vignette:

```R
library(diehard3000) # the package must already be installed using the above instructions
browseVignettes("diehard3000")
```

[This shinydashboard](http://shiny.ractuary.com:3838/insuree-simulation/) provides an example of the `diehard3000` package used to monitor the reserve for a group of individuals.
