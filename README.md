# Goals

1. Simulate confidence levels for the present value of death benefits for a group of insurees each with differing life insurance policy terms, age, and benefit amounts.

2. Simulate confidence levels for the present value of life contingent annuity payments for a group of insurees each with differing policy terms, age, and anuity payment amounts.   

The simulation will use the probabilities of death from an actuarial lifetable (such as the one available [here](http://www.ssa.gov/oact/STATS/table4c6.html)) to generate the sample.  A different actuarial table can be used for each insuree.

# Usage

```R
# install package
devtools::install_github("lifetable", build_vignettes = TRUE)

# read the vignette to get started
browseVignettes("lifetable")
```
