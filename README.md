# Goals

1. Simulate confidence levels for the present value of death benefits for a group of insurees each with differing life insurance policy terms, age, and benefit abounts.

2. Simulate confidence levels for the present value of life contingent annuity payments for a group of insurees each with differing policy terms, age, and anuity payment amounts.   

The simulation will use an actuarial lifetable to determine the probabilities of death and discount rates.  A different actuarial table can be used for each insuree.

# Usage

```R
# install package
devtools::install_github("lifetable", build_vignettes = TRUE)

# read the vignette to get started
browseVignettes("lifetable")
```
