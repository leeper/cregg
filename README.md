# Simple Conjoint Analyses and Visualization

**cregg** is a package for analyzing and visualizing the results of conjoint (factorial) experiments. It provides three main sets of functionality:

 - Estimation of average marginal component effects (AMCEs) for fully randomized conjoint designs, via `cjglm()`
 - Munging of AMCE estimates into tidy data frames, via `cj()`
 - ggplot2-based visualizations of AMCEs and marginal means (MMs), via `amceplot()` and `ammplot()` respectively

The package takes its name from the surname of a famous White House Press Secretary.

## Code Examples




The package includes an example conjoint dataset (borrowed from the [cjoint](https://cran.r-project.org/package=cjoint) package), which is used here and and in examples:


```r
library("cregg")
data("hainmueller")
```

The package provides straightforward calculation and visualization of descriptive marginal means (MMs). These represent the mean outcome across all appearances of a particular conjoint feature level, averaging across all other features. In forced choice conjoint designs, MMs by definition average 0.5 with values above 0.5 indicating features that increase profile favorability and values below 0.5 indicating features that decrease profile favorability. For continuous outcomes, AMMs can take any value in the full range of the outcome. Calculation of MMs entail no modelling assumptions are simply descriptive quantities of interest:


```r
# descriptive plotting
f <- ChosenImmigrant ~ Gender + Education + LanguageSkills + CountryOfOrigin + Job + JobExperience + JobPlans + ReasonForApplication + 
    PriorEntry
plot(mm(hainmueller, f, id = ~CaseID))
```

```
Error in mm(hainmueller, f, id = ~CaseID): could not find function "mm"
```

A more common analytic approach is to estimate average marginal component effects (AMCEs) using some form of regression analysis. cregg uses `glm()` and `svyglm()` to perform estimation and [margins](https://cran.r-project.org/package=margins) to generate average marginal effect estimates. Designs can be specified with any interactions between conjoint features but only AMCEs are returned. (No functionality is provided at the moment for explict estimation of feature interaction effects.) Just like for `amm()`, the output of `cj()` (or its alias, `amce()`) is a tidy data frame:


```r
# estimation
f <- ChosenImmigrant ~ Gender + Education + LanguageSkills + CountryOfOrigin
amces <- cj(hainmueller, f, id = ~CaseID)
str(amces)
```

```
Classes 'amce' and 'data.frame':	23 obs. of  9 variables:
 $ outcome  : chr  "ChosenImmigrant" "ChosenImmigrant" "ChosenImmigrant" "ChosenImmigrant" ...
 $ feature  : Factor w/ 4 levels "Country of Origin",..: 3 3 2 2 2 2 2 2 2 4 ...
 $ level    : Factor w/ 23 levels "Female","Male",..: 1 2 3 4 5 6 7 8 9 10 ...
 $ estimate : num  0 -0.0273 0 0.0323 0.0591 ...
 $ std.error: Named num  NA 0.00831 NA 0.01534 0.0154 ...
  ..- attr(*, "names")= chr  NA "Var_dydx_GenderMale" NA "Var_dydx_Education4th Grade" ...
 $ z        : num  NA -3.28 NA 2.11 3.84 ...
 $ p        : num  NA 0.001036 NA 0.035009 0.000125 ...
 $ lower    : num  NA -0.04355 NA 0.00227 0.02891 ...
 $ upper    : num  NA -0.011 NA 0.0624 0.0893 ...
```

This makes it very easy to modify, combine, print, etc. the resulting output. It also makes it easy to visualize using ggplot2. A convenience visualization function is provided:


```r
# plotting of AMCEs
plot(amces)
```

![plot of chunk plot_amce](figure/plot_amce-1.png)

The main selling point of cregg is simplicity of implementation and - unlike the [cjoint](https://cran.r-project.org/package=cjoint) package - cregg tries to follow tidy data principles throughout. Thus the response from both `mm()` and `cj()` consists of tidy data frames that can easily be stacked with others (e.g., for computing AMCEs for subsets of respondents and then producing a facetted or grouped visualization). It also tries to take better advantage of data preprocessing steps by:

 - Using factor base levels rather than trying to set baseline levels atomically
 - Using "label" attributes on variables to provide pretty printing

Contributions and feedback are welcome on [GitHub](https://github.com/leeper/cregg/issues).

## Installation

[![CRAN](https://www.r-pkg.org/badges/version/cregg)](https://cran.r-project.org/package=cregg)
![Downloads](https://cranlogs.r-pkg.org/badges/cregg)
[![Travis Build Status](https://travis-ci.org/leeper/cregg.png?branch=master)](https://travis-ci.org/leeper/cregg)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/PROJECTNUMBER?svg=true)](https://ci.appveyor.com/project/leeper/cregg)
[![codecov.io](https://codecov.io/github/leeper/cregg/coverage.svg?branch=master)](https://codecov.io/github/leeper/cregg?branch=master)

This package is not yet on CRAN. To install the latest development version you can pull a potentially unstable version directly from GitHub:

```R
if (!require("ghit")) {
    install.packages("ghit")
}
ghit::install_github("leeper/cregg")
```
