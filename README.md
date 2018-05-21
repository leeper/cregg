# Simple Conjoint Analyses and Visualization

**cregg** is a package for analyzing and visualizing the results of conjoint ("cj") factorial experiments. It provides functionality that is useful for analyzing and otherwise examining conjoint experimental data:

 - Estimation of average marginal component effects (AMCEs) for fully randomized conjoint designs and munging of AMCE estimates into tidy data frames, via `amce()`
 - Calculation of marginal means (MMs) for conjoint designs and munging them into tidy data frames via `mm()`
 - Tabulation of display frequencies of feature attributes via `freqs()` and cross-tabulation of feature restrictions using `props()`
 - Diagnostics to assess preference heterogeneity, including an omnibus statstical test (`cj_anova()`) and tidying of differences in AMCEs across subgroups (`amce_diffs()`)
 - Diagnostics to choose feature reference categories, via `amce_by_reference()`
 - **ggplot2**-based visualizations of AMCEs and MMs, via `plot()` methods for all of the above

The package takes its name from the surname of a famous White House Press Secretary.

The main selling point of cregg is simplicity of implementation and - unlike the [cjoint](https://cran.r-project.org/package=cjoint) package - cregg tries to follow tidy data principles throughout and provides a formula-based interface that meshes well with the underlying [**survey**](https://cran.r-project.org/package=survey)-based effect estimation API. Thus the response from any function is a tidy data frame that can easily be stacked with others (e.g., for computing AMCEs for subsets of respondents and then producing a facetted or grouped visualization). It also tries to take better advantage of data preprocessing steps by:

 - Using factor base levels rather than trying to set baseline levels atomically
 - Using "label" attributes on variables to provide pretty printing

Additionally all functions have arguments in data-formula order, making it simple to pipe into them via `%>%`.

A detailed website showcasing package functionality is available at: https://thomasleeper.com/cregg/

Contributions and feedback are welcome on [GitHub](https://github.com/leeper/cregg/issues).

## Basic Code Examples




The package includes an example conjoint dataset (borrowed and lightly modified from the [cjoint](https://cran.r-project.org/package=cjoint) package), which is used here and and in examples:


```r
library("cregg")
data("hainmueller")
```

The package provides straightforward calculation and visualization of descriptive marginal means (MMs). These represent the mean outcome across all appearances of a particular conjoint feature level, averaging across all other features. In forced choice conjoint designs, MMs by definition average 0.5 with values above 0.5 indicating features that increase profile favorability and values below 0.5 indicating features that decrease profile favorability. For continuous outcomes, MMs can take any value in the full range of the outcome. Calculation of MMs entail no modelling assumptions are simply descriptive quantities of interest:


```r
# descriptive plotting
f1 <- ChosenImmigrant ~ Gender + Education + LanguageSkills + CountryOfOrigin + Job + JobExperience + JobPlans + ReasonForApplication + 
    PriorEntry
plot(mm(hainmueller, f1, id = ~CaseID), vline = 0.5)
```

![plot of chunk mmplot](figure/mmplot-1.png)

cregg functions uses `attr(data$feature, "label")` to provide pretty printing of feature labels, so that variable names can be arbitrary. These can be overwritten using the `feature_labels` argument to override these settings. Feature levels are always deduced from the `levels()` of righthand-side variables in the model specification. All variables should be factors with levels in desired display order. Similarly, the plotted order of features is given by the order of terms in the RHS formula unless overridden by the order of variable names given in `feature_order`.

A more common analytic approach for conjoints is to estimate average marginal component effects (AMCEs) using some form of regression analysis. cregg uses `glm()` and `svyglm()` to perform estimation and [margins](https://cran.r-project.org/package=margins) to generate average marginal effect estimates. Designs can be specified with any interactions between conjoint features but only AMCEs are returned. (No functionality is provided at the moment for explict estimation of feature interaction effects.) Just like for `mm()`, the output of `cj()` (or its alias, `amce()`) is a tidy data frame:


```r
# estimation
amces <- cj(hainmueller, f1, id = ~CaseID)
head(amces[c("feature", "level", "estimate", "std.error")], 20L)
```

```
                  feature                    level     estimate   std.error
1                  Gender                   Female  0.000000000          NA
2                  Gender                     Male -0.024978435 0.007983009
3  Educational Attainment                No Formal  0.000000000          NA
4  Educational Attainment                4th Grade  0.033367996 0.014971357
5  Educational Attainment                8th Grade  0.057600734 0.015021063
6  Educational Attainment              High School  0.119492095 0.015118761
7  Educational Attainment         Two-Year College  0.154588453 0.015840268
8  Educational Attainment           College Degree  0.180948898 0.016183493
9  Educational Attainment          Graduate Degree  0.169440741 0.015809304
10        Language Skills           Fluent English  0.000000000          NA
11        Language Skills           Broken English -0.056822717 0.011265303
12        Language Skills Tried English but Unable -0.127357585 0.011321142
13        Language Skills         Used Interpreter -0.159808492 0.011553933
14      Country of Origin                    India  0.000000000          NA
15      Country of Origin                  Germany  0.047646243 0.016667948
16      Country of Origin                   France  0.026564521 0.017327948
17      Country of Origin                   Mexico  0.009818758 0.017530373
18      Country of Origin              Philippines  0.034102011 0.017412416
19      Country of Origin                   Poland  0.032666819 0.017640451
20      Country of Origin                    China -0.020128816 0.017843819
```

This makes it very easy to modify, combine, print, etc. the resulting output. It also makes it easy to visualize using ggplot2. A convenience visualization function is provided:


```r
# plotting of AMCEs
plot(amces)
```

![plot of chunk plot_amce](figure/plot_amce-1.png)

To provide simple subgroup analyses, the `cj()` function provides a `by` argument to iterate over subsets of `data` and calculate AMCEs or MMs on each subgroup. For example, we may want to ensure that there are no substantial variations in preferences within-respondents across multiple conjoint decision tasks:


```r
mm_by <- cj(hainmueller, ChosenImmigrant ~ Gender + Education + LanguageSkills, id = ~CaseID, estimate = "mm", by = ~contest_no)
plot(mm_by, group = "contest_no", vline = 0.5)
```

![plot of chunk mm_by](figure/mm_by-1.png)

A more formal test of these differences is provided by a nested model comparison test:


```r
cj_anova(hainmueller, ChosenImmigrant ~ Gender + Education + LanguageSkills, by = ~contest_no)
```

```
Analysis of Deviance Table

Model 1: ChosenImmigrant ~ Gender + Education + LanguageSkills
Model 2: ChosenImmigrant ~ Gender + Education + LanguageSkills + contest_no + 
    Gender:contest_no + Education:contest_no + LanguageSkills:contest_no
  Resid. Df Resid. Dev Df Deviance      F Pr(>F)
1     13949     3353.0                          
2     13938     3349.6 11   3.3873 1.2814 0.2279
```

which provides a test of whether any of the interactions between the `by` variable and feature levels differ from zero.

Again, a detailed website showcasing package functionality is available at: https://thomasleeper.com/cregg/ and the content thereof is installed as a vignette. The package documentation provides further examples.

## Installation

[![CRAN](https://www.r-pkg.org/badges/version/cregg)](https://cran.r-project.org/package=cregg)
![Downloads](https://cranlogs.r-pkg.org/badges/cregg)
[![Travis Build Status](https://travis-ci.org/leeper/cregg.png?branch=master)](https://travis-ci.org/leeper/cregg)
[![Appveyor Build Status](https://ci.appveyor.com/api/projects/status/PROJECTNUMBER?svg=true)](https://ci.appveyor.com/project/leeper/cregg)
[![codecov.io](https://codecov.io/github/leeper/cregg/coverage.svg?branch=master)](https://codecov.io/github/leeper/cregg?branch=master)

This package is not yet on CRAN. To install the latest development version you can pull a potentially unstable version directly from GitHub:

```R
if (!require("remotes")) {
    install.packages("remotes")
}
remotes::install_github("leeper/cregg")
```
