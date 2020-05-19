# cregg

## cregg 0.4.0

* Fixed a bug related to variances for clustered designs (e.g., multiple responses per respondent) that was erroneously handling clusters. The bug arose from an unintended difference in calculating variances when designs were weighted versus not. All designs now estimate AMCEs using `svyglm()` for consistency. Weighted designs with weights provided to the `weight` argument of `cj()`, `amce()`, etc. were unaffected. Analysts using the package for unweighted designs should update results to obtain appropriately conservative standard errors of AMCEs. Tests of this behavior have now been added.
* Fixed a bug wherein `mm_diffs()` returned 90% rather than 95% confidence intervals. (# 37, h/t @hoellers)
* Fixed a bug in `mm()` wherein `alpha` was ignored.
* Fixed a bug in `mm_diffs()` wherein `h0` was ignored.
* Added an error when attempting to use `by` variables in `cj()` that contain empty character string levels. (#32, h/t Farsan Ghassim)
* `amce_by_reference()` now returns a `REFERENCE` column rather than a `BY` column, for clarity. (#35, h/t Farsan Ghassim)
* Documentation clarifies behavior of `mm_diffs()` to allow >= 2 levels for `by` factor. (#33, h/t Farsan Ghassim)
* `cj()` now explicitly requires that all variables in `by` are factor and ensures factor level ordering is carried forward from input data to output data.
* Added more informative error message when trying to plot by a missing grouping variable.
* Added additional examples of plotting behavior, including ggplot2 fill/color scales and theming.

## cregg 0.3.1

* Fixed issue in `mm_diffs()` wherein a `by` variable that shared levels with a feature variable led to incorrect estimates being returned. (#22, h/t Michael Jankowski)
* A warning is now issued when levels are not unique across feature. (#13, h/t Chris Wratil)
* Updated documentation with various fixes (#19, #20, #21, #23, h/t Matthew Barnfield, Evgeniia Mitrokhina)
* Added error when feature variables are not factors. (#12, #25, Maciej Szymkiewicz)
* Added documentation of how to replace colour scale with shape scale in `plot()` methods. (#18, h/t Mia Costa)
* Added documentation of how to print estimates on the `plot()` method output. (#14)
* Make `cj_tidy()` work with tibbles. (#11, Umberto Mignozzetti)

## cregg 0.3.0

* CRAN release.

## cregg 0.2.4

* `cj()` now imposes class "cj_df" on `data` to preserve attributes during subsetting.

## cregg 0.2.3

* Added function `cj_table()`, which can be useful in communicating the set of features and levels used in the design as a data frame (e.g,. using `knitr::kable(cj_table(data, ~ feat1 + feat2))`).
* Renamed functions `props()` -> `cj_props()` and `freqs()` to `cj_freqs()` for API consistency.

## cregg 0.2.2

* Added function `cj_df()`, which provides a modified data frame class ("cj_df") that preserves variable "label" attributes when subsetting.
* Built-in datasets `immigration` and `taxes` gain a "cj_df" class.
* `cj_tidy()` now returns objects of class `c("cj_df", "data.frame")`.

## cregg 0.2.1

* Added function `cj_tidy()` to tidy a "wide" respondent-length conjoint dataset into a "long" respondent*task*alternative-length dataset. An example dataset, `wide_conjoint`, is provided for examples and testing.

## cregg 0.2.0

* First stable release.
* Completed functionality of `amce_diffs()`, limiting it to work with unconstrained designs. (#6)
* Added tests for accuracy of AMCEs in two-way constrained and fully unconstrained designs.

## cregg 0.1.14

* Added support for constrained designs (when two-way constraints are present). (#6)
* Removed **margins** dependency, leaving only linear probability model support.

## cregg 0.1.13

* Added another example datast, `taxes`, from Ballard-Rosa et al. (2016).
* Renamed `hainmueller` dataset to `immigration`.

## cregg 0.1.12

* Expanded test suite to cover survey-weighted data. Note: `cj_anova()` currently does not work with weighted data due to a bug in `survey::anova.svyglm()`.
* Cleaned up internal code for consistency.
* Added 'statistic' column to function outputs.

## cregg 0.1.11

* Variances returend by `amce_diffs()` now respect clustering. (#9)

## cregg 0.1.10

* Fixed a factor ordering issue in `mm_diffs()`.
* Added tests of numeric accuracy of estimates for all main functions.

## cregg 0.1.9

* Added new function `mm_diffs()` for calculating differences in marginal means.

## cregg 0.1.8

* `mm()` gains an `h0` argument to specify a null hypothesis values so that z statistics and p-values are meaningful.
* Cleaned up documentation and expanded 'Introduction' vignette, moving README content to there.
* Require **survey** version 0.33 (for `family` argument).
* Added a basic test suite. (#4)

## cregg 0.1.7

* Added `amce_diffs()` and `amce_anova()` functions to assess differences in AMCEs by a grouping variable.
* Removed some grouping warnings from `plot()` methods. (#8)

## cregg 0.1.6

* Fixed a bug in the creation of `svydesign()` objects that was generating incorrect variance estimates.
* Fully imported **ggplot2** and **ggstance**.

## cregg 0.1.5

* Added `props()` function to calculate display proportions for features or combinations of features (e.g., for examining constrained designs). Updated documentation accordingly. (#2)
* Expanded Introduction vignette with examples of a number of diagnostics. (#2)

## cregg 0.1.4

* Added `amce_by_reference()` function to examine sensitivity of results to choice of reference category. (#2)

## cregg 0.1.3

* Changed the `level` argument to `alpha` to avoid ambiguity with "levels" in the "feature level" sense used in the package (as opposed to the intended alpha or significance level).
* Added a `level_order` argument to `freqs()`, `mm()`, and `amce()` that specifies whether feature levels are ordered ascending in the output or descending. This is mostly only useful for plotting to specify whether the levels within each feature should be ordered with lower factor levels at the top ("ascending") or at the bottom ("descending") of the plot. (#1)
* `cj()` gains a `by` argument, which enables subgroup analyses, for example to investigate profile spillover effects or analyses by subsets of respondents. (#3)
* Added vignettes: "Introduction" and "Reproducing Hainmueller et al. (2014)". The latter is a work in progress. (#7)

## cregg 0.1.2

* Changed name of `freq()` to `freqs()` and prefixed class names of return values from all functions with `cj_*`.
* Added `feature_order` argument to all functions to regulate display order.
* Fixed a bug in the handling of `header_fmt` in `plot()`.
* Updated README with example of `freqs()`.

## cregg 0.1.1

* Initial release.
