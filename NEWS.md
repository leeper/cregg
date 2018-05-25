# cregg 0.1.12

* Expanded test suite to cover survey-weighted data. Note: `cj_anova()` currently does not work with weighted data due to a bug in `survey::anova.svyglm()`.
* Cleaned up internal code for consistency.
* Added 'statistic' column to function outputs.

# cregg 0.1.11

* Variances returend by `amce_diffs()` now respect clustering. (#9)

# cregg 0.1.10

* Fixed a factor ordering issue in `mm_diffs()`.
* Added tests of numeric accuracy of estimates for all main functions.

# cregg 0.1.9

* Added new function `mm_diffs()` for calculating differences in marginal means.

# cregg 0.1.8

* `mm()` gains an `h0` argument to specify a null hypothesis values so that z statistics and p-values are meaningful.
* Cleaned up documentation and expanded 'Introduction' vignette, moving README content to there.
* Require **survey** version 0.33 (for `family` argument).
* Added a basic test suite. (#4)

# cregg 0.1.7

* Added `amce_diffs()` and `amce_anova()` functions to assess differences in AMCEs by a grouping variable.
* Removed some grouping warnings from `plot()` methods. (#8)

# cregg 0.1.6

* Fixed a bug in the creation of `svydesign()` objects that was generating incorrect variance estimates.
* Fully imported **ggplot2** and **ggstance**.

# cregg 0.1.5

* Added `props()` function to calculate display proportions for features or combinations of features (e.g., for examining constrained designs). Updated documentation accordingly. (#2)
* Expanded Introduction vignette with examples of a number of diagnostics. (#2)

# cregg 0.1.4

* Added `amce_by_reference()` function to examine sensitivity of results to choice of reference category. (#2)

# cregg 0.1.3

* Changed the `level` argument to `alpha` to avoid ambiguity with "levels" in the "feature level" sense used in the package (as opposed to the intended alpha or significance level).
* Added a `level_order` argument to `freqs()`, `mm()`, and `amce()` that specifies whether feature levels are ordered ascending in the output or descending. This is mostly only useful for plotting to specify whether the levels within each feature should be ordered with lower factor levels at the top ("ascending") or at the bottom ("descending") of the plot. (#1)
* `cj()` gains a `by` argument, which enables subgroup analyses, for example to investigate profile spillover effects or analyses by subsets of respondents. (#3)
* Added vignettes: "Introduction" and "Reproducing Hainmueller et al. (2014)". The latter is a work in progress. (#7)

# cregg 0.1.2

* Changed name of `freq()` to `freqs()` and prefixed class names of return values from all functions with `cj_*`.
* Added `feature_order` argument to all functions to regulate display order.
* Fixed a bug in the handling of `header_fmt` in `plot()`.
* Updated README with example of `freqs()`.

# cregg 0.1.1

* Initial release.
