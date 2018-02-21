# CHANGES TO cregg 0.1.5

* Added `props()` function to calculate display proportions for features or combinations of features (e.g., for examining constrained designs). Updated documentation accordingly. (#2)

# CHANGES TO cregg 0.1.4

* Added `amce_by_reference()` function to examine sensitivity of results to choice of reference category. (#2)

# CHANGES TO cregg 0.1.3

* Changed the `level` argument to `alpha` to avoid ambiguity with "levels" in the "feature level" sense used in the package (as opposed to the intended alpha or significance level).
* Added a `level_order` argument to `freqs()`, `mm()`, and `amce()` that specifies whether feature levels are ordered ascending in the output or descending. This is mostly only useful for plotting to specify whether the levels within each feature should be ordered with lower factor levels at the top ("ascending") or at the bottom ("descending") of the plot. (#1)
* `cj()` gains a `by` argument, which enables subgroup analyses, for example to investigate profile spillover effects or analyses by subsets of respondents. (#3)
* Added vignettes: "Introduction" and "Reproducing Hainmueller et al. (2014)". The latter is a work in progress. (#7)

# CHANGES TO cregg 0.1.2

* Changed name of `freq()` to `freqs()` and prefixed class names of return values from all functions with `cj_*`.
* Added `feature_order` argument to all functions to regulate display order.
* Fixed a bug in the handling of `header_fmt` in `plot()`.
* Updated README with example of `freqs()`.

# CHANGES TO cregg 0.1.1

* Initial release.
