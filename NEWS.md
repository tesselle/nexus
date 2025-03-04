# nexus 0.4.0.9000
## Enhancements
* `barplot()` gained a new `names` argument to hide/display row names.
* `mean()` and `condense()` gained two new arguments `ignore_na` and `ignore_zero`.
* `as.data.frame()` methods for `LogRatio` and `GroupedLogRatio` class return a `data.frame` with variable labels.
* Translate into French.

## Breaking changes
* Rename `group_extract()` to `group_subset()`.

# nexus 0.4.0
## New classes and methods
* Add `ReferenceGroups`, `GroupedComposition` and `GroupedLogRatio` classes to represent grouped data.
* Add `group()`, `ungroup()`, `group_levels()`, `group_names()`, `group_indices()`, `group_rows()`, `group_length()`, `group_size()`, `group_extract()`, `group_split()` and `is_grouped()` to work with grouped data.
* Add `transform_lr()`, `transform_clr()`, `transform_alr()`, `transform_ilr()`, `transform_plr()` and `transform_inverse()` methods for `GroupedComposition` and `GroupedLogRatio` objects.
* Add `is_composition()`, `is_logratio()` and `is_grouped()` to check if an object is from a specific class.
* Add `pairs()` to display a matrix of ternary plots.

## Enhancements
* `as_composition()` gained a new `autodetect` argument to enable/disable automatic detection of numeric variables.

## Breaking changes
* Redesign the internal mechanism for grouping data.
* `hist()` now produces a single histogram.
* `plot(<LogRatio>)` produces a scatter plot instead of a density plot.
* Rename the `plot()` method for the `CompositionMatrix` class to `pairs()`.

# nexus 0.3.0
## New classes and methods
* Add transformation methods for `LogRatio` objects (clr <-> alr, clr -> ilr, alr -> ilr).
* Add `condense()` to compute compositional mean of data subsets.
* Add `pip()` to compute proportionality index of parts.
* Add `rbind()` method for `CompositionMatrix` objects.
* Add `show()` methods for `LogRatio` objects.
* Add `is_element_major()`, `is_element_minor()`, `is_element_trace()` and `is_oxide()` methods for `CompositionMatrix` objects.
* Add `variance()` and `variance_total()` to compute (total) variance.

## Enhancements
* Add example datasets.
* Improve bar chart rendering.

## Bugfixes & changes
* Fix `pca()` for `LogRatio` objects (default arguments were not passed to the internal method).

## Internals
* Store groups as `factor` instead of `character`.

## Breaking changes
* `[` always returns a `CompositionMatrix` object by default, even if only one row/column is accessed.
* Remove `samples` slot in all classes.
* Remove `as_features()`.
* Remove `metrix_var()` and `metric_sd()`.
* Remove the `base` argument of `transform_ilr()`.
* Rename `get_totals()` and `set_totals()` to `totals()`.
* Rename `get_groups()` and `get_groups()` to `groups()`.
* Rename `outliers()` to `detect_outlier()` and redefine methods.
* Rewrite `plot()` method for `OutlierIndex` object.

# nexus 0.2.0
## New classes and methods
* Add `hist()` methods to plot ILR histograms.
* Add `quantile()` methods for compositional data matrix.
* Add `split()` methods to divide into groups.

## Enhancements
* `transform_ilr()` gained a new argument allowing the specify the base of the transformation.
* `barplot()` gained a new argument to display a legend.

## Internals
* List **dimensio** in `Depends`.

# nexus 0.1.0

* First release.
