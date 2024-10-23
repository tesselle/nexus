# nexus 0.3.0.9000
## Breaking changes
* `hist()` now produces a single histogram.

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
