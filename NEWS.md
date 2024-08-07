# nexus 0.2.0.9000
## New classes and methods
* Add `condense()` to compute compositional mean of data subsets.
* Add `pip()` to compute proportionality index of parts.
* Add `show()` methods for `LogRatio-class` objects.

## Breaking changes
* `[` always returns a `CompositionMatrix` object by default, even if only one row/column is accessed.
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
