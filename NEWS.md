# L1centrality 0.3.1.9000

* Added three relevant papers to the documentations, DESCRIPTION, and README files.

* Generated a CITATION file.

# L1centrality 0.3.1

* `print.L1centNB()` can now handle a graph with a missing vertex name (i.e., `V(g)$name == NULL`). If vertex names are missing, the function automatically sets them to `V1`, `V2`, ..., and prints them.

* The `type` argument is set to 1 in the `quantile()` function inside the `L1centLOC()` and `L1centEDGE()` functions.

# L1centrality 0.3.0

## Major updates

* A new function `Gini()` computes the Gini coefficient. (See the document named `Heterogeneity`.)

* `L1cent()` now returns an object of class `L1cent`. `summary()` and `print()` methods are implemented for this new class.

* `L1centGROUP()` now returns an object of class `L1centGROUP`. `print()` method is implemented for this new class.

* `L1centLOC()` now returns an object of class `L1centLOC`. `summary()` and `print()` methods are implemented for this new class.

* `L1centNB()` now returns an object of class `L1centNB`. `summary()` and `print()` methods are implemented for this new class.

* `L1centEDGE()` now returns an object of class `L1centEDGE`. `summary()` and `print()` methods are implemented for this new class.

* Implementation of `L1centEDGE()` corrected.

* `print()` methods is implemented for the `L1centMDS` class. The `L1centMDS()` function formerly returned a length four list with label of the vertices as one component. This component is now an attribute of the returned list, i.e., the `L1centMDS()` now returns a length three list. (See the document for `L1centMDS()`.) As a result, the `plot()` method for the `L1centMDS` is modified as well.

## Minor updates

* Minor typo fix in the document.

* New document 'Heterogeneity' added. It includes the `Lorenz_plot()` function and the `Gini()` function.

* New document 'Summary' added. It includes all `summary()` method in this package. 

# L1centrality 0.2.1

* arXiv URL added to the package description and function documents.

* Minor typo fix in the document.

# L1centrality 0.2.0

## Major updates

* A new function `group_reduce()` constructs the group reduced graph, which is used in computing the group L1 centrality and group L1 prestige.

* A new function `L1centGROUP()` computes the group L1 centrality and group L1 prestige.  It supports undirected and directed graphs, as well as vertex and edge weighted graphs.

## Minor updates

* Error message of `validate_matrix()` (an internal function for validating the given graph) corrected.

* DESCRIPTION and the documentation `L1centrality-package` updated to include information on the group L1 centrality/prestige.

* Minor typo fix in the document.

# L1centrality 0.1.1

## Major updates

* `L1cent()`, `L1centNB()`, `L1centLOC()` now support (strongly connected) directed graphs. Use the new parameter `mode` to measures the prominence of each vertex in terms of *making* a choice (`mode = "centrality"`) or *receiving* a choice (`mode = "prestige"`). If the graph is undirected, `mode` does not affect the outcome. 

## Minor updates

* Minor typo fix in the document.

* `\()` changed to `function()` for compatibility (`L1centNB.matrix()`).

* Fixed error in the `L1centLOC()` function.

* Added package overview table to the `L1centrality-package` document.

* arXiv URL added to the package description and function documents.

# L1centrality 0.0.3

* Minor typo fix in the document.

* `L1centLOC()` and `L1centEDGE()` made more efficient.

* `\()` changed to `function()` for compatibility (`L1centMDS.matrix()`).

* The vertex multiplicity parameter `eta` generalized to nonnegative values (`L1cent()`, `L1centEDGE()`, `L1centLOC()`, `L1centNB()`). It previously accepted only positive values.

# L1centrality 0.0.2

* In `L1centMDS()`, setting `verbose=FALSE` suppresses all the console messages. In the previous version, the final message was printed even when `verbose=FALSE`.

# L1centrality 0.0.1

* Initial CRAN submission.

