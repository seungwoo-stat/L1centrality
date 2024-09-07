# L1centrality 0.2.1.9000

* Minor typo fix in the document.

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

