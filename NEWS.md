# L1centrality 0.0.3

* Minor typo fix in the document.

* `L1centLOC()` and `L1centEDGE()` made more efficient.

* `\()` changed to `function()` for compatibility (`L1centMDS.matrix()`).

* The vertex multiplicity parameter `eta` generalized to nonnegative values (`L1cent()`, `L1centEDGE()`, `L1centLOC()`, `L1centNB()`). It previously accepted only positive values.

# L1centrality 0.0.2

* In `L1centMDS()`, setting `verbose=FALSE` suppresses all the console messages. In the previous version, the final message was printed even when `verbose=FALSE`.

# L1centrality 0.0.1

* Initial CRAN submission.

