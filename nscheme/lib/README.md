# nScheme library

Avoid leaky abstractions: implement libraries that don't make strong assumptions and so don't have to compromise.  Users should get to decide all important tradeoffs.

For example, don't implement a one-size-fits-all dictionary.  Instead, implement specific data structures that could be used as dictionaries.

## TODO

* `ordered-assoc.scm`
* `array-mapped-trie.scm`
* `binary-search-tree.scm`
* `directed-graph.scm`
* `real-set.scm`
* `type-map.scm`
  * Define via define-vector-type to avoid magic constants.

* Linear equation solving
* Binary search on vectors

Implement lattice operations (join and meet) where applicable.
