### Module `wavl_tree_type` (Template)
This module defines the `wavl_tree` derived type which implements a core
reference implementation of a weak AVL (WAVL) binary search tree with
bottom-up rebalancing [1]. It uses integer keys and integer values, and
includes only the basic methods of insertion, deletion, and lookup. The
module also defines a companion `wavl_tree_iterator` derived type for
in-order iteration of the nodes of the tree.

While this is a compilable and testable module, it is intended to serve
*only* as a template, or starting point, for creating custom containers
that use a binary search tree as the internal data structure; e.g., set
and map. Simply copy the module and modify it to suit the specific need.

[1] Haeupler, B., Sen, S., and Tarjan, S. E. 2015. Rank-Balanced Trees.
    ACM Trans. Algorithms 11, 4. DOI:https://doi.org/10.1145/2689412
