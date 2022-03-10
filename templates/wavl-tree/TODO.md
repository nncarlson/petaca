### To-Do's

* Unit tests, especially those that verify that the rebalancing conforms to
  the WAVL rules. The "tests" originally used during development have been
  lost unfortunately.

* Would a recursion-less implementation perform significantly better? The
  implementation would be complicated by the required rebalancing that needs
  to walk (partway) back up the tree.

* Investigate the cost/benefit of maintaining a parent pointer component of
  the `rbt_node` derived type. It would trade the need for the iterator to
  maintain a node stack (using O(log(n)) storage) for O(n) of storage plus the
  cost of maintaining the parent pointer during insertion/deletion/rebalancing.

* Tree traversal methods (inorder, preorder, postorder) with visitor procedure.
