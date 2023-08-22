### Red-black tree

- [x] join and split
- [x] optimize join+split process (no reallocations)
- [x] black height
- [x] allow that root is red
- [x] union  (with test)
- [ ] insert and delete tests

- [ ] tide-up function names, consistent with list names
- [ ] example of simple tree usage

#####Set operations

- [ ] test intersection/difference
- [ ] operations are destructive, show an example how a copy
      can be made easily
- [ ] maybe overload operators + * - for union, intersection and differenve
- [ ] encapsulation as tree/tree operations?
- [ ] tidy-up code
- [ ] join-based insert / delete ??
- [x] import/export of data to/from an array
- [ ] filter
- [ ] build from unsorted array (import)
- [ ] import/export to file (directly or implement save/load of arrays in common_mod)

A tree module with a linear allocator
- [ ] basic implementation and speed comparisons

## Naming methods suggestions

- unlink_root, link_node
- insert(value)
- erase(value)
- erase(iterator)
- find(value)
- begin() / end() --> iterators get_leftmost
- size(), empty()
- lower_bound(value) --> first element not less than value
- upper_bound(value)
- clear() -> remove all elements from the set
- copy_tree, free_tree
- isvalid()
- read() / update() / 
