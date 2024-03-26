use tree_sitter::Node;

/// Checks whether two nodes are equivalent.
///
/// We define two nodes to be equivalent if they have the same sort (kind) and
/// equivalent named fields.
///
/// TODO: Improve performance. Equivalence checks happen often so we want them to
/// be fast. The current implementation requires a traversal of the tree on all
/// named fields, which can be slow for large nodes. It also creates a cursor
/// at each traversal step.
///
/// Potential improvements:
/// 1. Use cursors that are passed as arguments -- not clear if this would be faster.
/// 2. Precompute hashes on all nodes, which define the equivalence relation. The check then becomes O(1).
pub fn are_equivalent(source1: &str, node1: &Node, source2: &str, node2: &Node) -> bool {
    // If the source is identical, we consider the nodes equivalent.
    // This covers most cases of constant nodes.
    // We may want a more precise check here eventually, but this is a good start.
    if source1[node1.start_byte() as usize..node1.end_byte() as usize]
        == source2[node2.start_byte() as usize..node2.end_byte() as usize]
    {
        return true;
    }

    // If the node kinds are different, then the nodes are not equivalent.
    // Common case, should go first.
    // TODO: this check is wrong, eg. we want to match
    // shorthand_property_identifier and identifier in js.
    // currently fixed by moving this check to after string matching, but
    // still not enough consider that a given snippet could be any one
    // of several nested nodes.
    if node1.kind_id() != node2.kind_id() {
        return false;
    }

    // If the node kinds are the same, then we need to check the named fields.
    let mut cursor1 = node1.walk();
    let mut cursor2 = node2.walk();
    let named_fields1 = node1.named_children(&mut cursor1);
    let named_fields2 = node2.named_children(&mut cursor2);

    // If the number of named fields is different, then the nodes are not equivalent.
    // This also covers the case of mistached optional and "multiple" fields.
    if named_fields1.len() != named_fields2.len() {
        return false;
    }

    // This is effectively a leaf node. If two leaf nodes have different sources (see above),
    // then they are not equivalent.
    // If they do not have the same sources, we consider them different.
    if named_fields1.len() == 0 {
        return false;
    }

    // And now recursing on the named fields.
    for (child1, child2) in named_fields1.zip(named_fields2) {
        if !are_equivalent(source1, &child1, source2, &child2) {
            return false;
        }
    }

    true
}
