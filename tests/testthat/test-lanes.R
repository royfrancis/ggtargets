test_that("freed columns are reused last-in, first-out", {
  # a has two children b, c (opens columns for b then c... no, out-degree
  # of a is 2, so a's own column stays open until both are consumed).
  # Build a graph where two columns free in the same row, then the next
  # node should take the most recently freed one.
  #   a -> b, a -> c   (a's lane, out-degree 2)
  #   b -> d           (b gets its own column, freed when d consumes it)
  #   c -> e           (c gets its own column, freed when e consumes it)
  #   d, e both consume in row order d then e: d frees b's column first,
  #   e frees c's column second. A node with two predecessors d and e,
  #   consumed in that row, should reuse c's column (last freed) first.
  edges <- data.frame(
    from = c("a", "a", "b", "c", "d", "e"),
    to = c("b", "c", "d", "e", "f", "f")
  )
  nodes <- c("a", "b", "c", "d", "e", "f")
  order <- metro_order(nodes, edges)
  res <- metro_lanes(nodes, edges, order)

  # f has two predecessors (d, e); d's column frees first, then e's frees
  # second, so map_node for the *next* new node (there is none here) would
  # take e's column first. Verify directly: f itself reuses the more
  # recently freed of d's/e's own columns (whichever was freed last during
  # f's own add_input processing) rather than opening a brand new column.
  f_row <- res$cells[res$cells$label == "f" & res$cells$is_node, ]
  d_col <- res$cells[res$cells$label == "d" & res$cells$is_node, "column"]
  e_col <- res$cells[res$cells$label == "e" & res$cells$is_node, "column"]
  expect_true(f_row$column %in% c(d_col, e_col))
  # Predecessors of f are processed in edge order d, e: d's column frees
  # first (pushed first), e's frees second (pushed last, top of stack), so
  # f (the only thing placed afterwards) takes e's column.
  expect_identical(f_row$column, e_col)
})

test_that("a node with out-degree 0 frees its column in its own row", {
  # a -> b, a -> c (c is a sink, out-degree 0); c's column becomes free
  # immediately, and a later independent node reuses it in the same graph.
  edges <- data.frame(from = c("a", "a"), to = c("b", "c"))
  nodes <- c("a", "b", "c", "d")
  # d is unrelated (indegree 0, own generation); since it has no incoming
  # edges it's placed in row order after a's generation exhausts, and can
  # reuse c's freed column only if it comes after c in row order.
  order <- metro_order(nodes, edges)
  res <- metro_lanes(nodes, edges, order)
  c_cell <- res$cells[res$cells$label == "c" & res$cells$is_node, ]
  expect_identical(c_cell$remaining, 0L)
})

test_that("metro_lanes handles the empty graph", {
  res <- metro_lanes(
    character(0),
    data.frame(from = character(0), to = character(0)),
    character(0)
  )
  expect_equal(nrow(res$cells), 0)
  expect_identical(res$columns, c(start = 0L))
})
