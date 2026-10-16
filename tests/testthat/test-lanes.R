test_that("freed columns are reused last-in, first-out", {
  # Two columns are freed in the same row, and the next node takes the one
  # freed last.
  #   a -> b, a -> c   a's column stays open until both have consumed it
  #   b -> d, c -> e   d and e sit in different columns
  #   d -> f, e -> f   f consumes both: d's column frees first, e's second
  # f must take e's column (the top of the stack of freed columns), not d's,
  # and must not open a new one.
  edges <- data.frame(
    from = c("a", "a", "b", "c", "d", "e"),
    to = c("b", "c", "d", "e", "f", "f")
  )
  nodes <- c("a", "b", "c", "d", "e", "f")
  order <- metro_order(nodes, edges)
  res <- metro_lanes(nodes, edges, order)

  col_of <- function(label) {
    res$cells$column[res$cells$label == label & res$cells$is_node]
  }
  expect_false(identical(col_of("d"), col_of("e")))
  expect_identical(col_of("f"), col_of("e"))
})

test_that("a node with out-degree 0 frees its column at once, for the next node", {
  # a -> b, a -> c, and d on its own. d has no incoming edge, so it is in a's
  # generation and the row order is a, d, b, c. d is a sink: the column it
  # opened (-1, the first one to the left of a's) is free again in the same
  # row, so b takes it rather than opening a new one.
  edges <- data.frame(from = c("a", "a"), to = c("b", "c"))
  nodes <- c("a", "b", "c", "d")
  order <- metro_order(nodes, edges)
  expect_identical(order, c("a", "d", "b", "c"))
  res <- metro_lanes(nodes, edges, order)

  col_of <- function(label) {
    res$cells$column[res$cells$label == label & res$cells$is_node]
  }
  expect_identical(col_of("a"), 0L)
  expect_identical(col_of("d"), -1L)
  expect_identical(col_of("b"), col_of("d"))
  # c consumes a's last edge, which frees a's column for c itself
  expect_identical(col_of("c"), col_of("a"))
  # so a third column was never opened
  expect_identical(res$columns, c(start = -2L))
})

test_that("metro_lanes() handles the empty graph", {
  res <- metro_lanes(
    character(0),
    data.frame(from = character(0), to = character(0)),
    character(0)
  )
  expect_equal(nrow(res$cells), 0)
  expect_identical(res$columns, c(start = 0L))
})
