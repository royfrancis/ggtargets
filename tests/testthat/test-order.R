test_that("generation order differs from alphabetical and depth-first order", {
  # a -> c, b -> c, a -> d: alphabetical would be a,b,c,d; depth-first from
  # a would visit a,c,... before b. The true generation order is a,b,d,c:
  # d reaches indegree 0 while processing a (gen0), before c reaches
  # indegree 0 while processing b, so d is queued ahead of c for gen1.
  # Cross-checked against networkx.topological_sort() directly.
  edges <- data.frame(from = c("a", "b", "a"), to = c("c", "c", "d"))
  nodes <- c("a", "b", "c", "d")
  expect_identical(metro_order(nodes, edges), c("a", "b", "d", "c"))
})

test_that("node insertion order breaks ties among indegree-0 nodes", {
  edges <- data.frame(from = character(0), to = character(0))
  expect_identical(metro_order(c("z", "a", "m"), edges), c("z", "a", "m"))
  expect_identical(metro_order(c("m", "a", "z"), edges), c("m", "a", "z"))
})

test_that("edge insertion order breaks ties among a node's children", {
  # a has children b and c, both freed in the same generation; the order
  # they enter that generation follows edge-table order, not node order.
  edges <- data.frame(from = c("a", "a"), to = c("c", "b"))
  nodes <- c("a", "b", "c")
  expect_identical(metro_order(nodes, edges), c("a", "c", "b"))

  edges2 <- data.frame(from = c("a", "a"), to = c("b", "c"))
  expect_identical(metro_order(nodes, edges2), c("a", "b", "c"))
})

test_that("isolated nodes and single-node graphs get a row each", {
  edges <- data.frame(from = character(0), to = character(0))
  expect_identical(metro_order(c("x", "y", "z"), edges), c("x", "y", "z"))
  expect_identical(metro_order("only", edges), "only")
})
