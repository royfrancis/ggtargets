test_that("print.dag() output", {
  d <- as_dag(data.frame(
    from = c("a", "a", "b"),
    to = c("b", "c", "c"),
    stringsAsFactors = FALSE
  ))
  expect_snapshot(print(d))
})

test_that("print.dag_layout() output", {
  d <- as_dag(data.frame(
    from = c("a", "a", "b", "c"),
    to = c("b", "c", "d", "d"),
    stringsAsFactors = FALSE
  ))
  lay <- dag_layout(d, style_metro())
  expect_snapshot(print(lay))
})

test_that("the one-line summaries say \"1 node\", not \"1 nodes\"", {
  one <- as_dag(
    data.frame(from = character(0), to = character(0)),
    nodes = data.frame(name = "x", stringsAsFactors = FALSE)
  )
  expect_identical(format(one), "<dag> 1 node, 0 edges")
  expect_identical(
    format(dag_layout(one, style_metro())),
    "<dag_layout> 1 node, 0 edges, 1 column"
  )
  chain <- as_dag(data.frame(from = "a", to = "b"))
  expect_identical(format(chain), "<dag> 2 nodes, 1 edge")
  expect_identical(
    format(dag_layout(chain, style_metro())),
    "<dag_layout> 2 nodes, 1 edge, 1 column"
  )
  # a -> b, a -> c: b opens a second column
  fork <- as_dag(data.frame(from = c("a", "a"), to = c("b", "c")))
  expect_identical(
    format(dag_layout(fork, style_metro())),
    "<dag_layout> 3 nodes, 2 edges, 2 columns"
  )
})

test_that("print.dag_layout() output for a dag with no edges", {
  d <- as_dag(
    data.frame(from = character(0), to = character(0)),
    nodes = data.frame(name = c("x", "y"), stringsAsFactors = FALSE)
  )
  lay <- dag_layout(d, style_metro())
  expect_snapshot(print(lay))
})
