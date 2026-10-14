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

test_that("print.dag_layout() output for a dag with no edges", {
  d <- as_dag(
    data.frame(from = character(0), to = character(0)),
    nodes = data.frame(name = c("x", "y"), stringsAsFactors = FALSE)
  )
  lay <- dag_layout(d, style_metro())
  expect_snapshot(print(lay))
})
