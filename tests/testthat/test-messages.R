# Every error/warning the package raises, via expect_snapshot(), so a
# message change is a deliberate diff, not an accident.

test_that("as_dag() messages", {
  expect_snapshot(error = TRUE, as_dag(data.frame(from = "a", to = "a")))
  expect_snapshot(
    error = TRUE,
    as_dag(data.frame(from = c("a", "b", "c"), to = c("b", "c", "a")))
  )
  expect_snapshot(error = TRUE, as_dag(matrix(1:4, 2, 2)))
  expect_snapshot(error = TRUE, as_dag(list(foo = 1)))
  expect_snapshot(error = TRUE, as_dag(data.frame(a = 1)))
  expect_snapshot(
    error = TRUE,
    as_dag(data.frame(from = 1, to = 2), nodes = data.frame(x = 1))
  )
})

test_that("as_dag() node-name validation messages", {
  expect_snapshot(
    error = TRUE,
    as_dag(data.frame(from = c("a", NA), to = c("b", "c")))
  )
  expect_snapshot(
    error = TRUE,
    as_dag(data.frame(from = c("a", "b"), to = c("b", NA)))
  )
  expect_snapshot(
    error = TRUE,
    as_dag(
      data.frame(from = "a", to = "b"),
      nodes = data.frame(name = c("a", "b", NA))
    )
  )
  expect_snapshot(
    error = TRUE,
    as_dag(data.frame(from = c("a|b", "a"), to = c("c", "b|c")))
  )
  expect_snapshot(error = TRUE, as_dag(data.frame(from = "a\rb", to = "c")))
  expect_snapshot(
    error = TRUE,
    as_dag(data.frame(from = c("a", ""), to = c("b", "c")))
  )
})

test_that("metro_palette() messages", {
  expect_snapshot(error = TRUE, metro_palette(-1))
  expect_snapshot(error = TRUE, metro_palette(c(1, 2)))
  expect_snapshot(error = TRUE, metro_palette(NA))
  expect_snapshot(error = TRUE, metro_palette(1.9))
  # beyond the 18 base colors, when no extra color can be told apart (as in
  # test-palette.R: every color is made identical, so the search gives up)
  testthat::local_mocked_bindings(
    lab_of = function(hex) matrix(0, nrow = length(hex), ncol = 3)
  )
  expect_snapshot(error = TRUE, metro_palette(19))
})

test_that("style_metro() unknown argument message", {
  expect_snapshot(error = TRUE, style_metro(not_a_real_arg = 1))
})

test_that("resolve_node_color()/resolve_edge_color() messages", {
  nodes <- data.frame(
    name = "a",
    row = 0,
    color_index = 0,
    stringsAsFactors = FALSE
  )
  expect_snapshot(
    error = TRUE,
    resolve_node_color(nodes, "not_a_column_or_color", metro_palette)
  )
  expect_snapshot(
    error = TRUE,
    resolve_node_color(
      rbind(nodes, data.frame(name = "b", row = 1, color_index = 1)),
      NULL,
      "red"
    )
  )

  edge_paths <- data.frame(
    from = "a",
    to = "b",
    piece = "vline",
    stringsAsFactors = FALSE
  )
  expect_snapshot(
    error = TRUE,
    resolve_edge_color(edge_paths, "not_a_color", c(a = "red", b = "green"))
  )

  expect_snapshot(resolve_value_palette(c("x", "y"), c(x = "red")))
  expect_snapshot(
    error = TRUE,
    resolve_value_palette(c("x", "y", "z"), c("red", "blue"))
  )
  expect_snapshot(
    error = TRUE,
    resolve_value_palette(c("x", "y", "z"), function(n) c("red", "blue"))
  )

  # not a column name or a single color, and `NA`, which is no color either
  expect_snapshot(error = TRUE, resolve_node_color(nodes, 123, metro_palette))
  expect_snapshot(
    error = TRUE,
    resolve_node_color(nodes, NA_character_, metro_palette)
  )
  expect_snapshot(
    error = TRUE,
    resolve_edge_color(edge_paths, 123, c(a = "red", b = "green"))
  )
  expect_snapshot(
    error = TRUE,
    resolve_edge_color(edge_paths, NA_character_, c(a = "red", b = "green"))
  )
})

test_that("resolve_node_shape() messages", {
  nodes <- data.frame(
    name = c("a", "b"),
    type = c("stem", "other"),
    stringsAsFactors = FALSE
  )
  shapes <- targets_node_shapes()
  expect_snapshot(error = TRUE, resolve_node_shape(nodes, "hexagon", shapes))
  expect_snapshot(error = TRUE, resolve_node_shape(nodes, 1, shapes))
  expect_snapshot(
    error = TRUE,
    resolve_node_shape(nodes, NULL, c(stem = "hexagon"))
  )
  expect_snapshot(error = TRUE, resolve_node_shape(nodes, NULL, 21))
  expect_snapshot(error = TRUE, resolve_node_shape(nodes, "type", "circle"))
  # a column named in node_shape, with a value that `shapes` doesn't name
  expect_snapshot(invisible(resolve_node_shape(nodes, "type", shapes)))
  # ...and the same error, reached from the plotting functions
  expect_snapshot(
    error = TRUE,
    ggplot_dag(as_dag(data.frame(from = "a", to = "b")), node_shape = "hexagon")
  )
})

test_that("ggsave_dag() no-viewbox message", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  expect_snapshot(
    error = TRUE,
    ggsave_dag(withr::local_tempfile(fileext = ".png"), p)
  )
})

test_that("ggsave_dag()'s scale= and dag_layers()'s zoom= messages", {
  # `scale` becomes ggsave()'s dpi, and a negative dpi crashes the png
  # device outright, so this guard is load-bearing, not cosmetic
  d <- as_dag(data.frame(from = "a", to = "b", stringsAsFactors = FALSE))
  p <- ggplot_dag(d)
  expect_snapshot(
    error = TRUE,
    ggsave_dag(withr::local_tempfile(fileext = ".png"), p, scale = -1)
  )
  expect_snapshot(
    error = TRUE,
    dag_layers(dag_layout(d, style_metro()), zoom = -1)
  )
  # `zoom` scales the fixed panel size, so without one it is ignored, with a
  # warning
  expect_snapshot(invisible(ggplot_dag(d, fixed_size = FALSE, zoom = 2)))
})

test_that("dag_layout() input message", {
  edges <- data.frame(from = "a", to = "b")
  expect_snapshot(error = TRUE, dag_layout(edges))
  # ...and the same error, reached from the plotting function
  expect_snapshot(error = TRUE, ggplot_dag(edges))
})

test_that("tar_dag() message when targets is not installed", {
  testthat::local_mocked_bindings(targets_installed = function() FALSE)
  expect_snapshot(error = TRUE, tar_dag())
})

test_that("dag_collapse() messages", {
  d <- stage_dag()
  expect_snapshot(error = TRUE, dag_collapse(data.frame(from = "a", to = "b"), "g"))
  expect_snapshot(error = TRUE, dag_collapse(d, 1))
  expect_snapshot(error = TRUE, dag_collapse(d, c("stage", "type")))
  expect_snapshot(error = TRUE, dag_collapse(d, "nope"))
  expect_snapshot(error = TRUE, dag_collapse(d, "stage", label_n = NA))
  # X -> Y -> X: the first hop is two edges (a -> b, d -> e), the second one
  loop <- as_dag(
    data.frame(from = c("a", "b", "c", "d"), to = c("b", "c", "d", "e")),
    nodes = data.frame(
      name = c("a", "b", "c", "d", "e"),
      group = c("X", "Y", "X", "X", "Y")
    )
  )
  expect_snapshot(error = TRUE, dag_collapse(loop, "group"))
  # a value that is also the name of a node left alone
  clash <- as_dag(
    data.frame(from = "a", to = "b"),
    nodes = data.frame(name = c("a", "b"), group = c("b", NA))
  )
  expect_snapshot(error = TRUE, dag_collapse(clash, "group"))
  bad <- as_dag(
    data.frame(from = "a", to = "b"),
    nodes = data.frame(name = c("a", "b"), group = c("x|y", "z\rw"))
  )
  expect_snapshot(error = TRUE, dag_collapse(bad, "group"))
})

test_that("node_size and edge_width messages", {
  d <- dag_collapse(stage_dag(), "stage")
  expect_snapshot(error = TRUE, ggplot_dag(d, node_size = "nope"))
  expect_snapshot(error = TRUE, ggplot_dag(d, node_size = "stage"))
  expect_snapshot(error = TRUE, ggplot_dag(d, node_size = 1))
  expect_snapshot(error = TRUE, ggplot_dag(d, node_size = c("n", "n")))
  expect_snapshot(error = TRUE, ggplot_dag(d, edge_width = "nope"))
  expect_snapshot(error = TRUE, ggplot_dag(d, edge_width = TRUE))
  d$edges$kind <- c("x", "y")
  expect_snapshot(error = TRUE, ggplot_dag(d, edge_width = "kind"))
  # a column of the nodes is not one of the edges
  expect_snapshot(error = TRUE, ggplot_dag(d, edge_width = "stage"))
})

test_that("style_metro()'s node_size_range and edge_width_range messages", {
  expect_snapshot(error = TRUE, style_metro(node_size_range = 5))
  expect_snapshot(error = TRUE, style_metro(node_size_range = c(8, 4)))
  expect_snapshot(error = TRUE, style_metro(node_size_range = c(0, 4)))
  expect_snapshot(error = TRUE, style_metro(node_size_range = c(4, 11)))
  expect_snapshot(error = TRUE, style_metro(node_size_range = c("a", "b")))
  expect_snapshot(error = TRUE, style_metro(node_size_range = c(4, NA)))
  expect_snapshot(error = TRUE, style_metro(edge_width_range = c(1, 6)))
  expect_snapshot(error = TRUE, style_metro(scale = 20, edge_width_range = c(1, 11)))
})

test_that("reserved node column messages", {
  edges <- data.frame(from = "a", to = "b")
  expect_snapshot(
    error = TRUE,
    as_dag(edges, nodes = data.frame(name = c("a", "b"), x = 1:2))
  )
  # several, in the order of the table, among others that are fine
  expect_snapshot(
    error = TRUE,
    as_dag(
      edges,
      nodes = data.frame(
        name = c("a", "b"),
        y = 1:2,
        kind = "k",
        color_index = 1:2
      )
    )
  )
  # a dag edited after as_dag() is caught when it is laid out
  d <- as_dag(edges)
  d$nodes$row <- 1:2
  expect_snapshot(error = TRUE, dag_layout(d))
})
