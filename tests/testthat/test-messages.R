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
})

test_that("metro_palette() messages", {
  expect_snapshot(error = TRUE, metro_palette(-1))
  expect_snapshot(error = TRUE, metro_palette(c(1, 2)))
  expect_snapshot(error = TRUE, metro_palette(NA))
  expect_snapshot(error = TRUE, metro_palette(1.9))
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
})

test_that("tar_dag()/tar_ggplot() outside a targets project message", {
  skip_if_not_installed("targets")
  empty_dir <- withr::local_tempdir()
  withr::with_dir(empty_dir, {
    expect_snapshot(error = TRUE, tar_dag())
  })
})
