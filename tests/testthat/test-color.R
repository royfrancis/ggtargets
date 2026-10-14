# Section 6's color resolution rules, tested directly against
# resolve_node_color()/resolve_edge_color()/resolve_value_palette().

test_that("resolve_value_palette: character column, order of first appearance", {
  res <- resolve_value_palette(c("b", "a", "b", "c"), metro_palette)
  expect_identical(names(res$map), c("b", "a", "c"))
  expect_identical(res$colors, unname(res$map[c("b", "a", "b", "c")]))
})

test_that("resolve_value_palette: factor column, level order (not first-appearance)", {
  f <- factor(c("b", "a", "b", "c"), levels = c("c", "b", "a"))
  res <- resolve_value_palette(f, metro_palette)
  expect_identical(names(res$map), c("c", "b", "a"))
})

test_that("resolve_value_palette: function, unnamed vector, and named vector palettes", {
  values <- c("x", "y", "x")

  res_fn <- resolve_value_palette(values, metro_palette)
  expect_identical(res_fn$map, stats::setNames(metro_palette(2), c("x", "y")))

  res_vec <- resolve_value_palette(values, c("red", "blue"))
  expect_identical(res_vec$map, c(x = "red", y = "blue"))

  res_named <- resolve_value_palette(values, c(y = "blue", x = "red"))
  expect_identical(unname(res_named$map[c("x", "y")]), c("red", "blue"))
})

test_that("resolve_value_palette: unmatched values in a named palette get gray, with a warning", {
  expect_warning(
    res <- resolve_value_palette(c("x", "y"), c(x = "red")),
    "no match"
  )
  expect_identical(unname(res$map["y"]), "#94a4ac")
})

test_that("resolve_value_palette: an unnamed vector too short is an error", {
  expect_error(resolve_value_palette(c("x", "y", "z"), c("red", "blue")))
})

test_that("resolve_value_palette: a function returning too few colors is an error", {
  expect_error(resolve_value_palette(c("x", "y", "z"), function(n) {
    c("red", "blue")
  }))
})

test_that("resolve_node_color: NULL gives one palette color per node, by row", {
  nodes <- data.frame(
    name = c("a", "b", "c"),
    row = c(0, 1, 2),
    color_index = c(0, 1, 2),
    stringsAsFactors = FALSE
  )
  res <- resolve_node_color(nodes, NULL, metro_palette)
  expect_identical(res$colors, stats::setNames(metro_palette(3), nodes$name))
  expect_false(res$legend)
})

test_that("resolve_node_color: a column name maps and shows a legend", {
  nodes <- data.frame(
    name = c("a", "b", "c"),
    row = c(0, 1, 2),
    color_index = c(0, 1, 2),
    type = c("x", "y", "x"),
    stringsAsFactors = FALSE
  )
  res <- resolve_node_color(nodes, "type", metro_palette)
  expect_true(res$legend)
  expect_identical(res$title, "type")
  expect_identical(unname(res$colors["a"]), unname(res$colors["c"])) # both type "x"
  expect_false(identical(unname(res$colors["a"]), unname(res$colors["b"])))
})

test_that("resolve_node_color: a column name that is also a valid color name means the column (rule 3)", {
  nodes <- data.frame(
    name = c("a", "b"),
    row = c(0, 1),
    color_index = c(0, 1),
    red = c("low", "high"),
    stringsAsFactors = FALSE
  )
  res <- resolve_node_color(nodes, "red", metro_palette)
  expect_true(res$legend) # treated as the column, not the color "red"
  expect_identical(res$title, "red")
})

test_that("resolve_node_color: a single valid color paints every node, no legend", {
  nodes <- data.frame(
    name = c("a", "b"),
    row = c(0, 1),
    color_index = c(0, 1),
    stringsAsFactors = FALSE
  )
  res <- resolve_node_color(nodes, "steelblue", metro_palette)
  expect_false(res$legend)
  expect_true(all(res$colors == "steelblue"))
})

test_that("resolve_node_color: an invalid node_color (not a column or a color) errors", {
  nodes <- data.frame(
    name = "a",
    row = 0,
    color_index = 0,
    stringsAsFactors = FALSE
  )
  expect_error(resolve_node_color(
    nodes,
    "not_a_column_or_color",
    metro_palette
  ))
})

test_that("resolve_edge_color: source and target copy the resolved node color", {
  edge_paths <- data.frame(
    from = c("a", "b"),
    to = c("b", "c"),
    piece = "vline",
    stringsAsFactors = FALSE
  )
  node_colors <- c(a = "red", b = "green", c = "blue")
  src <- resolve_edge_color(edge_paths, "source", node_colors)
  tgt <- resolve_edge_color(edge_paths, "target", node_colors)
  expect_identical(unname(src[c("a|b", "b|c")]), c("red", "green"))
  expect_identical(unname(tgt[c("a|b", "b|c")]), c("green", "blue"))
})

test_that("resolve_edge_color: a single valid color paints every edge", {
  edge_paths <- data.frame(
    from = "a",
    to = "b",
    piece = "vline",
    stringsAsFactors = FALSE
  )
  res <- resolve_edge_color(edge_paths, "gray50", c(a = "red", b = "green"))
  expect_identical(res, c("a|b" = "gray50"))
})

test_that("resolve_edge_color: an invalid edge_color errors", {
  edge_paths <- data.frame(
    from = "a",
    to = "b",
    piece = "vline",
    stringsAsFactors = FALSE
  )
  expect_error(resolve_edge_color(
    edge_paths,
    "not_a_color",
    c(a = "red", b = "green")
  ))
})

test_that("legend key order follows first appearance (map order), via explicit breaks", {
  # ggplot orders a manual discrete scale's legend alphabetically unless
  # `breaks` is passed explicitly; dag_layers() must pass breaks =
  # names(node_fill$map) so the legend follows first-appearance order.
  d <- as_dag(
    data.frame(from = c("a", "b"), to = c("b", "c"), stringsAsFactors = FALSE),
    nodes = data.frame(
      name = c("a", "b", "c"),
      type = c("z", "y", "x"),
      stringsAsFactors = FALSE
    )
  )
  lay <- dag_layout(d, style_metro())
  layers <- dag_layers(lay, node_color = "type")
  scale <- layers[[which(vapply(
    layers,
    function(l) inherits(l, "ScaleDiscrete") && "fill" %in% l$aesthetics,
    logical(1)
  ))]]
  expect_identical(scale$breaks, c("z", "y", "x")) # first-appearance order, not alphabetical
})

test_that("legend is present for node_color as a column, absent for NULL/constant", {
  d <- as_dag(
    data.frame(from = "a", to = "b", stringsAsFactors = FALSE),
    nodes = data.frame(
      name = c("a", "b"),
      type = c("x", "y"),
      stringsAsFactors = FALSE
    )
  )
  has_fill_legend <- function(lay, ...) {
    layers <- dag_layers(lay, ...)
    any(vapply(
      layers,
      function(l) inherits(l, "ScaleDiscrete") && "fill" %in% l$aesthetics,
      logical(1)
    ))
  }
  lay <- dag_layout(d, style_metro())
  expect_true(has_fill_legend(lay, node_color = "type"))
  expect_false(has_fill_legend(lay))
  expect_false(has_fill_legend(lay, node_color = "steelblue"))
})

test_that("a numeric node_color column maps to distinct palette colors, not a continuous scale", {
  d <- as_dag(
    data.frame(from = "a", to = "b", stringsAsFactors = FALSE),
    nodes = data.frame(
      name = c("a", "b"),
      seconds = c(1.5, 2.7),
      stringsAsFactors = FALSE
    )
  )
  p <- ggplot_dag(d, node_color = "seconds")
  expect_silent(b <- ggplot2::ggplot_build(p))
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  ld <- b$data[[which(geoms == "GeomPoint")]]
  expect_length(unique(ld$fill), 2)
})

test_that("resolve_node_color(NULL): a too-short palette (vector or function) errors rather than recycling", {
  nodes <- data.frame(
    name = letters[1:4],
    row = 0:3,
    color_index = 0:3,
    stringsAsFactors = FALSE
  )
  expect_error(
    resolve_node_color(nodes, NULL, c("red", "blue")),
    "fewer colors"
  )
  res <- resolve_node_color(nodes, NULL, c("red", "blue", "green", "black"))
  expect_identical(unname(res$colors), c("red", "blue", "green", "black"))
  expect_error(
    resolve_node_color(nodes, NULL, function(n) c("red", "blue")),
    "fewer colors"
  )
})

test_that("ggplot_dag(palette = NULL) resolves to metro_palette", {
  d <- as_dag(data.frame(from = "a", to = "b"))
  expect_identical(
    ggplot2::ggplot_build(ggplot_dag(d, palette = NULL))$data,
    ggplot2::ggplot_build(ggplot_dag(d))$data
  )
})

test_that("resolve_node_color()/resolve_edge_color(): non-character argument errors", {
  nodes <- data.frame(
    name = "a",
    row = 0,
    color_index = 0,
    stringsAsFactors = FALSE
  )
  expect_error(resolve_node_color(nodes, 123, metro_palette))
  edge_paths <- data.frame(
    from = "a",
    to = "b",
    piece = "vline",
    stringsAsFactors = FALSE
  )
  expect_error(resolve_edge_color(edge_paths, 123, c(a = "red", b = "green")))
})

test_that("find_cycle(): a cycle found via one successor stops checking a node's other successors", {
  # a -> b -> a (cycle), and a -> c: once the cycle is found while
  # visiting b (a's first successor), visiting c must be skipped.
  edges <- data.frame(
    from = c("a", "b", "a"),
    to = c("b", "a", "c"),
    stringsAsFactors = FALSE
  )
  cyc <- find_cycle(c("a", "b", "c"), edges)
  expect_identical(cyc, c("a", "b", "a"))
})

test_that("a wide DAG with more than 21 lanes gets all-distinct lane colors", {
  d <- as_dag(data.frame(from = "root", to = paste0("t", 1:30)))
  lay <- dag_layout(d)
  res <- resolve_node_color(lay$nodes, NULL, metro_palette)
  lanes <- unique(lay$nodes$color_index)
  expect_gt(length(lanes), 21)
  expect_false(
    anyDuplicated(res$colors[match(lanes, lay$nodes$color_index)]) > 0
  )
})
