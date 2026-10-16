# node_size and edge_width: sizes from a numeric column (resolve_node_size(),
# resolve_edge_width()), their ranges (style_metro()), and how they are drawn
# (dag_layers()). Their error messages are in test-messages.R.

nodes_with <- function(values) {
  data.frame(name = letters[seq_along(values)], v = values)
}

test_that("with no node_size or edge_width there is nothing to resolve", {
  expect_null(resolve_node_size(nodes_with(1:3), NULL, style_metro()))
  expect_null(resolve_edge_width(NULL, NULL, style_metro()))
})

test_that("node radius grows with the square root of the value, over the default range", {
  style <- style_metro() # radius 6: from 3 to 9
  expect_identical(node_size_range(style), c(3, 9))
  radius <- resolve_node_size(nodes_with(c(0, 25, 100)), "v", style)
  expect_identical(names(radius), c("a", "b", "c"))
  # position 0, 0.25, 1; its square root 0, 0.5, 1
  expect_equal(unname(radius), c(3, 6, 9))
})

test_that("an NA value, or a constant column, is node_radius", {
  style <- style_metro(node_radius = 5)
  radius <- resolve_node_size(nodes_with(c(1, NA, 4)), "v", style)
  expect_equal(unname(radius), c(2.5, 5, 7.5)) # NA is the baseline, not a size
  expect_equal(unname(resolve_node_size(nodes_with(c(7, 7)), "v", style)), c(5, 5))
  expect_equal(unname(resolve_node_size(nodes_with(c(NA_real_, NA)), "v", style)), c(5, 5))
  # infinite values are not sizes either
  expect_equal(unname(resolve_node_size(nodes_with(c(1, Inf, 4)), "v", style))[2], 5)
})

test_that("the baseline is kept inside a range that does not contain node_radius", {
  style <- style_metro(node_radius = 6, node_size_range = c(7, 9))
  expect_equal(unname(resolve_node_size(nodes_with(c(1, NA)), "v", style)), c(7, 7))
})

test_that("edge width is linear in the value, over the default range", {
  style <- style_metro() # edge width 2: from 1 to 5
  expect_identical(edge_width_range(style), c(1, 5))
  edges <- data.frame(from = c("a", "a", "b"), to = c("b", "c", "c"), n = c(1, 3, 5))
  width <- resolve_edge_width(edges, "n", style)
  expect_identical(names(width), c("a|b", "a|c", "b|c"))
  expect_equal(unname(width), c(1, 3, 5))
  edges$n <- c(NA, 2, 4)
  expect_equal(unname(resolve_edge_width(edges, "n", style)), c(2, 1, 5))
})

test_that("the default ranges stay inside the grid, whatever the style", {
  for (scale in c(4, 10, 25)) {
    for (radius in c(2, 6, 20)) {
      style <- style_metro(scale = scale, node_radius = radius, edge_stroke_width = radius)
      expect_lte(node_size_range(style)[[2]], scale)
      expect_lte(edge_width_range(style)[[2]], scale / 2)
    }
  }
})

test_that("style_metro() keeps the ranges it is given", {
  style <- style_metro(node_size_range = c(2, 8), edge_width_range = c(1, 4))
  expect_identical(node_size_range(style), c(2, 8))
  expect_identical(edge_width_range(style), c(1, 4))
  expect_null(style_metro()$node_size_range)
  # as much as the grid allows
  expect_no_error(style_metro(node_size_range = c(1, 10), edge_width_range = c(1, 5)))
})

test_that("dag_layout() carries the dag's edges, with their extra columns", {
  d <- dag_collapse(stage_dag(), "stage")
  lay <- dag_layout(d)
  expect_identical(lay$edges, d$edges)
  expect_true("n" %in% names(lay$edges))
  # kept apart from edge_paths, so a column named x can't overwrite a coordinate
  d$edges$x <- 1
  expect_identical(dag_layout(d)$edge_paths$x, lay$edge_paths$x)
})

test_that("node_size draws each node at its own size, and the default is unchanged", {
  d <- stage_dag()
  p <- ggplot_dag(d, node_size = "seconds")
  data <- node_data(p)
  style <- style_metro()
  radius <- resolve_node_size(dag_layout(d)$nodes, "seconds", style)
  diameter <- 2 * radius - style$node_stroke_width
  # layout order is not the dag's: look the nodes up by name
  lay_nodes <- dag_layout(d)$nodes
  expect_equal(data$size, unname(diameter[lay_nodes$name]) / ggplot2::.pt)
  expect_gt(length(unique(data$size)), 1L)
  # the border does not change with the size
  expect_true(all(data$stroke == data$stroke[[1]]))

  plain <- node_data(ggplot_dag(d))
  expect_true(all(plain$size == plain$size[[1]]))
  expect_equal(plain$size[[1]], (2 * style$node_radius - style$node_stroke_width) / ggplot2::.pt)
})

test_that("zoom scales the sizes of the nodes", {
  d <- stage_dag()
  one <- node_data(ggplot_dag(d, node_size = "seconds"))$size
  two <- node_data(ggplot_dag(d, node_size = "seconds", zoom = 2))$size
  expect_equal(two, 2 * one)
})

test_that("a node_size leaves the legend keys at the usual size, in every legend", {
  d <- stage_dag()
  cases <- list(
    fill = list(node_color = "stage", node_shape = "circle"),
    shape = list(node_shape = "stage", shapes = c("circle", "square", "triangle")),
    two = list(node_color = "stage", node_shape = "type"),
    merged = list(node_color = "type", node_shape = "type")
  )
  for (case in names(cases)) {
    args <- cases[[case]]
    plain <- legends_of(do.call(ggplot_dag, c(list(d), args)))
    sized <- legends_of(do.call(ggplot_dag, c(list(d, node_size = "seconds"), args)))
    expect_gt(length(plain), 0L)
    expect_identical(
      lapply(sized, function(g) g$size),
      lapply(plain, function(g) g$size),
      info = case
    )
    # and the keys are as before in everything else
    expect_identical(lapply(sized, `[[`, "labels"), lapply(plain, `[[`, "labels"), info = case)
    expect_identical(lapply(sized, `[[`, "pch"), lapply(plain, `[[`, "pch"), info = case)
  }
})

# a dag whose edges cross, so there are bridges: b's line passes c's row
cross_dag <- function() {
  as_dag(
    data.frame(from = c("a", "b", "a"), to = c("c", "d", "d")),
    nodes = data.frame(name = c("a", "b", "c", "d"))
  )
}

test_that("edge_width draws every edge at its own width, the widest first, for any edge_color", {
  d <- stage_dag()
  d$edges$w <- c(1, 2, 3, 4, 5, 6)
  style <- style_metro()
  for (edge_color in c("source", "target", "gray40")) {
    p <- ggplot_dag(d, edge_width = "w", edge_color = edge_color)
    expect_silent(ggplot2::ggplot_build(p))
    layers <- edge_layers(p)
    widths <- unlist(lapply(c(layers$segments, layers$paths), `[[`, "linewidth"))
    expect_true(all(widths > 0), info = edge_color)
    # the widest edges are drawn first: in the vlines (the second segment
    # layer, after the leaders) and in the arcs and hlines. The bridges are
    # all white, so their order doesn't matter.
    for (layer in list(layers$segments[[2]], layers$paths[[1]])) {
      expect_gt(length(unique(layer$linewidth)), 1L)
      expect_false(is.unsorted(rev(layer$linewidth)), info = edge_color)
    }
  }
})

test_that("a constant column draws every edge at edge_stroke_width", {
  d <- stage_dag()
  d$edges$w <- 7
  px <- function(w) w / ggplot2::.pt
  for (edge_color in c("source", "target")) {
    layers <- edge_layers(ggplot_dag(d, edge_width = "w", edge_color = edge_color))
    # the first segment layer is the leaders, 1 px
    vline <- layers$segments[[2]]
    expect_true(all(vline$linewidth == px(2)), info = edge_color)
    expect_true(all(layers$paths[[1]]$linewidth == px(2)), info = edge_color)
  }
})

test_that("the width of a path is constant, from its first point to its last", {
  d <- stage_dag()
  d$edges$w <- c(1, 2, 3, 4, 5, 6)
  paths <- edge_layers(ggplot_dag(d, edge_width = "w"))$paths[[1]]
  per_group <- tapply(paths$linewidth, paths$group, function(w) length(unique(w)))
  expect_true(all(per_group == 1L))
})

test_that("a bridge is wide enough for the widest edge drawn over it", {
  d <- cross_dag()
  lay <- dag_layout(d)
  bridge <- lay$segments[lay$segments$layer == "bridge", ]
  expect_gt(nrow(bridge), 0L)
  style <- lay$style
  gap <- function(w) w + 2 * pmin(w, style$node_stroke_width)
  # a|c reaches c along a horizontal line from the right (a's column is to
  # the right of c's), b|d reaches d from the left; a|d is a straight line,
  # with no horizontal part, so there is no bridge under it
  expect_identical(bridge$to, c("c", "d"))

  # every edge as wide as an ordinary one: the ordinary bridge
  same <- stats::setNames(rep(2, 3), c("a|c", "b|d", "a|d"))
  expect_equal(
    metro_bridge_widths(bridge, lay$edge_paths, same, style),
    rep(gap(2), nrow(bridge))
  )

  # different edges: each bridge is the gap of the edge over it, so a wide
  # edge gets a wide bridge, and the edge with no horizontal part none
  widths <- stats::setNames(c(1, 4, 2), c("a|c", "b|d", "a|d"))
  expect_equal(
    metro_bridge_widths(bridge, lay$edge_paths, widths, style),
    gap(c(1, 4))
  )
  # a plot draws them: the same, once the column is mapped onto the range
  d$edges$w <- c(1, 4, 2)
  mapped <- resolve_edge_width(d$edges, "w", style)
  geoms <- function(p) vapply(p$layers, function(l) class(l$geom)[1], character(1))
  p <- ggplot_dag(d, edge_width = "w")
  drawn <- ggplot2::ggplot_build(p)$data[[which(geoms(p) == "GeomSegment")[[3]]]]
  expect_equal(
    drawn$linewidth,
    metro_bridge_widths(bridge, lay$edge_paths, mapped, style) / ggplot2::.pt
  )
})

test_that("a bridge under several edges is as wide as the widest of them", {
  # z has four inputs; three of them reach it along one horizontal line, from
  # the right, under one bridge. y -> z comes straight down, with no horizontal
  # part, so it is under no bridge, however wide it is.
  d <- as_dag(
    data.frame(from = c("a", "b", "x", "y"), to = c("z", "z", "z", "z")),
    nodes = data.frame(name = c("a", "b", "x", "y", "z"))
  )
  lay <- dag_layout(d)
  bridge <- lay$segments[lay$segments$layer == "bridge", ]
  expect_identical(nrow(bridge), 1L)
  widths <- c("a|z" = 1, "b|z" = 2, "x|z" = 3, "y|z" = 4)
  got <- metro_bridge_widths(bridge, lay$edge_paths, widths, lay$style)
  # the widest under it is x|z, 3 px, with a margin of 2 px (the node
  # border) on each side
  expect_equal(got, 3 + 2 * 2)
})

test_that("an edge_width with no edges table at all is the same error, not a crash", {
  # a layout that has no `edges` (not one from dag_layout()) has no columns
  expect_error(resolve_edge_width(NULL, "w", style_metro()), "not an edge column")
})

test_that("a dag with no edges draws with a node_size, and an edge_width has no edge column to name", {
  d <- as_dag(
    data.frame(from = character(), to = character()),
    nodes = data.frame(name = c("a", "b"), w = 1:2)
  )
  expect_silent(ggplot2::ggplot_build(ggplot_dag(d, node_size = "w")))
  expect_error(ggplot_dag(d, edge_width = "w"), "not an edge column")
})

test_that("a collapsed dag is drawn with sizes and widths", {
  small <- dag_collapse(stage_dag(), "stage")
  p <- ggplot_dag(small, node_color = "stage", node_size = "n", edge_width = "n")
  expect_silent(ggplot2::ggplot_build(p))
  # three nodes of different sizes: 2, 3 and 1 members
  sizes <- node_data(p)$size
  expect_identical(length(unique(sizes)), 3L)
  lay_nodes <- dag_layout(small)$nodes
  by_name <- stats::setNames(sizes, lay_nodes$name)
  expect_gt(by_name[["model"]], by_name[["data"]])
  expect_gt(by_name[["data"]], by_name[["report"]])
})

test_that("node_size and edge_width draw with a bigger scale and radius, as the cap allows", {
  style <- style_metro(scale = 14, node_radius = 8, node_size_range = c(4, 14))
  p <- ggplot_dag(
    dag_collapse(stage_dag(), "stage"),
    node_size = "n",
    edge_width = "n",
    style = style
  )
  expect_silent(ggplot2::ggplot_build(p))
  expect_equal(
    max(node_data(p)$size) * ggplot2::.pt,
    2 * 14 - style$node_stroke_width
  )
})
