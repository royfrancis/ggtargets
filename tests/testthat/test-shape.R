# Node shapes: which shape each node gets (resolve_node_shape()), how the
# node layer and its legend are built (dag_layers()), and that the legend
# fits in the plot (dag_size()).

test_that("targets_node_shapes() maps each targets vertex type to a shape", {
  shapes <- targets_node_shapes()
  expect_identical(
    shapes,
    c(
      stem = "circle",
      pattern = "square",
      `function` = "triangle",
      object = "triangle_down"
    )
  )
  expect_true(all(shapes %in% names(node_shape_pch)))
})

test_that("with no type column, nodes are circles and there is no legend", {
  nodes <- data.frame(name = c("a", "b"), status = c("x", "y"))
  res <- resolve_node_shape(nodes, NULL, targets_node_shapes())
  expect_identical(res$shapes, c(a = "circle", b = "circle"))
  expect_false(res$legend)
  expect_null(res$title)
})

test_that("node_shape = NULL follows the type column, with a legend in the order of `shapes`", {
  nodes <- nodes_of(c("object", "stem", "function", "pattern", "stem"))
  res <- resolve_node_shape(nodes, NULL, targets_node_shapes())
  expect_identical(
    unname(res$shapes),
    c("triangle_down", "circle", "triangle", "square", "circle")
  )
  expect_identical(names(res$shapes), nodes$name)
  expect_true(res$legend)
  expect_identical(res$title, "type")
  # the order of `shapes`, not of the nodes
  expect_identical(res$breaks, c("stem", "pattern", "function", "object"))
  expect_identical(
    res$labels,
    c("Regular target", "Dynamic branches", "Function", "Object")
  )
  expect_identical(
    unname(res$map[res$breaks]),
    unname(node_shape_pch[c("circle", "square", "triangle", "triangle_down")])
  )
})

test_that("the legend lists only the values that occur", {
  res <- resolve_node_shape(
    nodes_of(c("pattern", "stem", "stem")),
    NULL,
    targets_node_shapes()
  )
  expect_identical(res$breaks, c("stem", "pattern"))
  expect_identical(res$labels, c("Regular target", "Dynamic branches"))
})

test_that("a type column that isn't about shapes stays circles, silently", {
  # the shape of `type = c("x", "y")` in as_dag()'s own example
  nodes <- nodes_of(c("x", "y", "x"))
  expect_silent(res <- resolve_node_shape(nodes, NULL, targets_node_shapes()))
  expect_identical(unname(res$shapes), rep("circle", 3))
  expect_false(res$legend)

  # one known value among unknown ones: still one shape drawn, no legend
  res <- resolve_node_shape(
    nodes_of(c("stem", "other")),
    NULL,
    targets_node_shapes()
  )
  expect_false(res$legend)
})

test_that("naming a column warns about values `shapes` doesn't name", {
  nodes <- nodes_of(c("stem", "other", "pattern"))
  expect_warning(
    res <- resolve_node_shape(nodes, "type", targets_node_shapes()),
    "Values with no match in `shapes` drawn as circles: other"
  )
  expect_identical(unname(res$shapes), c("circle", "circle", "square"))
  # a legend entry for each value that occurs, the unmatched one last
  expect_identical(res$breaks, c("stem", "pattern", "other"))
  expect_identical(res$labels, c("Regular target", "Dynamic branches", "other"))

  expect_no_warning(resolve_node_shape(
    nodes_of(c("stem", "pattern")),
    "type",
    targets_node_shapes()
  ))
})

test_that("any column can be mapped, by a named or an unnamed `shapes`", {
  nodes <- data.frame(
    name = c("a", "b", "c", "d"),
    kind = c("p", "q", "p", "r")
  )
  named <- resolve_node_shape(
    nodes,
    "kind",
    c(p = "diamond", q = "square", r = "circle")
  )
  expect_identical(unname(named$shapes), c("diamond", "square", "diamond", "circle"))
  expect_identical(named$title, "kind")
  expect_identical(named$breaks, c("p", "q", "r"))

  # unnamed: in the order of the distinct values
  unnamed <- resolve_node_shape(nodes, "kind", c("triangle", "diamond", "square"))
  expect_identical(
    unname(unnamed$shapes),
    c("triangle", "diamond", "triangle", "square")
  )
  expect_identical(unnamed$breaks, c("p", "q", "r"))

  # ...or in the order of a factor's levels
  nodes$kind <- factor(nodes$kind, levels = c("r", "q", "p"))
  by_level <- resolve_node_shape(nodes, "kind", c("triangle", "diamond", "square"))
  expect_identical(
    unname(by_level$shapes),
    c("square", "diamond", "square", "triangle")
  )

  expect_error(
    resolve_node_shape(nodes, "kind", c("circle", "square")),
    "`shapes` has fewer shapes than there are distinct values"
  )
})

test_that("a single shape name draws every node as that shape, with no legend", {
  res <- resolve_node_shape(
    nodes_of(c("stem", "pattern")),
    "diamond",
    targets_node_shapes()
  )
  expect_identical(unname(res$shapes), c("diamond", "diamond"))
  expect_false(res$legend)
  expect_null(res$breaks)
})

test_that("a column name wins over a shape name", {
  nodes <- data.frame(name = c("a", "b"), square = c("u", "v"))
  res <- resolve_node_shape(nodes, "square", c("circle", "triangle"))
  expect_identical(unname(res$shapes), c("circle", "triangle"))
  expect_identical(res$title, "square")
})

test_that("NA values are circles, with no legend entry", {
  nodes <- nodes_of(c("stem", NA, "pattern"))
  res <- resolve_node_shape(nodes, NULL, targets_node_shapes())
  expect_identical(unname(res$shapes), c("circle", "circle", "square"))
  expect_identical(res$breaks, c("stem", "pattern"))
  expect_true(res$legend)

  # and drawn as one: the layer's NA shape is the scale's `na.value`
  p <- ggplot_dag(as_dag(
    data.frame(from = c("a", "b"), to = c("b", "c")),
    nodes = nodes
  ))
  expect_identical(node_data(p)$shape, c(21L, 21L, 22L))
  expect_identical(legends_of(p)[[1]]$labels, c("Regular target", "Dynamic branches"))
})

test_that("a bad node_shape or shapes is an error", {
  nodes <- nodes_of(c("stem", "pattern"))
  shapes <- targets_node_shapes()
  expect_error(
    resolve_node_shape(nodes, "hexagon", shapes),
    "`node_shape` \\('hexagon'\\) is not a node column name or a valid shape"
  )
  expect_error(
    resolve_node_shape(nodes, 1, shapes),
    "`node_shape` must be NULL, a column name, or a single shape"
  )
  expect_error(
    resolve_node_shape(nodes, c("type", "name"), shapes),
    "`node_shape` must be NULL"
  )
  expect_error(
    resolve_node_shape(nodes, NULL, c(stem = "hexagon")),
    "`shapes` has unknown shape\\(s\\): hexagon"
  )
  expect_error(
    resolve_node_shape(nodes, NULL, 21),
    "`shapes` must be a character vector"
  )
  expect_error(
    resolve_node_shape(nodes, NULL, character(0)),
    "`shapes` must be a character vector"
  )
})

test_that("the node layer is one GeomPoint, with the shape of each node", {
  p <- ggplot_dag(typed_dag())
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_identical(sum(geoms == "GeomPoint"), 1L)
  expect_false("GeomPolygon" %in% geoms)

  # layout row order: f, g, a, b, c, d
  data <- node_data(p)
  expect_identical(
    data$shape,
    unname(node_shape_pch[c("triangle", "triangle_down", "circle", "circle", "square", "square")])
  )
  expect_identical(nrow(data), 6L)
})

test_that("with one shape, the shape is a constant of the layer, not a mapped legend", {
  lay <- dag_layout(typed_dag())

  circles <- node_layer(ggplot_dag(lay, node_shape = "circle"))
  expect_identical(circles$aes_params$shape, 21L)
  expect_null(circles$mapping$shape)

  squares <- ggplot_dag(lay, node_shape = "square")
  expect_identical(node_layer(squares)$aes_params$shape, 22L)
  expect_true(all(node_data(squares)$shape == 22L))

  # a plain dag, no type column
  plain <- ggplot_dag(as_dag(data.frame(from = "a", to = "b")))
  expect_identical(node_layer(plain)$aes_params$shape, 21L)
  expect_length(legends_of(plain), 0)
})

test_that("a dag with no nodes still draws", {
  d <- as_dag(
    data.frame(from = character(0), to = character(0)),
    nodes = data.frame(name = character(0), stringsAsFactors = FALSE)
  )
  expect_silent(ggplot2::ggplot_build(ggplot_dag(d)))
  expect_silent(ggplot2::ggplot_build(ggplot_dag(d, node_shape = "square")))
})

test_that("a node's size and border are those of a circle of node_radius, scaled by zoom", {
  lay <- dag_layout(typed_dag())
  style <- lay$style

  one <- node_layer(ggplot_dag(lay, zoom = 1))$aes_params
  two <- node_layer(ggplot_dag(lay, zoom = 2))$aes_params
  # exactly double, for the point and for its border
  expect_identical(two$size, 2 * one$size)
  expect_identical(two$stroke, 2 * one$stroke)
  expect_identical(one$colour, style$node_stroke)

  # a pch-21 circle is as wide as its font size to the middle of its border:
  # `size * .pt + lwd` px, with `lwd = stroke * .stroke / 2` - the node's
  # diameter, 2 * node_radius, and its border's width
  lwd <- one$stroke * ggplot2::.stroke / 2
  expect_equal(lwd, style$node_stroke_width)
  expect_equal(one$size * ggplot2::.pt + lwd, 2 * style$node_radius)

  big <- dag_layout(typed_dag(), style_metro(node_radius = 9, node_stroke_width = 3))
  p <- node_layer(ggplot_dag(big))$aes_params
  expect_equal(p$size * ggplot2::.pt + p$stroke * ggplot2::.stroke / 2, 18)
  expect_equal(p$stroke * ggplot2::.stroke / 2, 3)
})

test_that("a legend of shapes (case B): lane colors, with gray keys of the node shapes", {
  legends <- legends_of(ggplot_dag(typed_dag()))
  expect_length(legends, 1)
  expect_identical(
    legends[[1]]$labels,
    c("Regular target", "Dynamic branches", "Function", "Object")
  )
  expect_identical(legends[[1]]$pch, c(21L, 22L, 24L, 25L))
  expect_true(all(toupper(substr(legends[[1]]$fill, 1, 7)) == "#899DA4"))
})

test_that("a color legend of another column (case C): two legends, colors first, dot keys", {
  p <- ggplot_dag(typed_dag(), node_color = "status", palette = targets_status_colors())
  legends <- legends_of(p)
  expect_length(legends, 2)

  # the status entries: all circles, in their colors, whatever shape the node
  expect_setequal(legends[[1]]$labels, c("none", "uptodate", "outdated", "errored", "queued"))
  expect_true(all(legends[[1]]$pch == 21L))
  by_label <- stats::setNames(substr(legends[[1]]$fill, 1, 7), legends[[1]]$labels)
  expect_identical(
    unname(toupper(by_label[["errored"]])),
    unname(targets_status_colors()[["errored"]])
  )

  # then the types, in gray
  expect_identical(
    legends[[2]]$labels,
    c("Regular target", "Dynamic branches", "Function", "Object")
  )
  expect_identical(legends[[2]]$pch, c(21L, 22L, 24L, 25L))
  expect_true(all(toupper(substr(legends[[2]]$fill, 1, 7)) == "#899DA4"))
})

test_that("coloring and shaping by the same column (case D) is one merged legend", {
  p <- ggplot_dag(typed_dag(), node_color = "type")
  legends <- legends_of(p)
  expect_length(legends, 1)
  expect_identical(
    legends[[1]]$labels,
    c("Regular target", "Dynamic branches", "Function", "Object")
  )
  # colored shapes: the palette's colors, not gray. They go to the values in
  # the order the nodes have them (function, object, stem, pattern), the
  # legend lists them in the order of `shapes`
  expect_identical(legends[[1]]$pch, c(21L, 22L, 24L, 25L))
  pal <- toupper(metro_palette(4))
  expect_identical(
    toupper(substr(legends[[1]]$fill, 1, 7)),
    c(pal[3], pal[4], pal[1], pal[2])
  )
  # ...and the nodes have those colors and shapes
  expect_setequal(toupper(node_data(p)$fill), toupper(metro_palette(4)))
})

test_that("a color legend with a single shape (case A) has circle keys", {
  d <- typed_dag()
  p <- ggplot_dag(d, node_color = "status", node_shape = "circle")
  legends <- legends_of(p)
  expect_length(legends, 1)
  expect_true(all(legends[[1]]$pch == 21L))
  expect_true(all(substr(legends[[1]]$fill, 1, 1) == "#"))
})

test_that("the shape legend goes with guides(shape = \"none\")", {
  p <- ggplot_dag(typed_dag()) + ggplot2::guides(shape = "none")
  expect_length(legends_of(p), 0)
})

test_that("shapes can map a column to any shapes, with its own legend", {
  d <- as_dag(
    data.frame(from = c("a", "a"), to = c("b", "c")),
    nodes = data.frame(name = c("a", "b", "c"), kind = c("in", "mid", "out"))
  )
  p <- ggplot_dag(d, node_shape = "kind", shapes = c("diamond", "square", "triangle"))
  legends <- legends_of(p)
  expect_identical(legends[[1]]$labels, c("in", "mid", "out"))
  expect_identical(legends[[1]]$pch, c(23L, 22L, 24L))
})

test_that("a legend taller than the graph makes dag_size() taller, so it isn't cut off", {
  skip_if_not_installed("png")
  d <- typed_dag()
  plain <- ggplot_dag(d)
  tall <- ggplot_dag(d, node_color = "status", palette = targets_status_colors())
  panel <- as.numeric(tall$theme$panel.heights)
  # the two legends together are taller than the panel
  expect_gt(dag_size(tall)[["height"]], panel)
  # a plot without a legend taller than its panel is its panel's height
  expect_equal(dag_size(ggplot_dag(d, node_shape = "circle"))[["height"]], panel)
  # the shape legend alone (four entries) fits in this panel
  expect_equal(dag_size(plain)[["height"]], panel)

  # and ggsave_dag() saves at that size
  f <- withr::local_tempfile(fileext = ".png")
  ggsave_dag(f, tall)
  img <- png::readPNG(f)
  expect_equal(dim(img)[1] / 96, dag_size(tall)[["height"]], tolerance = 0.02)
})

test_that("a tall legend isn't cut off either when the plot has a title or a caption", {
  # a title puts the panel off the center of the figure, and the legend, which
  # is centred on the panel, with it
  skip_if_not_installed("png")
  base <- ggplot_dag(
    typed_dag(),
    node_color = "status",
    palette = targets_status_colors()
  )
  plots <- list(
    none = base,
    title = base + ggplot2::labs(title = "my dag"),
    caption = base + ggplot2::labs(caption = "a caption\nof two lines"),
    both = base + ggplot2::labs(title = "my dag", caption = "a caption")
  )
  for (name in names(plots)) {
    f <- withr::local_tempfile(fileext = ".png")
    ggsave_dag(f, plots[[name]], bg = "white")
    img <- png::readPNG(f)
    # nothing is drawn on the first and last rows: nothing runs off the figure
    expect_true(all(img[1, , 1:3] > 0.99), label = paste(name, "top edge"))
    expect_true(
      all(img[dim(img)[1], , 1:3] > 0.99),
      label = paste(name, "bottom edge")
    )
  }
})

test_that("tar_ggplot()'s defaults draw each vertex type with the targets shape", {
  # what tar_dag() returns for a pipeline with functions and objects, without
  # running one
  testthat::local_mocked_bindings(tar_dag = function(...) typed_dag())
  expect_setequal(node_data(tar_ggplot())$shape, c(21L, 22L, 24L, 25L))

  # node_shape and shapes reach the plot
  p2 <- tar_ggplot(node_shape = "circle")
  expect_identical(node_layer(p2)$aes_params$shape, 21L)
  expect_true(23L %in% node_data(tar_ggplot(shapes = c(stem = "diamond")))$shape)
})
