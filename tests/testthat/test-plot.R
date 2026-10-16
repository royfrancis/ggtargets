small_dag <- function() {
  as_dag(data.frame(
    from = c("a", "a", "b", "c"),
    to = c("b", "c", "d", "d"),
    stringsAsFactors = FALSE
  ))
}

test_that("ggplot_dag() returns a ggplot that builds successfully", {
  p <- ggplot_dag(small_dag())
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("ggplot_dag() accepts a dag_layout directly, or a dag (laid out internally)", {
  lay <- dag_layout(small_dag(), style_metro())
  p1 <- ggplot_dag(lay)
  p2 <- ggplot_dag(small_dag())
  expect_s3_class(p1, "ggplot")
  expect_s3_class(p2, "ggplot")
})

test_that("dag_layers() returns layers in dagviz's own draw order", {
  # leader, vline, bridge, hline+arc, nodes, labels, coord - bridges must
  # come after vlines (painting a vline on top of a bridge would erase the
  # white crossing-gap), distinguished from vline by stroke color since
  # both are GeomSegment.
  lay <- dag_layout(small_dag(), style_metro())
  layers <- dag_layers(lay)
  geoms <- vapply(
    layers,
    function(l) if (inherits(l, "Layer")) class(l$geom)[1] else class(l)[1],
    character(1)
  )

  segment_idx <- which(geoms == "GeomSegment")
  is_bridge <- vapply(
    segment_idx,
    function(i) {
      identical(layers[[i]]$aes_params$colour, lay$style$bridge_color)
    },
    logical(1)
  )
  leader_idx <- segment_idx[1]
  vline_idx <- segment_idx[!is_bridge][segment_idx[!is_bridge] != leader_idx]
  bridge_idx <- segment_idx[is_bridge]
  path_idx <- which(geoms == "GeomPath")
  node_idx <- which(geoms == "GeomPoint")
  text_idx <- which(geoms == "GeomText")
  coord_idx <- which(vapply(
    layers,
    function(l) inherits(l, "Coord"),
    logical(1)
  ))

  expect_identical(leader_idx, 1L)
  # one node layer: an empty `node_idx` would make the order checks vacuous
  expect_length(node_idx, 1)
  expect_true(all(vline_idx < bridge_idx))
  expect_true(all(bridge_idx < path_idx))
  expect_true(all(path_idx < node_idx))
  expect_true(all(node_idx < text_idx))
  expect_length(coord_idx, 1)
})

test_that("the combined hlines+arcs path draws in dagviz's order, surviving GeomPath's internal group sort", {
  # GeomPath$draw_panel() does `data <- data[order(data$group), ]` before
  # drawing, and ggplot2 assigns a character group its integer id
  # alphabetically - so a naive character group ("a1", "a10", "h2", ...)
  # would draw alphabetically, not in dagviz's row/column order. `group`
  # must already be encoded as 1, 2, 3, ... in draw order so that sort is
  # a no-op. Needs >= 10 hlines/arcs for the alphabetical-vs-numeric gap
  # ("a10" before "a2") to actually be exercised.
  wide_fanout <- function() {
    as_dag(data.frame(
      from = "root",
      to = paste0("leaf", 1:15),
      stringsAsFactors = FALSE
    ))
  }
  p <- ggplot_dag(wide_fanout())
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  ld <- ggplot2::layer_data(p, which(geoms == "GeomPath"))
  expect_false(is.unsorted(ld$group)) # already in draw order pre-sort, so draw_panel's sort changes nothing
})

test_that("the plot accepts extra layers and themes added with +", {
  p <- ggplot_dag(small_dag()) +
    ggplot2::labs(title = "my dag") +
    ggplot2::theme(plot.title = ggplot2::element_text(size = 20))
  expect_s3_class(p, "ggplot")
  b <- ggplot2::ggplot_build(p)
  expect_identical(b$plot$labels$title, "my dag")
})

test_that("node_color as a column shows a fill legend; NULL and a constant color don't", {
  d <- as_dag(
    data.frame(from = "a", to = "b", stringsAsFactors = FALSE),
    nodes = data.frame(
      name = c("a", "b"),
      type = c("x", "y"),
      stringsAsFactors = FALSE
    )
  )
  p_col <- ggplot_dag(d, node_color = "type")
  p_null <- ggplot_dag(d)
  p_const <- ggplot_dag(d, node_color = "steelblue")

  has_fill_legend <- function(p) {
    b <- ggplot2::ggplot_build(p)
    any(vapply(
      b$plot$scales$scales,
      function(s) "fill" %in% s$aesthetics,
      logical(1)
    ))
  }
  expect_true(has_fill_legend(p_col))
  expect_false(has_fill_legend(p_null))
  expect_false(has_fill_legend(p_const))
})

test_that("edge_color \"source\", \"target\" and a constant all build without error", {
  d <- small_dag()
  expect_silent(ggplot2::ggplot_build(ggplot_dag(d, edge_color = "source")))
  expect_silent(ggplot2::ggplot_build(ggplot_dag(d, edge_color = "target")))
  expect_silent(ggplot2::ggplot_build(ggplot_dag(d, edge_color = "gray50")))
})

test_that("invalid node_color/edge_color error", {
  d <- small_dag()
  expect_error(ggplot_dag(d, node_color = "not_a_column_or_color"))
  expect_error(ggplot_dag(d, edge_color = "not_a_column_or_color"))
})

test_that("ggsave_dag() writes PNG, PDF and SVG, the PNG at the layout's true pixel size", {
  skip_if_not_installed("png")
  skip_if_not_installed("svglite")
  lay <- dag_layout(small_dag(), style_metro())
  p <- ggplot_dag(lay)
  png_file <- withr::local_tempfile(fileext = ".png")
  pdf_file <- withr::local_tempfile(fileext = ".pdf")
  svg_file <- withr::local_tempfile(fileext = ".svg")

  ggsave_dag(png_file, p)
  ggsave_dag(pdf_file, p)
  ggsave_dag(svg_file, p)

  expect_true(file.exists(png_file))
  expect_true(file.exists(pdf_file))
  expect_true(file.exists(svg_file))

  dims <- dim(png::readPNG(png_file))
  vb <- lay$viewbox
  expect_equal(c(dims[1], dims[2]), c(round(vb$height), round(vb$width)))
})

test_that("ggsave_dag()'s scale raises dpi, not physical size, so strokes stay proportioned", {
  skip_if_not_installed("png")
  p <- ggplot_dag(small_dag())
  f1 <- withr::local_tempfile(fileext = ".png")
  f2 <- withr::local_tempfile(fileext = ".png")
  ggsave_dag(f1, p, scale = 1)
  ggsave_dag(f2, p, scale = 2)
  d1 <- dim(png::readPNG(f1))
  d2 <- dim(png::readPNG(f2))
  expect_equal(d2[1] / d1[1], 2, tolerance = 0.02)
  expect_equal(d2[2] / d1[2], 2, tolerance = 0.02)
})

test_that("ggsave_dag() errors on a plot with no coord_fixed() limits", {
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, wt)) + ggplot2::geom_point()
  expect_error(ggsave_dag(withr::local_tempfile(fileext = ".png"), p))
})

test_that("manual assembly (ggplot() + dag_layers() + dag_theme()) works, including ggsave_dag()", {
  lay <- dag_layout(small_dag(), style_metro())
  p <- ggplot2::ggplot() + dag_layers(lay) + dag_theme()
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  f <- withr::local_tempfile(fileext = ".png")
  expect_no_error(ggsave_dag(f, p))
  expect_true(file.exists(f))
})

test_that("style_metro(node_fill=) recolors nodes only, not edges", {
  d <- small_dag()
  lay <- dag_layout(d, style_metro(node_fill = "purple"))
  layers <- dag_layers(lay)
  geoms <- vapply(
    layers,
    function(l) if (inherits(l, "Layer")) class(l$geom)[1] else class(l)[1],
    character(1)
  )
  point_layer <- layers[[which(geoms == "GeomPoint")]]
  expect_true(all(point_layer$data$fill_ == "purple"))
  # the edges: the straight lines, and the bends and horizontal runs
  edge_colors <- function(geom) {
    unlist(lapply(layers[which(geoms == geom)], function(l) l$data$color_))
  }
  for (geom in c("GeomSegment", "GeomPath")) {
    colors <- edge_colors(geom)
    # an empty set would pass the check below without checking anything
    expect_gt(length(colors), 0L)
    expect_false(any(colors == "purple"), info = geom)
  }
})

test_that("ggplot_dag() and ggsave_dag() handle the empty dag without error", {
  d <- as_dag(
    data.frame(from = character(0), to = character(0)),
    nodes = data.frame(name = character(0), stringsAsFactors = FALSE)
  )
  p <- ggplot_dag(d)
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  f <- withr::local_tempfile(fileext = ".png")
  # an empty dag is a 1 x 1 px image, and the Windows png() device (what
  # ggsave() uses without ragg) warns that this is an unlikely size; that isn't
  # what is tested
  expect_no_error(suppressWarnings(ggsave_dag(f, p)))
})

chain_dag <- function(n) {
  as_dag(data.frame(
    from = paste0("n", seq_len(n - 1)),
    to = paste0("n", seq_len(n)[-1])
  ))
}

test_that("ggplot_dag() fixes the panel at zoom px per data unit by default", {
  lay <- dag_layout(chain_dag(5), style_metro())
  vb <- lay$viewbox
  p <- ggplot_dag(lay)
  expect_equal(as.numeric(p$theme$panel.widths), vb$width / 96)
  expect_equal(as.numeric(p$theme$panel.heights), vb$height / 96)
  expect_equal(unname(dag_size(p)), c(vb$width, vb$height) / 96)
  expect_equal(
    unname(dag_size(ggplot_dag(lay, zoom = 0.5))),
    c(vb$width, vb$height) / 96 / 2
  )
  expect_null(ggplot_dag(lay, fixed_size = FALSE)$theme$panel.widths)
  expect_equal(
    unname(dag_size(ggplot_dag(lay, fixed_size = FALSE))),
    c(vb$width, vb$height) / 96
  )
})

test_that("zoom scales line widths and text with the panel", {
  lay <- dag_layout(chain_dag(3), style_metro())
  lw <- function(layers) {
    unlist(lapply(layers, function(l) {
      if (inherits(l, "Layer")) c(l$aes_params$linewidth, l$aes_params$size)
    }))
  }
  expect_equal(lw(dag_layers(lay, zoom = 2)), 2 * lw(dag_layers(lay)))
})

test_that("zoom is validated and ignored (with a warning) without a fixed size", {
  d <- chain_dag(3)
  expect_error(ggplot_dag(d, zoom = 0), "positive")
  expect_error(ggplot_dag(d, zoom = c(1, 2)), "positive")
  expect_warning(p <- ggplot_dag(d, fixed_size = FALSE, zoom = 2), "ignored")
  expect_null(p$theme$panel.widths)
})

test_that("element sizes are constant across graph sizes: only the canvas grows", {
  small <- ggplot_dag(chain_dag(3))
  large <- ggplot_dag(chain_dag(40))
  expect_equal(
    as.numeric(large$theme$panel.heights) /
      as.numeric(small$theme$panel.heights),
    dag_layout(chain_dag(40))$viewbox$height /
      dag_layout(chain_dag(3))$viewbox$height
  )
  expect_equal(
    lapply(dag_layers(dag_layout(chain_dag(3))), function(l) {
      l$aes_params$linewidth
    })[1:2],
    lapply(dag_layers(dag_layout(chain_dag(40))), function(l) {
      l$aes_params$linewidth
    })[1:2]
  )
})

test_that("dag_size() and ggsave_dag() make room for a legend", {
  skip_if_not_installed("png")
  lay <- dag_layout(chain_dag(5), style_metro())
  p <- ggplot_dag(lay, node_color = "name")
  s <- dag_size(p)
  expect_gt(s[["width"]], lay$viewbox$width / 96)
  f <- withr::local_tempfile(fileext = ".png")
  ggsave_dag(f, p)
  dims <- dim(png::readPNG(f))
  # within a pixel: the device drops a fractional pixel. (`expect_equal(
  # tolerance = 1)` would be relative, and allow an image twice the size.)
  expect_lte(max(abs(c(dims[2], dims[1]) - unname(s) * 96)), 1)
})

test_that("dag_size() leaves no device open", {
  n <- length(grDevices::dev.list())
  dag_size(ggplot_dag(chain_dag(3)))
  expect_equal(length(grDevices::dev.list()), n)
})

test_that("a node with an NA color value keeps its edges, whatever the edge_color", {
  edges <- data.frame(from = c("a", "a", "b"), to = c("b", "c", "c"))
  with_values <- function(values) {
    as_dag(edges, nodes = data.frame(name = c("a", "b", "c"), v = values))
  }
  complete <- with_values(c("p", "q", "r"))
  cases <- list(
    source = c(NA, "q", "r"), # a is the source of two edges
    middle = c("p", NA, "r"),
    sink = c("p", "q", NA),
    all = c(NA_character_, NA, NA)
  )
  for (edge_color in c("source", "target", "gray40")) {
    reference <- length(line_colours(ggplot_dag(complete, node_color = "v", edge_color = edge_color)))
    for (case in names(cases)) {
      info <- paste(edge_color, case)
      p <- ggplot_dag(with_values(cases[[case]]), node_color = "v", edge_color = edge_color)
      # the lines are all there, in a color, and drawing drops none of them
      expect_false(anyNA(line_colours(p)), info = info)
      expect_length(line_colours(p), reference)
      expect_no_warning(draw_plot(p))
      # the node is the gray, and the legend has no entry for it
      fills <- node_data(p)$fill
      layout_names <- dag_layout(with_values(cases[[case]]))$nodes$name
      na_nodes <- layout_names[is.na(cases[[case]][match(layout_names, c("a", "b", "c"))])]
      expect_true(all(fills[match(na_nodes, layout_names)] == value_na_color), info = info)
      labels <- unlist(lapply(legends_of(p), `[[`, "labels"))
      expect_false(any(is.na(labels) | labels == "NA"), info = info)
    }
  }
  # no node_color at all: nothing changes
  expect_no_warning(draw_plot(ggplot_dag(with_values(c(NA, "q", "r")))))
})

test_that("a named palette does not warn about NA, and still colors the other values", {
  d <- as_dag(
    data.frame(from = c("a", "b"), to = c("c", "c")),
    nodes = data.frame(name = c("a", "b", "c"), v = c(NA, "x", "y"))
  )
  p <- expect_no_warning(
    ggplot_dag(d, node_color = "v", palette = c(x = "red", y = "blue"))
  )
  expect_setequal(node_data(p)$fill, c(value_na_color, "red", "blue"))
  expect_no_warning(draw_plot(p))
})
