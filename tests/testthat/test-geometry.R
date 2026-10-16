# Structural checks for dag_layout()'s tables, on hand-built graphs.
# Exact-match checks against dagviz's own reference SVGs (view box, node
# circles, labels, segments, draw order, arc geometry and sweep direction)
# were verified during development against 67 fixtures and have since been
# removed along with that fixture data; see NEWS.md for what was verified.

test_that("user-supplied node labels (distinct from name) reach nodes and labels tables", {
  d <- as_dag(
    data.frame(from = "a", to = "b", stringsAsFactors = FALSE),
    nodes = data.frame(
      name = c("a", "b"),
      label = c("Alpha", "Beta"),
      stringsAsFactors = FALSE
    )
  )
  lay <- dag_layout(d, style_metro())
  expect_setequal(lay$nodes$label, c("Alpha", "Beta"))
  expect_setequal(lay$labels$label, c("Alpha", "Beta"))
  # name (identity, used for from/to and joins) must stay untouched
  expect_setequal(lay$nodes$name, c("a", "b"))
})

test_that("dag_layout() handles the empty dag (0 nodes) without error", {
  d <- as_dag(
    data.frame(from = character(0), to = character(0)),
    nodes = data.frame(name = character(0), stringsAsFactors = FALSE)
  )
  lay <- dag_layout(d, style_metro())
  expect_equal(nrow(lay$nodes), 0)
  expect_equal(nrow(lay$segments), 0)
  expect_equal(nrow(lay$arcs), 0)
  expect_equal(nrow(lay$labels), 0)
  expect_equal(nrow(lay$edge_paths), 0)
  expect_true(is.finite(lay$viewbox$width) && is.finite(lay$viewbox$height))
})

# ---- view box width ----------------------------------------------------------

box_dag <- function(label = c("a", "b", "c")) {
  as_dag(
    data.frame(
      from = c("a", "a", "b"),
      to = c("b", "c", "c"),
      stringsAsFactors = FALSE
    ),
    nodes = data.frame(
      name = c("a", "b", "c"),
      label = label,
      stringsAsFactors = FALSE
    )
  )
}

test_that("the view box ends just past the longest label by default", {
  lay <- dag_layout(box_dag(), style_metro())
  vb <- lay$viewbox
  expect_lt(vb$right, 500)
  label_w <- label_width_px(
    lay$labels$label,
    lay$style$label_font_family,
    lay$style$label_font_size
  )
  expect_gte(vb$right, max(lay$labels$x + label_w) + lay$style$scale)
  expect_equal(vb$width, vb$right - vb$left)
  # a whole number of pixels, so a saved PNG is exactly as wide as the box
  expect_equal(vb$width, round(vb$width))
})

test_that("a longer label widens the view box by about its extra text", {
  short <- dag_layout(box_dag(), style_metro())$viewbox
  long <- dag_layout(
    box_dag(c("a", "b", strrep("w", 30))),
    style_metro()
  )$viewbox
  expect_gt(long$width, short$width + 100)
  expect_equal(long$left, short$left)
})

test_that("a numeric minimal_width gives dagviz's rule: max(minimal_width, 2 * right-most x)", {
  lay <- dag_layout(box_dag(), style_metro(minimal_width = 500))
  expect_equal(lay$viewbox$right, 500)
  box_x <- c(lay$nodes$x, lay$segments$x, lay$segments$xend, lay$labels$x)
  narrow <- dag_layout(box_dag(), style_metro(minimal_width = 0))
  expect_equal(narrow$viewbox$right, max(box_x) * 2)
})

test_that("label measuring copes with fonts it can't use, and leaves no device open", {
  # (relative: a session may already have a device open, as RStudio does)
  n_devices <- length(grDevices::dev.list())
  # an unknown family only draws grid warnings, which are not ours to report
  expect_no_warning(w <- label_width_px(c("ab", ""), "no such font", 16))
  expect_gt(w[1], 0)
  expect_equal(w[2], 0)
  # an unusable `family` errors inside grid: fall back to 0.6 em per character
  expect_equal(label_width_px(c("ab", ""), character(0), 16), c(19.2, 0))
  expect_equal(length(grDevices::dev.list()), n_devices)
})

test_that("dag_size() is proportional to zoom, whatever the view box", {
  lay <- dag_layout(box_dag(), style_metro())
  expect_equal(
    dag_size(ggplot_dag(lay, zoom = 2)) / 2,
    dag_size(ggplot_dag(lay))
  )
})
