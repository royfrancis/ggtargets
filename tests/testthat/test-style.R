# The "toy style" compatibility test from plan.md Section 9.2 (a hand-built
# style working with ggplot_dag()/tar_ggplot() unchanged) needs dag_layers()
# and ggplot_dag(), which are Phase 3. Only style_metro()'s own defaults,
# SVG-to-R mappings and validation are covered here.

test_that("style_metro() defaults equal dagviz's StyleConfig", {
  s <- style_metro()
  expect_equal(s$scale, 10)
  expect_equal(s$node_radius, 6)
  expect_null(s$node_fill)
  expect_equal(s$node_stroke, "white")
  expect_equal(s$node_stroke_width, 2)
  expect_equal(s$edge_stroke_width, 2)
  expect_equal(s$arc_radius, 15)
  expect_equal(s$bridge_color, "white")
})

test_that("style_metro() fits the plot to its labels by default, unlike dagviz's 500 px floor", {
  expect_null(style_metro()$minimal_width)
  expect_equal(style_metro(minimal_width = 500)$minimal_width, 500)
})

test_that("style_metro() maps SVG-specific StyleConfig values to R equivalents by default", {
  s <- style_metro()
  expect_equal(s$label_font_family, "sans") # "sans-serif" -> "sans"
  expect_equal(s$label_font_size, 16) # "inherit" -> 16 (px, in a standalone SVG)
  expect_equal(s$label_font_color, "black") # "currentColor" -> "black"
  expect_equal(s$label_arrow_stroke, "lightgray") # SVG "lightgrey" keyword -> R "lightgray"
  expect_equal(s$label_arrow_dash_array, "22") # dash "2" -> linetype "22"
})

test_that("style_metro() maps dagviz's literal StyleConfig values when passed explicitly", {
  # e.g. the dagviz dark-style example (customizing.qmd) passes
  # dagviz's own StyleConfig values through, not R-native ones.
  s <- style_metro(
    label_font_family = "sans-serif",
    label_font_size = "inherit",
    label_font_color = "currentColor",
    label_arrow_stroke = "lightgrey",
    label_arrow_dash_array = "2"
  )
  expect_equal(s$label_font_family, "sans")
  expect_equal(s$label_font_size, 16)
  expect_equal(s$label_font_color, "black")
  expect_equal(s$label_arrow_stroke, "lightgray")
  expect_equal(s$label_arrow_dash_array, "22")
})

test_that("style_metro() passes already-R-native values through unchanged", {
  s <- style_metro(
    label_font_family = "serif",
    label_font_size = 20,
    label_font_color = "navy",
    label_arrow_stroke = "gray50",
    label_arrow_dash_array = "44"
  )
  expect_equal(s$label_font_family, "serif")
  expect_equal(s$label_font_size, 20)
  expect_equal(s$label_font_color, "navy")
  expect_equal(s$label_arrow_stroke, "gray50")
  expect_equal(s$label_arrow_dash_array, "44")
})

test_that("style_metro() accepts every documented argument", {
  s <- style_metro(
    scale = 12,
    node_radius = 9,
    node_fill = "red",
    node_stroke = "black",
    node_stroke_width = 3,
    edge_stroke_width = 4,
    label_font_family = "serif",
    label_font_size = 14,
    label_font_color = "gray20",
    label_arrow_stroke = "gray80",
    label_arrow_dash_array = "44",
    arc_radius = 12,
    bridge_color = "gray95",
    minimal_width = 200
  )
  expect_equal(s$scale, 12)
  expect_equal(s$node_fill, "red")
  expect_equal(s$minimal_width, 200)
})

test_that("style_metro() errors on an unknown argument", {
  expect_error(style_metro(not_a_real_arg = 1))
})

test_that("style_metro() returns a classed dag_style_metro object", {
  expect_s3_class(style_metro(), "dag_style_metro")
})
