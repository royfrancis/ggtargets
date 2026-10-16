# vdiffr snapshots of the rendered plot.

test_that("column coloring with a legend snapshot", {
  skip_if_not_installed("vdiffr")
  d <- as_dag(
    data.frame(
      from = c("a", "a", "b", "c"),
      to = c("b", "c", "d", "d"),
      stringsAsFactors = FALSE
    ),
    nodes = data.frame(
      name = c("a", "b", "c", "d"),
      type = c("x", "y", "x", "y"),
      stringsAsFactors = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    "column-coloring-legend",
    ggplot_dag(d, node_color = "type")
  )
})

test_that("constant edge color snapshot", {
  skip_if_not_installed("vdiffr")
  d <- as_dag(data.frame(
    from = c("a", "a", "b", "c"),
    to = c("b", "c", "d", "d"),
    stringsAsFactors = FALSE
  ))
  vdiffr::expect_doppelganger(
    "constant-edge-color",
    ggplot_dag(d, edge_color = "gray50")
  )
})

test_that("plot with an added title snapshot", {
  skip_if_not_installed("vdiffr")
  d <- as_dag(data.frame(
    from = c("a", "a", "b", "c"),
    to = c("b", "c", "d", "d"),
    stringsAsFactors = FALSE
  ))
  vdiffr::expect_doppelganger(
    "with-title",
    ggplot_dag(d) + ggplot2::labs(title = "my dag")
  )
})

test_that("status coloring snapshot", {
  # a hand-built dag with a status column, not an actual targets pipeline:
  # targets_status_colors() is a hard-coded copy, so this needs no targets
  # project (its drift from the installed targets is checked in
  # test-targets.R).
  skip_if_not_installed("vdiffr")
  d <- as_dag(
    data.frame(
      from = c("a", "a", "b", "c"),
      to = c("b", "c", "d", "d"),
      stringsAsFactors = FALSE
    ),
    nodes = data.frame(
      name = c("a", "b", "c", "d"),
      status = c("uptodate", "outdated", "errored", "queued"),
      stringsAsFactors = FALSE
    )
  )
  vdiffr::expect_doppelganger(
    "status-coloring",
    ggplot_dag(d, node_color = "status", palette = targets_status_colors())
  )
})

# a hand-built dag with every targets vertex type, not a pipeline: no
# targets project is needed
test_that("node shapes by type snapshot", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger("node-shapes", ggplot_dag(typed_dag()))
})

test_that("node shapes with a status color legend snapshot", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "node-shapes-status",
    ggplot_dag(typed_dag(), node_color = "status", palette = targets_status_colors())
  )
})

test_that("node shapes colored by the same column snapshot", {
  skip_if_not_installed("vdiffr")
  vdiffr::expect_doppelganger(
    "node-shapes-merged-legend",
    ggplot_dag(typed_dag(), node_color = "type")
  )
})

test_that("collapsed dag with node size and edge width snapshots", {
  skip_if_not_installed("vdiffr")
  small <- dag_collapse(
    as_dag(
      data.frame(
        from = c("config", "raw1", "raw2", "clean1", "clean2", "fit1", "fit2", "fit1"),
        to = c("fit1", "clean1", "clean2", "feat", "feat", "report", "report", "eval")
      ),
      nodes = data.frame(
        name = c("config", "raw1", "raw2", "clean1", "clean2", "feat", "fit1", "fit2", "eval", "report"),
        stage = c("config", "raw", "raw", "clean", "clean", "feature", "model", "model", "eval", "report")
      )
    ),
    "stage"
  )
  vdiffr::expect_doppelganger(
    "collapsed-sized",
    ggplot_dag(small, node_color = "stage", node_size = "n", edge_width = "n")
  )
  vdiffr::expect_doppelganger(
    "collapsed-sized-target",
    ggplot_dag(
      small,
      node_color = "stage",
      node_size = "n",
      edge_width = "n",
      edge_color = "target"
    )
  )
})
