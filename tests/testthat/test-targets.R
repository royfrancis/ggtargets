# Each tar_network()/tar_make() call spawns a callr subprocess, so
# assertions are batched per temporary project rather than one project per
# test_that(), and the slower cases are skipped on CRAN.
#
# The tests at the end of the file start no subprocess: they swap
# tar_network() (or tar_dag()) for a stub that records its arguments. They
# are fast and have no skip_on_cran(), so the code they cover is counted
# when coverage is measured without NOT_CRAN=true too.

test_that("tar_dag() keeps type/status/description/seconds/bytes/branches", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    targets::tar_script(list(
      targets::tar_target(x, 1),
      targets::tar_target(y, x + 1)
    ))
    d <- tar_dag()
    expect_s3_class(d, "dag")
    expect_setequal(d$nodes$name, c("x", "y"))
    for (col in c("type", "status", "description", "seconds", "bytes", "branches")) {
      expect_true(col %in% names(d$nodes), info = col)
    }
  })
})

test_that("targets_status_colors() matches the installed targets version", {
  skip_if_not_installed("targets")
  # reads targets' internals, which a targets release may reorganize
  skip_on_cran()
  fn <-targets:::visual_class$public_methods$produce_fills # nolint: undesirable_function_linter.
  theirs <- eval(body(fn)[[2]][[3]])
  ours <- targets_status_colors()
  expect_identical(ours[order(names(ours))], theirs[order(names(theirs))])
})

test_that("targets_node_shapes() matches the installed targets version", {
  skip_if_not_installed("targets")
  # reads targets' internals, which a targets release may reorganize
  skip_on_cran()
  fn <-targets:::visnetwork_class$public_methods$produce_shapes # nolint: undesirable_function_linter.
  theirs <- eval(body(fn)[[2]][[3]])
  # visNetwork's own names for the shapes
  visnetwork <- c(
    circle = "dot",
    square = "square",
    triangle = "triangle",
    triangle_down = "triangleDown"
  )
  ours <- targets_node_shapes()
  ours <- stats::setNames(unname(visnetwork[ours]), names(ours))
  expect_identical(ours[order(names(ours))], theirs[order(names(theirs))])
})

test_that("tar_ggplot() draws stems as circles and dynamic branches as squares, with a legend", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    use_pipeline("branching")
    p <- tar_ggplot()
    expect_setequal(node_data(p)$shape, c(21L, 22L))
    legends <- legends_of(p)
    expect_length(legends, 1)
    expect_identical(legends[[1]]$labels, c("Regular target", "Dynamic branches"))
  })
  # a pipeline of stems only has one shape, so no legend
  targets::tar_dir({
    use_pipeline("basic")
    p <- tar_ggplot()
    expect_setequal(node_data(p)$shape, 21L)
    expect_length(legends_of(p), 0)
  })
})

test_that("tar_ggplot(targets_only = FALSE) draws functions and objects as triangles", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    targets::tar_script({
      helper <- function(x) x + 1
      offset <- 2
      list(
        targets::tar_target(x, helper(offset)),
        targets::tar_target(y, x + 1, pattern = map(x))
      )
    })
    d <- tar_dag(targets_only = FALSE)
    expect_setequal(d$nodes$type, c("stem", "pattern", "function", "object"))

    p <- tar_ggplot(targets_only = FALSE)
    shape <- stats::setNames(node_data(p)$shape, dag_layout(d)$nodes$name)
    expect_identical(
      shape[c("x", "y", "helper", "offset")],
      c(x = 21L, y = 22L, helper = 24L, offset = 25L)
    )
    expect_identical(
      legends_of(p)[[1]]$labels,
      c("Regular target", "Dynamic branches", "Function", "Object")
    )
  })
})

test_that("tar_ggplot() plots before and after tar_make(), status changes, for every bundled pipeline", {
  skip_if_not_installed("targets")
  skip_on_cran()
  pipelines <- list.files(pipelines_dir)
  expect_gte(length(pipelines), 4L)
  for (pipeline in pipelines) {
    targets::tar_dir({
      use_pipeline(pipeline)

      p_before <- tar_ggplot()
      expect_silent(ggplot2::ggplot_build(p_before))
      p_before_status <- tar_ggplot(
        node_color = "status",
        palette = targets_status_colors()
      )
      expect_silent(ggplot2::ggplot_build(p_before_status))

      status_before <- tar_dag()$nodes$status
      targets::tar_make(reporter = "silent")
      status_after <- tar_dag()$nodes$status
      expect_false(identical(status_before, status_after), info = pipeline)

      p_after <- tar_ggplot()
      expect_silent(ggplot2::ggplot_build(p_after))
      p_after_status <- tar_ggplot(
        node_color = "status",
        palette = targets_status_colors()
      )
      expect_silent(ggplot2::ggplot_build(p_after_status))
    })
  }
})

test_that("node_color = \"type\" on the branching pipeline shows pattern and stem", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    use_pipeline("branching")
    d <- tar_dag()
    expect_setequal(d$nodes$type, c("stem", "pattern"))
    p <- tar_ggplot(node_color = "type")
    expect_silent(ggplot2::ggplot_build(p))
  })
})

test_that("node_color = \"branches\" (NA for stems) maps to a real color, no warning", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    use_pipeline("branching")
    d <- tar_dag()
    expect_true(anyNA(d$nodes$branches)) # stems have NA branches
    expect_no_warning(p <- tar_ggplot(node_color = "branches"))
    b <- ggplot2::ggplot_build(p)
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    fills <- b$data[[which(geoms == "GeomPoint")]]$fill
    expect_false(anyNA(fills)) # every node, including NA-branches ones, gets a real color
    # ...and so does every edge: one out of a stem (NA branches) used to have
    # no color, and ggplot2 dropped it, with a warning, when it drew the plot
    expect_gt(length(line_colours(p)), 0L)
    expect_false(anyNA(line_colours(p)))
    expect_no_warning(draw_plot(p))
  })
})

test_that("targets_only = FALSE adds the functions a target uses", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    targets::tar_script({
      helper <- function(x) x + 1
      list(targets::tar_target(x, helper(1)))
    })
    d_targets_only <- tar_dag(targets_only = TRUE)
    d_all <- tar_dag(targets_only = FALSE)
    expect_true(nrow(d_all$nodes) > nrow(d_targets_only$nodes))
    expect_true("helper" %in% d_all$nodes$name)
  })
})

test_that("names/allow/exclude pass through as tidyselect (tar_network()'s own semantics)", {
  skip_if_not_installed("targets")
  skip_if_not_installed("tidyselect")
  skip_on_cran()
  targets::tar_dir({
    targets::tar_script(list(
      targets::tar_target(x, 1),
      targets::tar_target(y, x + 1),
      targets::tar_target(z, y + 1)
    ))
    d_all <- tar_dag()
    expect_setequal(d_all$nodes$name, c("x", "y", "z"))

    # `names` keeps what it selects and everything upstream of it
    d_names <- tar_dag(names = tidyselect::starts_with("y"))
    expect_setequal(d_names$nodes$name, c("x", "y"))

    d_allow <- tar_dag(allow = tidyselect::starts_with("x"))
    expect_setequal(d_allow$nodes$name, "x")

    d_exclude <- tar_dag(exclude = tidyselect::starts_with("z"))
    expect_setequal(d_exclude$nodes$name, c("x", "y"))
  })
})

test_that("tar_ggplot()'s exclude= filters through two ... layers", {
  # `exclude` is not an argument of tar_ggplot(): it goes through `...` to
  # tar_dag(), and through its `...` to tar_network(). A user writes the bare
  # starts_with("tmp_"), with no tidyselect:: prefix, which only resolves if
  # targets itself re-exports it (it does: "starts_with" %in%
  # getNamespaceExports("targets")), since ggtargets doesn't depend on
  # tidyselect. Use targets::starts_with() here for the same bare resolution
  # without an unconditional library(targets) in a test file.
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    targets::tar_script(list(
      targets::tar_target(x, 1),
      targets::tar_target(tmp_y, x + 1)
    ))
    p <- tar_ggplot(
      targets_only = FALSE,
      exclude = targets::starts_with("tmp_")
    )
    b <- ggplot2::ggplot_build(p)
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    labels <- b$data[[which(geoms == "GeomText")]]$label
    expect_false(any(grepl("^tmp_", labels)))
    expect_true(any(labels == "x"))
  })
})

test_that("script and store pass through, and reflect a real build", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    targets::tar_script(
      list(targets::tar_target(x, 1)),
      script = "my_targets.R",
      ask = FALSE
    )
    targets::tar_make(
      script = "my_targets.R",
      store = "my_store",
      reporter = "silent"
    )

    d_built <- tar_dag(script = "my_targets.R", store = "my_store")
    expect_setequal(d_built$nodes$name, "x")
    expect_identical(d_built$nodes$status, "uptodate")

    d_default_store <- tar_dag(script = "my_targets.R") # a fresh, never-built default store
    expect_identical(d_default_store$nodes$status, "outdated")
  })
})

test_that("withr::with_dir() plots a pipeline in another folder", {
  skip_if_not_installed("targets")
  skip_if_not_installed("withr")
  skip_on_cran()
  project_dir <- withr::local_tempdir()
  withr::with_dir(project_dir, {
    targets::tar_script(list(targets::tar_target(x, 1)))
  })
  d <- withr::with_dir(project_dir, tar_dag())
  expect_setequal(d$nodes$name, "x")
  p <- withr::with_dir(project_dir, tar_ggplot())
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("envir defaults to the caller's environment, seeing caller-defined functions", {
  # targets_only = FALSE so a global function shows up as its own node;
  # callr_function = NULL so tar_network() analyzes code in *this* process
  # instead of a fresh callr subprocess (which wouldn't see my_fn at all,
  # regardless of envir). No explicit envir= is passed here - that's the
  # point: tar_dag()'s own default (envir = parent.frame(), evaluated in
  # tar_dag()'s frame) must resolve to this test's environment, not a
  # ggtargets function frame. (Verified by temporarily deleting `envir =
  # envir` from tar_dag()'s tar_network() call and confirming this test
  # fails with "object 'my_fn' not found".)
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    my_fn <- function(x) x * 2
    targets::tar_script({
      list(targets::tar_target(x, my_fn(1)))
    })
    d <- tar_dag(targets_only = FALSE, callr_function = NULL)
    expect_true("my_fn" %in% d$nodes$name)

    p <- tar_ggplot(targets_only = FALSE, callr_function = NULL)
    b <- ggplot2::ggplot_build(p)
    geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
    labels <- b$data[[which(geoms == "GeomText")]]$label
    expect_true("my_fn" %in% labels)
  })
})

test_that("tar_dag()/tar_ggplot() error clearly outside a targets project", {
  skip_if_not_installed("targets")
  skip_on_cran()
  empty_dir <- withr::local_tempdir()
  # the message is targets' own, so it is matched here, not snapshotted (the
  # one for a missing targets package, which is ours, is in test-messages.R)
  withr::with_dir(empty_dir, {
    expect_error(tar_dag(), "_targets.R", fixed = TRUE)
    expect_error(tar_ggplot(), "_targets.R", fixed = TRUE)
  })
})

test_that("targets_status_colors() is a named vector of hex colors, one per status", {
  cols <- targets_status_colors()
  expect_type(cols, "character")
  expect_setequal(
    names(cols),
    c(
      "completed",
      "uptodate",
      "outdated",
      "dispatched",
      "canceled",
      "errored",
      "queued",
      "skipped",
      "none"
    )
  )
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$", cols)))
  expect_false(anyDuplicated(cols) > 0)
})

test_that("targets_status_colors() colors nodes by status, with no targets project", {
  p <- ggplot_dag(
    fake_dag(),
    node_color = "status",
    palette = targets_status_colors()
  )
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  fills <- ggplot2::ggplot_build(p)$data[[which(geoms == "GeomPoint")]]$fill
  expected <- targets_status_colors()[c("uptodate", "outdated", "errored")]
  expect_setequal(toupper(fills), toupper(expected))
})

test_that("tar_dag() runs tar_network() with targets_only = TRUE and the caller's envir", {
  skip_if_not_installed("targets")
  seen <- NULL
  testthat::local_mocked_bindings(
    tar_network = function(...) {
      seen <<- list(...)
      fake_network()
    },
    .package = "targets"
  )
  env <- environment()
  d <- tar_dag()
  expect_true(seen$targets_only)
  expect_identical(seen$envir, env)
  # the result is as_dag() of what tar_network() returned
  expect_identical(d, as_dag(fake_network()))
  expect_identical(d$nodes$name, c("raw", "clean", "model"))
  expect_false("path" %in% names(d$nodes))
})

test_that("tar_dag() forwards targets_only, envir and ... to tar_network() untouched", {
  skip_if_not_installed("targets")
  seen <- NULL
  testthat::local_mocked_bindings(
    tar_network = function(...) {
      seen <<- list(...)
      fake_network()
    },
    .package = "targets"
  )
  other <- new.env()
  tar_dag(
    targets_only = FALSE,
    envir = other,
    names = "x",
    exclude = "y",
    script = "s.R",
    store = "st"
  )
  expect_false(seen$targets_only)
  expect_identical(seen$envir, other)
  expect_identical(seen$names, "x")
  expect_identical(seen$exclude, "y")
  expect_identical(seen$script, "s.R")
  expect_identical(seen$store, "st")
})

test_that("tar_ggplot() hands targets_only, envir and ... to tar_dag(), and nothing else", {
  seen <- NULL
  testthat::local_mocked_bindings(
    tar_dag = function(targets_only = TRUE, envir = parent.frame(), ...) {
      seen <<- list(targets_only = targets_only, envir = envir, dots = list(...))
      fake_dag()
    }
  )
  env <- environment()
  p <- tar_ggplot()
  expect_s3_class(p, "ggplot")
  expect_silent(ggplot2::ggplot_build(p))
  expect_true(seen$targets_only)
  expect_identical(seen$envir, env)
  expect_length(seen$dots, 0)

  other <- new.env()
  tar_ggplot(
    targets_only = FALSE,
    envir = other,
    exclude = "x",
    script = "s.R"
  )
  expect_false(seen$targets_only)
  expect_identical(seen$envir, other)
  expect_identical(seen$dots, list(exclude = "x", script = "s.R"))
})

test_that("tar_ggplot() sends style, node_color, edge_color, palette and zoom on to the plot", {
  testthat::local_mocked_bindings(tar_dag = function(...) fake_dag())
  style <- style_metro(minimal_width = 600)
  pal <- c(x = "#112233", y = "#445566")
  p <- tar_ggplot(
    style = style,
    node_color = "type",
    edge_color = "target",
    palette = pal,
    zoom = 2
  )

  # the same plot as laying out and drawing the dag by hand
  ref <- ggplot_dag(
    fake_dag(),
    node_color = "type",
    edge_color = "target",
    palette = pal,
    style = style,
    zoom = 2
  )
  built <- ggplot2::ggplot_build(p)
  expect_identical(built$data, ggplot2::ggplot_build(ref)$data)
  expect_equal(as.numeric(p$theme$panel.widths), as.numeric(ref$theme$panel.widths))
  expect_equal(as.numeric(p$theme$panel.heights), as.numeric(ref$theme$panel.heights))

  # ...and each argument made a difference, so the comparison isn't vacuous:
  # the palette colors the nodes,
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_setequal(built$data[[which(geoms == "GeomPoint")]]$fill, unname(pal))
  # edge_color = "target" differs from the default "source",
  by_source <- ggplot_dag(
    fake_dag(),
    node_color = "type",
    palette = pal,
    style = style,
    zoom = 2
  )
  expect_false(identical(built$data, ggplot2::ggplot_build(by_source)$data))
  # and style and zoom set the panel size: the minimal_width view box at 2x, at 96 dpi
  width <- dag_layout(fake_dag(), style = style)$viewbox$width
  expect_equal(as.numeric(p$theme$panel.widths), width * 2 / 96)
})

test_that("tar_ggplot() sends fixed_size and zoom on to the plot, and keeps ggplot_dag()'s checks", {
  testthat::local_mocked_bindings(tar_dag = function(...) fake_dag())
  p1 <- tar_ggplot()
  p2 <- tar_ggplot(zoom = 2)
  expect_equal(
    as.numeric(p2$theme$panel.widths),
    2 * as.numeric(p1$theme$panel.widths)
  )
  expect_equal(
    as.numeric(p2$theme$panel.heights),
    2 * as.numeric(p1$theme$panel.heights)
  )

  p_free <- tar_ggplot(fixed_size = FALSE)
  expect_null(p_free$theme$panel.widths)
  expect_null(p_free$theme$panel.heights)

  expect_warning(tar_ggplot(fixed_size = FALSE, zoom = 2), "zoom")
  expect_error(tar_ggplot(zoom = -1), "zoom")
})

test_that("tar_ggplot(collapse_by =) draws the dag collapsed by that column", {
  testthat::local_mocked_bindings(tar_dag = function(...) stage_dag())
  p <- tar_ggplot(collapse_by = "stage", node_color = "stage")
  ref <- ggplot_dag(dag_collapse(stage_dag(), "stage"), node_color = "stage")
  expect_identical(
    ggplot2::ggplot_build(p)$data,
    ggplot2::ggplot_build(ref)$data
  )

  # ...and it made a difference: three nodes, not six
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  expect_identical(nrow(ggplot2::ggplot_build(p)$data[[which(geoms == "GeomPoint")]]), 3L)
  full <- tar_ggplot()
  geoms <- vapply(full$layers, function(l) class(l$geom)[1], character(1))
  expect_identical(nrow(ggplot2::ggplot_build(full)$data[[which(geoms == "GeomPoint")]]), 6L)

  expect_error(tar_ggplot(collapse_by = "nope"), "not a node column")
})

test_that("tar_ggplot(collapse_by = \"description\") collapses a real pipeline", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    targets::tar_script(list(
      targets::tar_target(x, 1, description = "input"),
      targets::tar_target(y, x + 1, description = "input"),
      targets::tar_target(z, y * 2, description = "output"),
      targets::tar_target(w, z)
    ))
    d <- dag_collapse(tar_dag(), "description")
    # `w` has no description, so it is left alone (tar_network() lists the
    # targets in its own order, so look them up by name)
    expect_setequal(d$nodes$name, c("input", "output", "w"))
    n <- stats::setNames(d$nodes$n, d$nodes$name)
    expect_identical(n[c("input", "output", "w")], c(input = 2L, output = 1L, w = 1L))
    expect_setequal(
      paste(d$edges$from, d$edges$to),
      c("input output", "output w")
    )
    p <- tar_ggplot(collapse_by = "description", node_color = "description")
    expect_silent(ggplot2::ggplot_build(p))
  })
})

test_that("tar_ggplot() sends node_size and edge_width on to the plot", {
  testthat::local_mocked_bindings(tar_dag = function(...) stage_dag())
  p <- tar_ggplot(collapse_by = "stage", node_size = "n", edge_width = "n")
  ref <- ggplot_dag(
    dag_collapse(stage_dag(), "stage"),
    node_size = "n",
    edge_width = "n"
  )
  built <- ggplot2::ggplot_build(p)
  expect_identical(built$data, ggplot2::ggplot_build(ref)$data)
  # ...and they made a difference
  plain <- tar_ggplot(collapse_by = "stage")
  expect_false(identical(built$data, ggplot2::ggplot_build(plain)$data))
  expect_error(tar_ggplot(node_size = "nope"), "not a node column")
  expect_error(tar_ggplot(edge_width = "nope"), "not an edge column")
})
