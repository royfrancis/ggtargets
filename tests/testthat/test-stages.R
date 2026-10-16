# A pipeline's stages kept in the `description` of its targets, and the
# pipeline collapsed by them: tar_ggplot(collapse_by = "description"). The
# fixture "stages" sets the description for a block of targets at a time,
# with tar_option_set(); see tar_ggplot()'s "Collapsing a pipeline by stage".
#
# Each tar_network() call spawns a process, so the cases that can share a
# project do. The scripts in the failure-mode tests are written inline.

stage_names <- c("config", "ingest", "model", "report", "note")

test_that("a pipeline's stages, set a block at a time, collapse into one node each, and draw", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    use_pipeline("stages")
    d <- tar_dag()

    # the block form: each target has the description set before it. `note`
    # has none because the option was reset before it, and `config` has none
    # because it was defined before any was set
    description <- stats::setNames(d$nodes$description, d$nodes$name)
    expect_identical(
      unname(description[c("raw_a", "raw_b", "clean_a", "clean_b")]),
      rep("ingest", 4)
    )
    expect_identical(
      unname(description[c("features", "fit_1", "fit_2")]),
      rep("model", 3)
    )
    expect_identical(
      unname(description[c("summary_all", "report_doc")]),
      rep("report", 2)
    )
    expect_true(is.na(description[["config"]]))
    expect_true(is.na(description[["note"]]))

    # one node per stage, and each undescribed target on its own. tar_network()
    # has its own order, so look the nodes up by name.
    s <- dag_collapse(d, "description")
    expect_setequal(s$nodes$name, stage_names)
    n <- stats::setNames(s$nodes$n, s$nodes$name)
    expect_identical(
      n[stage_names],
      c(config = 1L, ingest = 4L, model = 3L, report = 2L, note = 1L)
    )
    labels <- stats::setNames(s$nodes$label, s$nodes$name)
    expect_identical(
      unname(labels[stage_names]),
      c("config", "ingest (4)", "model (3)", "report (2)", "note")
    )
    # the edges between stages, counting those they stand for; the ones inside
    # a stage are gone
    expect_length(s$edges$n, 3L)
    edge_n <- stats::setNames(s$edges$n, paste(s$edges$from, s$edges$to))
    expect_identical(
      edge_n[c("ingest model", "model report", "config model")],
      c(`ingest model` = 2L, `model report` = 3L, `config model` = 1L)
    )

    # drawn: five nodes; sized and widened by the counts; colored by stage,
    # with the edges out of the undescribed nodes drawn
    p <- tar_ggplot(
      collapse_by = "description",
      node_color = "description",
      node_size = "n",
      edge_width = "n"
    )
    expect_gt(length(line_colours(p)), 0L)
    expect_false(anyNA(line_colours(p)))
    expect_no_warning(draw_plot(p))
    lay <- dag_layout(s)
    radius <- stats::setNames(node_data(p)$size, lay$nodes$name)
    expect_length(radius, 5L)
    expect_gt(radius[["ingest"]], radius[["model"]])
    expect_gt(radius[["model"]], radius[["report"]])
    expect_gt(radius[["report"]], radius[["config"]])
    expect_identical(radius[["config"]], radius[["note"]])

    # three edge counts, three widths, from the thinnest to the widest of the
    # range (1 to 5 px)
    width <- resolve_edge_width(lay$edges, "n", lay$style)
    expect_equal(sort(unname(width)), c(1, 3, 5))
    # drawn: the vlines, and the arcs and hlines. (Not the leaders, which are
    # always 1 px, nor the bridges, which are wider than the edges over them.)
    layers <- edge_layers(p)
    drawn <- unlist(lapply(
      list(layers$segments[[2]], layers$paths[[1]]),
      `[[`,
      "linewidth"
    ))
    expect_setequal(round(drawn * ggplot2::.pt, 6), c(1, 3, 5))

    # the full pipeline is still there, uncollapsed
    expect_identical(nrow(tar_dag()$nodes), 11L)
  })
})

test_that("a stage with mixed statuses has no status, and is drawn gray with its edges", {
  skip_if_not_installed("targets")
  skip_on_cran()
  colors <- targets_status_colors()
  by_stage_status <- function() {
    s <- dag_collapse(tar_dag(), "description")
    stats::setNames(s$nodes$status, s$nodes$name)
  }
  status_plot <- function() {
    tar_ggplot(
      collapse_by = "description",
      node_color = "status",
      palette = colors
    )
  }
  # the fill of each node of the plot, by name
  fills <- function(p) {
    s <- dag_collapse(tar_dag(), "description")
    stats::setNames(node_data(p)$fill, dag_layout(s)$nodes$name)
  }

  targets::tar_dir({
    use_pipeline("stages")

    # before a build: every target is outdated, so every stage is
    p <- status_plot()
    expect_no_warning(draw_plot(p))
    expect_gt(length(line_colours(p)), 0L)
    expect_false(anyNA(line_colours(p)))
    expect_true(all(by_stage_status() == "outdated"))

    # after it: every target is up to date
    targets::tar_make(reporter = "silent")
    p <- status_plot()
    expect_no_warning(draw_plot(p))
    expect_true(all(by_stage_status() == "uptodate"))

    # edit raw_b's own loader: raw_b, and everything downstream of it, is
    # outdated again; raw_a and `config` are not, so "ingest" is half and half
    script <- readLines("_targets.R")
    expect_true(any(grepl("set.seed(2)", script, fixed = TRUE)))
    writeLines(sub("set.seed(2)", "set.seed(3)", script, fixed = TRUE), "_targets.R")
    status <- by_stage_status()
    expect_true(is.na(status[["ingest"]]))
    expect_identical(status[["model"]], "outdated")
    expect_identical(status[["report"]], "outdated")
    expect_identical(status[["config"]], "uptodate")
    expect_identical(status[["note"]], "uptodate")

    # drawn without a warning, the edges out of "ingest" among them, with the
    # gray for the stage that has no status and the status colors for the rest
    p <- status_plot()
    expect_no_warning(draw_plot(p))
    expect_gt(length(line_colours(p)), 0L)
    expect_false(anyNA(line_colours(p)))
    fill <- fills(p)
    expect_identical(fill[["ingest"]], value_na_color)
    expect_identical(fill[["model"]], colors[["outdated"]])
    expect_identical(fill[["report"]], colors[["outdated"]])
    expect_identical(fill[["config"]], colors[["uptodate"]])
    # the gray has no legend entry
    labels <- unlist(lapply(legends_of(p), `[[`, "labels"))
    expect_setequal(labels, c("outdated", "uptodate"))
  })
})

test_that("a description that has to match exactly, and a stage named like a target", {
  skip_if_not_installed("targets")
  skip_on_cran()

  # "ingest" and "Ingest " are two stages
  targets::tar_dir({
    targets::tar_script(list(
      targets::tar_target(a, 1, description = "ingest"),
      targets::tar_target(b, a, description = "Ingest "),
      targets::tar_target(c, a, description = "ingest")
    ))
    s <- dag_collapse(tar_dag(), "description")
    expect_setequal(s$nodes$name, c("ingest", "Ingest "))
    n <- stats::setNames(s$nodes$n, s$nodes$name)
    expect_identical(n[["ingest"]], 2L)
    expect_identical(n[["Ingest "]], 1L)
  })

  # a stage named like a target with no description would be merged with it:
  # an error, not a silent merge
  targets::tar_dir({
    targets::tar_script(list(
      targets::tar_target(a, 1, description = "report"),
      targets::tar_target(report, a)
    ))
    expect_error(
      tar_ggplot(collapse_by = "description"),
      "also the name of a node with no `description`: report",
      fixed = TRUE
    )
  })
})

test_that("stages that depend on each other both ways are an error naming the edges", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    # A -> B -> A, through different targets of each
    targets::tar_script(list(
      targets::tar_target(a1, 1, description = "A"),
      targets::tar_target(b1, a1, description = "B"),
      targets::tar_target(a2, b1, description = "A")
    ))
    expect_error(tar_ggplot(collapse_by = "description"), "creates a cycle")
    expect_error(tar_ggplot(collapse_by = "description"), "a1 -> b1", fixed = TRUE)
    expect_error(tar_ggplot(collapse_by = "description"), "b1 -> a2", fixed = TRUE)
    # the pipeline itself is fine, uncollapsed
    expect_no_error(ggplot2::ggplot_build(tar_ggplot()))
  })
})

test_that("a block's description also reaches targets defined in a file loaded later with tar_source()", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    writeLines("extra_targets <- list(tar_target(z, 1))", "extra.R")
    targets::tar_script({
      tar_option_set(description = "block")
      tar_source("extra.R")
      c(list(tar_target(a, 1)), extra_targets)
    })
    d <- tar_dag()
    description <- stats::setNames(d$nodes$description, d$nodes$name)
    expect_identical(description[["a"]], "block")
    expect_identical(description[["z"]], "block")
  })
})

test_that("a target's own description overrides the block's", {
  skip_if_not_installed("targets")
  skip_on_cran()
  targets::tar_dir({
    # tar_network() runs the script in its own process, so this
    # tar_option_set() doesn't change anything here
    targets::tar_script({
      tar_option_set(description = "block")
      list(
        tar_target(p, 1),
        tar_target(q, p, description = "own")
      )
    })
    d <- tar_dag()
    description <- stats::setNames(d$nodes$description, d$nodes$name)
    expect_identical(description[["p"]], "block")
    expect_identical(description[["q"]], "own")
  })
})
