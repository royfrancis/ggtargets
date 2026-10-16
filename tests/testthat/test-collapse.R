# dag_collapse(): merging the nodes that share a value of a column, and
# projecting the edges onto the result. Its error messages are in
# test-messages.R.

test_that("dag_collapse() makes one node per value, in order of first appearance", {
  small <- dag_collapse(stage_dag(), "stage")
  expect_s3_class(small, "dag")
  expect_identical(small$nodes$name, c("data", "model", "report"))
  expect_identical(small$nodes$label, c("data (2)", "model (3)", "report (1)"))
  expect_identical(small$nodes$n, c(2L, 3L, 1L))
  # the by column survives, so node_color = "stage" colors the collapsed graph
  expect_identical(small$nodes$stage, c("data", "model", "report"))
})

test_that("dag_collapse() keeps edges between groups, once each, and counts them", {
  small <- dag_collapse(stage_dag(), "stage")
  # clean -> fit, clean -> fit2 both become data -> model; fit -> eval and
  # fit2 -> eval are inside model, and raw -> clean inside data
  expect_identical(small$edges$from, c("data", "model"))
  expect_identical(small$edges$to, c("model", "report"))
  expect_identical(small$edges$n, c(2L, 1L))
})

test_that("label_n = FALSE labels a merged node with just the value", {
  small <- dag_collapse(stage_dag(), "stage", label_n = FALSE)
  expect_identical(small$nodes$label, c("data", "model", "report"))
})

test_that("a node with an NA value is left as it is, where it was", {
  d <- as_dag(
    data.frame(from = c("a", "b", "c"), to = c("b", "c", "d")),
    nodes = data.frame(
      name = c("a", "b", "c", "d"),
      stage = c("x", "x", NA, "y"),
      label = c("A", "B", "C", "D"),
      stringsAsFactors = FALSE
    )
  )
  small <- dag_collapse(d, "stage")
  expect_identical(small$nodes$name, c("x", "c", "y"))
  # a singleton keeps its own label, with no count
  expect_identical(small$nodes$label, c("x (2)", "C", "y (1)"))
  expect_identical(small$nodes$n, c(2L, 1L, 1L))
  expect_true(is.na(small$nodes$stage[2]))
  expect_identical(small$edges$from, c("x", "c"))
  expect_identical(small$edges$to, c("c", "y"))
})

test_that("seconds, bytes and branches are summed; other columns are kept only if shared", {
  small <- dag_collapse(stage_dag(), "stage")
  # data: 1 + NA, model: 2 + 3 + 4, report: NA alone (NA, not 0)
  expect_identical(small$nodes$seconds, c(1, 9, NA))
  # data: stem, stem; model: stem, stem, pattern; report: stem
  expect_identical(small$nodes$type, c("stem", NA, "stem"))

  d <- as_dag(
    data.frame(from = "a", to = "c"),
    nodes = data.frame(
      name = c("a", "b", "c"),
      g = c("x", "x", "y"),
      bytes = c(10L, 5L, NA),
      branches = c(NA, NA, 3L),
      status = factor(c("up", "up", "out")),
      stringsAsFactors = FALSE
    )
  )
  small <- dag_collapse(d, "g")
  expect_identical(small$nodes$bytes, c(15L, NA))
  expect_identical(small$nodes$branches, c(NA, 3L))
  # a factor stays a factor
  expect_s3_class(small$nodes$status, "factor")
  expect_identical(as.character(small$nodes$status), c("up", "out"))
})

test_that("a node with an empty value is left as it is, like an NA", {
  # "" is how a spreadsheet says "no value": two nodes with one must not be
  # merged into a node named ""
  d <- as_dag(
    data.frame(from = c("a", "b"), to = c("b", "c")),
    nodes = data.frame(
      name = c("a", "b", "c"),
      stage = c("", "", "y"),
      stringsAsFactors = FALSE
    )
  )
  small <- dag_collapse(d, "stage")
  expect_identical(small$nodes$name, c("a", "b", "y"))
  expect_identical(small$nodes$label, c("a", "b", "y (1)"))
  expect_identical(small$edges$from, c("a", "b"))
  expect_identical(small$edges$to, c("b", "y"))
})

test_that("a `by` column that is otherwise summed keeps its value", {
  d <- as_dag(
    data.frame(from = "a", to = "c"),
    nodes = data.frame(name = c("a", "b", "c"), bytes = c(10L, 10L, 5L))
  )
  small <- dag_collapse(d, "bytes")
  expect_identical(small$nodes$name, c("10", "5"))
  expect_identical(small$nodes$bytes, c(10L, 5L))
  expect_identical(small$nodes$n, c(2L, 1L))
})

test_that("an `n` column of the dag is replaced by the member count", {
  d <- stage_dag()
  d$nodes$n <- 100
  d$edges$n <- 100
  small <- dag_collapse(d, "stage")
  expect_identical(small$nodes$n, c(2L, 3L, 1L))
  expect_identical(small$edges$n, c(2L, 1L))
})

test_that("other edge columns are dropped", {
  d <- stage_dag()
  d$edges$weight <- seq_len(nrow(d$edges))
  expect_identical(names(dag_collapse(d, "stage")$edges), c("from", "to", "n"))
})

test_that("a dag with no edges, or none between groups, collapses to its nodes", {
  d <- as_dag(
    data.frame(from = character(), to = character()),
    nodes = data.frame(name = c("a", "b", "c"), g = c("x", "x", "y"))
  )
  small <- dag_collapse(d, "g")
  expect_identical(small$nodes$name, c("x", "y"))
  expect_identical(nrow(small$edges), 0L)

  d <- as_dag(
    data.frame(from = "a", to = "b"),
    nodes = data.frame(name = c("a", "b"), g = c("x", "x"))
  )
  small <- dag_collapse(d, "g")
  expect_identical(small$nodes$name, "x")
  expect_identical(nrow(small$edges), 0L)
})

test_that("a collapsed dag is a dag: it lays out and draws", {
  small <- dag_collapse(stage_dag(), "stage")
  lay <- dag_layout(small)
  expect_setequal(lay$nodes$name, c("data", "model", "report"))
  expect_setequal(lay$labels$label, c("data (2)", "model (3)", "report (1)"))
  p <- ggplot_dag(small, node_color = "stage")
  expect_silent(ggplot2::ggplot_build(p))
})

test_that("collapsing by the columns tar_dag() returns", {
  d <- as_dag(fake_network())
  # `description` is NA for raw and clean, "fit" for model
  small <- dag_collapse(d, "description")
  expect_identical(small$nodes$name, c("raw", "clean", "fit"))
  expect_identical(small$nodes$label, c("raw", "clean", "fit (1)"))
  expect_identical(small$nodes$seconds, c(0.5, NA, 12))
  expect_identical(small$edges$n, c(1L, 1L))

  # stem -> pattern -> stem is a loop once the two stems are one node
  expect_error(dag_collapse(d, "type"), "cycle")
  # every status is different, so nothing is merged
  expect_identical(
    dag_collapse(d, "status")$nodes$name,
    c("uptodate", "outdated", "errored")
  )
})

test_that("on random DAGs the edges are the projected pairs, and a loop is an error", {
  skip_on_cran()
  # Kahn's algorithm: the graph is acyclic if every node can be peeled off
  acyclic <- function(nodes, from, to) {
    remaining <- nodes
    repeat {
      has_incoming <- remaining %in% to[from %in% remaining]
      free <- remaining[!has_incoming]
      if (length(free) == 0L) {
        return(length(remaining) == 0L)
      }
      remaining <- setdiff(remaining, free)
    }
  }

  results <- c(ok = 0L, cycle = 0L)
  for (seed in seq_len(200)) {
    withr::local_seed(seed)
    n <- sample(3:15, 1)
    names <- sprintf("n%02d", seq_len(n))
    group <- sample(c("p", "q", "r", NA), n, replace = TRUE)
    pairs <- t(utils::combn(n, 2))
    pairs <- pairs[stats::runif(nrow(pairs)) < 0.3, , drop = FALSE]
    if (nrow(pairs) == 0L) {
      next
    }
    d <- as_dag(
      data.frame(from = names[pairs[, 1]], to = names[pairs[, 2]]),
      nodes = data.frame(name = names, group = group, stringsAsFactors = FALSE)
    )

    target <- ifelse(is.na(group), names, group)
    t_from <- target[match(d$edges$from, names)]
    t_to <- target[match(d$edges$to, names)]
    across <- t_from != t_to
    expected_nodes <- unique(target)

    if (acyclic(expected_nodes, t_from[across], t_to[across])) {
      small <- dag_collapse(d, "group")
      results[["ok"]] <- results[["ok"]] + 1L
      expect_identical(small$nodes$name, expected_nodes, info = seed)
      expect_identical(sum(small$nodes$n), n, info = seed)
      expect_setequal(
        paste(small$edges$from, small$edges$to),
        unique(paste(t_from[across], t_to[across]))
      )
      expect_identical(sum(small$edges$n), sum(across), info = seed)
    } else {
      results[["cycle"]] <- results[["cycle"]] + 1L
      expect_error(dag_collapse(d, "group"), "creates a cycle", info = seed)
    }
  }
  # both outcomes happen, so neither branch passes vacuously
  expect_gt(results[["ok"]], 20L)
  expect_gt(results[["cycle"]], 20L)
})
