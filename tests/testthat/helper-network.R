# Hand-built inputs for tests that need tar_dag()'s data without running a
# pipeline (no callr subprocess, so they are fast and never need
# skip_on_cran()).

# A tar_network()-shaped list: `list(vertices =, edges =)`. The vertices have
# tar_network()'s own columns and order, including the NA `branches` of
# stems, plus `path`, which as_dag.list() must drop.
fake_network <- function() {
  list(
    vertices = data.frame(
      name = c("raw", "clean", "model"),
      type = c("stem", "pattern", "stem"),
      description = c(NA, NA, "fit"),
      status = c("uptodate", "outdated", "errored"),
      seconds = c(0.5, NA, 12),
      bytes = c(120, NA, 4096),
      branches = c(NA, 3L, NA),
      path = c("a", "b", "c"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("raw", "clean"),
      to = c("clean", "model"),
      stringsAsFactors = FALSE
    )
  )
}

# A pipeline in three stages, for collapsing: raw -> clean -> fit -> eval ->
# report, plus a `fit2` that also feeds `eval`. `seconds` has an NA, and
# `type` differs inside the "model" stage.
stage_dag <- function() {
  as_dag(
    data.frame(
      from = c("raw", "clean", "clean", "fit", "fit2", "eval"),
      to = c("clean", "fit", "fit2", "eval", "eval", "report")
    ),
    nodes = data.frame(
      name = c("raw", "clean", "fit", "fit2", "eval", "report"),
      stage = c("data", "data", "model", "model", "model", "report"),
      type = c("stem", "stem", "stem", "stem", "pattern", "stem"),
      seconds = c(1, NA, 2, 3, 4, NA),
      stringsAsFactors = FALSE
    )
  )
}

# What tar_dag() returns, for stubbing it out: a small dag with a
# `type` column to color by (two values) and a `status` column (three).
fake_dag <- function() {
  as_dag(
    data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")),
    nodes = data.frame(
      name = c("a", "b", "c"),
      type = c("x", "y", "x"),
      status = c("uptodate", "outdated", "errored"),
      stringsAsFactors = FALSE
    )
  )
}
