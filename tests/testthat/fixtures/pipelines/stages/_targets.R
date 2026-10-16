library(targets)
tar_option_set(packages = character())

load_raw <- function(seed) {
  set.seed(seed)
  stats::rnorm(20)
}

# its own loader, so a test can edit just this function and outdate raw_b
# (and what depends on it) without touching raw_a
load_b <- function() {
  set.seed(2)
  stats::rnorm(20)
}

clean <- function(x) {
  x[abs(x) < 3]
}

combine2 <- function(a, b) {
  c(a, b)
}

fit <- function(x, shift = 0) {
  mean(x) + shift
}

render <- function(x, y) {
  paste(round(mean(x), 3), round(mean(y), 3))
}

# The stage of a target is its `description`. tar_option_set() sets it for
# every target defined after it, so each block of targets is one stage, and
# the last call puts the default back, so that `note` has none.
#
# `config` comes first, before any description is set, and feeds only fit_1:
# an undescribed target, with one edge into the "model" stage. (The blocks
# are not named like targets: a global with a target's name is ignored, with
# a warning.)
config_targets <- list(tar_target(config, list(shift = 1)))

tar_option_set(description = "ingest")
ingest_targets <- list(
  tar_target(raw_a, load_raw(1)),
  tar_target(raw_b, load_b()),
  tar_target(clean_a, clean(raw_a)),
  tar_target(clean_b, clean(raw_b))
)

tar_option_set(description = "model")
model_targets <- list(
  tar_target(features, combine2(clean_a, clean_b)),
  tar_target(fit_1, fit(features, config$shift)),
  tar_target(fit_2, fit(features))
)

tar_option_set(description = "report")
report_targets <- list(
  tar_target(summary_all, combine2(fit_1, fit_2)),
  tar_target(report_doc, render(summary_all, features))
)

tar_option_set(description = character(0))
other_targets <- list(tar_target(note, "done"))

c(config_targets, ingest_targets, model_targets, report_targets, other_targets)
