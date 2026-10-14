library(targets)
tar_option_set(packages = character())

make_config <- function() list(scale = 2, offset = 1)

load_raw <- function(seed) {
  set.seed(seed)
  stats::rnorm(50)
}

clean <- function(x) {
  x[abs(x) < 3]
}

feature <- function(x, fn) {
  fn(x)
}

combine2 <- function(a, b) {
  c(a, b)
}

model_fit <- function(x, config) {
  mean(x) * config$scale + config$offset
}

evaluate <- function(model, other = NULL) {
  if (is.null(other)) model else mean(c(model, other))
}

report_all <- function(config, ...) {
  vals <- c(...)
  paste("config:", config$scale, config$offset, "| results:", paste(round(vals, 3), collapse = ", "))
}

list(
  tar_target(config, make_config()),
  tar_target(raw_a, load_raw(1)),
  tar_target(raw_b, load_raw(2)),
  tar_target(raw_c, load_raw(3)),
  tar_target(clean_a, clean(raw_a)),
  tar_target(clean_b, clean(raw_b)),
  tar_target(clean_c, clean(raw_c)),
  tar_target(feature_a1, feature(clean_a, mean)),
  tar_target(feature_a2, feature(clean_a, stats::median)),
  tar_target(feature_b1, feature(clean_b, mean)),
  tar_target(feature_ab, combine2(feature_a1, feature_b1)),
  tar_target(feature_bc, combine2(clean_b, clean_c)),
  tar_target(model_1, model_fit(combine2(feature_a2, feature_ab), config)),
  tar_target(model_2, model_fit(combine2(feature_ab, feature_bc), config)),
  tar_target(model_3, model_fit(combine2(feature_bc, clean_c), config)),
  tar_target(eval_1, evaluate(model_1)),
  tar_target(eval_2, evaluate(model_2, model_1)),
  tar_target(eval_3, evaluate(model_3)),
  tar_target(summary_all, combine2(combine2(eval_1, eval_2), eval_3)),
  tar_target(report, report_all(config, summary_all))
)
