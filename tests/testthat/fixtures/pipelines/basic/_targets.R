library(targets)
tar_option_set(packages = character())

read_data <- function(n) {
  data.frame(x = seq_len(n), y = seq_len(n) * 2 + stats::rnorm(n))
}

clean_data <- function(data) {
  data[stats::complete.cases(data), ]
}

fit_model <- function(data) {
  stats::lm(y ~ x, data = data)
}

summarize_model <- function(model) {
  data.frame(term = names(stats::coef(model)), estimate = unname(stats::coef(model)))
}

write_report <- function(summary) {
  paste(utils::capture.output(print(summary)), collapse = "\n")
}

list(
  tar_target(n, 20),
  tar_target(raw_data, read_data(n)),
  tar_target(clean, clean_data(raw_data)),
  tar_target(model, fit_model(clean)),
  tar_target(summary, summarize_model(model)),
  tar_target(report, write_report(summary))
)
