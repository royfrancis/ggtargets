library(targets)
tar_option_set(packages = character())

make_group_data <- function(group, seed) {
  set.seed(seed)
  data.frame(group = group, value = stats::runif(5))
}

clean_group <- function(data, threshold) {
  data[data$value > threshold, ]
}

summarize_group <- function(data) {
  data.frame(group = data$group[1], mean = mean(data$value))
}

compute_stats <- function(data) {
  data.frame(group = data$group[1], sd = stats::sd(data$value))
}

merge_all <- function(summary, stats) {
  merge(summary, stats, by = "group")
}

overall_summary <- function(merged) {
  data.frame(mean_of_means = mean(merged$mean), mean_of_sds = mean(merged$sd))
}

write_report <- function(merged, overall) {
  paste(utils::capture.output(print(merged)), utils::capture.output(print(overall)),
    collapse = "\n"
  )
}

list(
  tar_target(seed, 1),
  tar_target(threshold, 0.05),
  tar_target(groups, letters[1:6]),
  tar_target(group_data, make_group_data(groups, seed), pattern = map(groups)),
  tar_target(cleaned, clean_group(group_data, threshold), pattern = map(group_data)),
  tar_target(group_summary, summarize_group(cleaned), pattern = map(cleaned)),
  tar_target(group_stats, compute_stats(cleaned), pattern = map(cleaned)),
  tar_target(merged, merge_all(group_summary, group_stats)),
  tar_target(overall, overall_summary(merged)),
  tar_target(report, write_report(merged, overall))
)
