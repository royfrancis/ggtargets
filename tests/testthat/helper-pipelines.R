# Copy one of the example pipelines in fixtures/pipelines/ into the
# current directory as _targets.R. The path is made absolute here, while
# the working directory is still tests/testthat: callers run inside
# targets::tar_dir(), which changes it.
pipelines_dir <- normalizePath(test_path("fixtures", "pipelines"))

use_pipeline <- function(name) {
  file.copy(file.path(pipelines_dir, name, "_targets.R"), "_targets.R")
}
