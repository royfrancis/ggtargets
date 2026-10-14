# ggtargets <img class="ggtargets-logo" src="reference/figures/logo-light.webp" alt="ggtargets-logo" style="width:160px;float:right;" align="right" />

[![ci_badge](https://github.com/royfrancis/ggtargets/workflows/r-cmd-check/badge.svg)](https://github.com/royfrancis/ggtargets/actions?workflow=r-cmd-check) [![codecov](https://codecov.io/gh/royfrancis/ggtargets/branch/main/graph/badge.svg?token=4DOQ8HNQFK)](https://app.codecov.io/gh/royfrancis/targets/) ![status: experimental](https://github.com/GIScience/badges/raw/master/status/experimental.svg)

**ggtargets** creates metro-style diagrams of [targets](https://docs.ropensci.org/targets/) pipelines dependency graph and other directed acyclic graphs using ggplot2.

![](man/figures/README-hero-1.png){width="40%"}

## Installation

Install from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("royfrancis/ggtargets")
```

## Quick start

Inside a `targets` project:

``` r
library(ggtargets)
tar_ggplot()
tar_ggplot(node_color = "status", palette = targets_status_colors())
```

## Example: a `targets` pipeline

``` r
withr::with_dir(withr::local_tempdir(), {
  targets::tar_script(list(
    tar_target(n, 20),
    tar_target(raw_data, 
      data.frame(x = seq_len(n), y = seq_len(n) * 2 + rnorm(n))
    ),
    tar_target(clean, raw_data[complete.cases(raw_data), ]),
    tar_target(model, lm(y ~ x, data = clean)),
    tar_target(summary, coef(model)),
    tar_target(report, capture.output(print(summary)))
  ))
  tar_ggplot()
})
```

![](man/figures/README-targets-example-1.png){width="40%"}

Color by build status to see what's out of date at a glance:

``` r
withr::with_dir(withr::local_tempdir(), {
  targets::tar_script(list(
    tar_target(n, 20),
    tar_target(raw_data, 
      data.frame(x = seq_len(n), y = seq_len(n) * 2 + rnorm(n))
    ),
    tar_target(clean, raw_data[complete.cases(raw_data), ]),
    tar_target(model, lm(y ~ x, data = clean)),
    tar_target(summary, coef(model)),
    tar_target(report, capture.output(print(summary)))
  ))
  targets::tar_make(reporter = "silent")
  tar_ggplot(node_color = "status", palette = targets_status_colors())
})
```

![](man/figures/README-targets-status-1.png){width="40%"}

Nodes are drawn by their type, as `targets::tar_visnetwork()` does: a circle for a regular target, a square for a dynamic-branching one, and an up and a down triangle for imported functions and objects (which are in the graph with `targets_only = FALSE`). A legend names the shapes:

``` r
withr::with_dir(withr::local_tempdir(), {
  targets::tar_script({
    simulate <- function(group) rnorm(10, mean = match(group, letters))
    list(
      tar_target(groups, letters[1:3]),
      tar_target(data, simulate(groups), pattern = map(groups)),
      tar_target(means, vapply(data, mean, numeric(1)))
    )
  })
  tar_ggplot(targets_only = FALSE)
})
```

![](man/figures/README-targets-shapes-1.png){width="40%"}

## Example: any directed acyclic graph

`ggplot_dag()`/`as_dag()` work with any edge-list data frame, not just a `targets` pipeline:

``` r
edges <- data.frame(
  from = c("a", "a", "b", "c"), 
  to = c("b", "c", "d", "d")
)
nodes <- data.frame(
  name = c("a", "b", "c", "d"), 
  type = c("input", "process", "process", "output")
)
ggplot_dag(as_dag(edges, nodes = nodes), node_color = "type")
```

![](man/figures/README-dataframe-example-1.png){width="40%"}

See `vignette("ggtargets")` to get started, and `browseVignettes("ggtargets")` for more (customizing the style, other inputs, how the layout works). A few more articles (colors, working with ggplot2, a gallery) are pkgdown-only for now, since the package doesn't ship them to keep install-time dependencies light - build the site locally with `pkgdown::build_site()` to read them.

## Acknowledgements

The metro style and its default color palette are reproduced from [dagviz](https://pypi.org/project/dagviz/) by Wim Yedema (Apache-2.0), and the workflow-diagram use case is inspired by
[snakevision](https://github.com/OpenOmics/snakevision) by OpenOmics (MIT). The code was mostly rewritten by Anthropic AI models Claude Sonnet 5 and Opus 5.5. ggtargets is an independent package: it is not part of rOpenSci's targets project. 
