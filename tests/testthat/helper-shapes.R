# Helpers for tests of the drawn plot: node shapes and their legends, the
# node and line layers (test-shape.R, test-plot.R, test-size.R, test-stages.R,
# test-targets.R).

# A hand-built dag with every targets vertex type: tar_network()'s own
# `type` values, in an order that is not the legend's (stem, pattern,
# function, object).
typed_dag <- function() {
  as_dag(
    data.frame(
      from = c("f", "g", "a", "a", "b", "b", "c"),
      to = c("a", "a", "b", "c", "c", "d", "d")
    ),
    nodes = data.frame(
      name = c("f", "g", "a", "b", "c", "d"),
      type = c("function", "object", "stem", "stem", "pattern", "pattern"),
      status = c("none", "none", "uptodate", "outdated", "errored", "queued"),
      stringsAsFactors = FALSE
    )
  )
}

nodes_of <- function(type, name = letters[seq_along(type)]) {
  data.frame(name = name, type = type, stringsAsFactors = FALSE)
}

# The legends of a plot, as drawn: for each, its labels and the `pch` and
# fill of its keys, read from the plot's own grob.
legends_of <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  gt <- ggplot2::ggplotGrob(p)
  box <- gt$grobs[[which(gt$layout$name == "guide-box-right")]]
  if (!inherits(box, "gtable")) {
    return(list())
  }
  guides <- box$grobs[box$layout$name == "guides"]
  lapply(guides, function(g) {
    key <- g$grobs[grepl("^key", g$layout$name)]
    points <- lapply(key, function(k) {
      Filter(function(child) inherits(child, "points"), k$children)[[1]]
    })
    labels <- g$grobs[grepl("^label", g$layout$name)]
    list(
      labels = vapply(labels, function(l) l$children[[1]]$label, character(1)),
      pch = vapply(points, function(pt) as.integer(pt$pch), integer(1)),
      fill = vapply(points, function(pt) pt$gp$fill, character(1)),
      size = vapply(points, function(pt) as.numeric(pt$gp$fontsize), numeric(1))
    )
  })
}

# the plot's node layer, and the data it draws (one row per node)
node_layer <- function(p) {
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  p$layers[[which(geoms == "GeomPoint")]]
}

node_data <- function(p) {
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  ggplot2::ggplot_build(p)$data[[which(geoms == "GeomPoint")]]
}

# Draw a plot on a null device. ggplot2 drops a line with a missing color only
# when it draws (a warning, "Removed rows containing missing values"), which
# ggplot_build() never does, so a check on the built data alone can miss it.
draw_plot <- function(p) {
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off())
  ggplot2::ggplotGrob(p)
}

# The plot's line layers, as built: `segments` (in draw order: the leaders, the
# vlines, the bridges) and `paths` (the arcs and hlines).
edge_layers <- function(p) {
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  data <- ggplot2::ggplot_build(p)$data
  list(
    segments = data[geoms == "GeomSegment"],
    paths = data[geoms == "GeomPath"]
  )
}

# The colors of the plot's lines (edges, bridges and leaders), as built.
line_colours <- function(p) {
  geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
  built <- ggplot2::ggplot_build(p)$data[geoms %in% c("GeomPath", "GeomSegment")]
  unlist(lapply(built, function(layer) layer$colour))
}
