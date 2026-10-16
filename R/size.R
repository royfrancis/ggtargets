#' Radius of each node from a numeric column
#'
#' With `node_size = NULL` the nodes are all `style$node_radius` and this
#' returns `NULL`, so the default drawing path is untouched. With a column,
#' the radius follows the square root of the value's position between the
#' column's smallest and largest value, from the low to the high end of
#' [node_size_range()]. That way it is the node's area, about, that grows in
#' step with the value, not its radius: a node with twice the value does not
#' look twice as wide. Nodes with an `NA` value, and every node when the
#' column is constant, get `node_radius` (kept inside the range).
#'
#' @param nodes The layout's `nodes` data frame.
#' @param node_size `NULL`, or the name of a numeric node column.
#' @param style A [style_metro()] object.
#' @return `NULL`, or the radius in pixels, named by node `name`.
#' @keywords internal
resolve_node_size <- function(nodes, node_size, style) {
  if (is.null(node_size)) {
    return(NULL)
  }
  values <- size_column(nodes, node_size, "node_size", "node")
  range <- node_size_range(style)
  baseline <- min(max(style$node_radius, range[[1L]]), range[[2L]])
  stats::setNames(rescale_size(values, range, baseline, sqrt), nodes$name)
}

#' Width of each edge from a numeric column
#'
#' As [resolve_node_size()], but linear in the value, over
#' [edge_width_range()], and for the layout's `edges`: one width per edge,
#' with `edge_stroke_width` for an `NA` value or a constant column.
#'
#' @param edges The layout's `edges` data frame (`from`, `to` and the `dag`'s
#'   extra edge columns), or `NULL`.
#' @param edge_width `NULL`, or the name of a numeric edge column.
#' @param style A [style_metro()] object.
#' @return `NULL`, or the width in pixels, named `"from|to"`.
#' @keywords internal
resolve_edge_width <- function(edges, edge_width, style) {
  if (is.null(edge_width)) {
    return(NULL)
  }
  if (is.null(edges)) {
    edges <- data.frame(from = character(0), to = character(0))
  }
  values <- size_column(edges, edge_width, "edge_width", "edge")
  range <- edge_width_range(style)
  baseline <- min(max(style$edge_stroke_width, range[[1L]]), range[[2L]])
  stats::setNames(
    rescale_size(values, range, baseline),
    paste(edges$from, edges$to, sep = "|")
  )
}

#' The numeric column that `node_size` or `edge_width` names
#' @param data The nodes or edges data frame.
#' @param column The argument's value.
#' @param arg,what The argument's name, and `"node"` or `"edge"`, for the
#'   error.
#' @return The column.
#' @keywords internal
size_column <- function(data, column, arg, what) {
  if (!is.character(column) || length(column) != 1L || is.na(column)) {
    stop(
      sprintf("`%s` must be NULL or the name of a numeric %s column.", arg, what),
      call. = FALSE
    )
  }
  if (!column %in% names(data)) {
    stop(
      sprintf(
        "`%s` ('%s') is not %s column.",
        arg,
        column,
        if (what == "edge") "an edge" else "a node"
      ),
      call. = FALSE
    )
  }
  values <- data[[column]]
  if (!is.numeric(values)) {
    stop(sprintf("`%s` ('%s') must be a numeric column.", arg, column), call. = FALSE)
  }
  values
}

#' Map values onto a range of sizes
#' @param values A numeric vector.
#' @param range The smallest and largest size.
#' @param baseline The size of a value that is `NA` or infinite, or of every
#'   value when they are all the same.
#' @param curve Applied to each value's position (0 to 1) between the smallest
#'   and largest value; `identity` is linear.
#' @return A numeric vector the length of `values`.
#' @keywords internal
rescale_size <- function(values, range, baseline, curve = identity) {
  out <- rep(baseline, length(values))
  finite <- is.finite(values)
  if (any(finite)) {
    low <- min(values[finite])
    high <- max(values[finite])
    if (high > low) {
      position <- (values[finite] - low) / (high - low)
      out[finite] <- range[[1L]] + (range[[2L]] - range[[1L]]) * curve(position)
    }
  }
  out
}

# Rows and lanes are `2 * scale` apart, so a node whose radius exceeds `scale`
# touches its neighbors, and the plot ends `scale` px from the nodes' centers,
# so a bigger one is cut off. A line wider than half of `scale` crowds the lines
# beside it. The defaults reach 1.5 times the node radius (the radius
# itself is in the range) and 2.5 times the edge width, as far as that cap.
#' Range of node radii and edge widths
#' @param style A [style_metro()] object.
#' @return The smallest and largest size, in pixels.
#' @keywords internal
node_size_range <- function(style) {
  if (!is.null(style$node_size_range)) {
    return(style$node_size_range)
  }
  pmin(c(0.5, 1.5) * style$node_radius, style$scale)
}

#' @rdname node_size_range
#' @keywords internal
edge_width_range <- function(style) {
  if (!is.null(style$edge_width_range)) {
    return(style$edge_width_range)
  }
  pmin(c(0.5, 2.5) * style$edge_stroke_width, style$scale / 2)
}

#' Check a size range given to `style_metro()`
#' @param x `NULL` (the default range), or two numbers.
#' @param cap The largest value allowed.
#' @param arg,cap_name The argument's name, and how the cap is written, for
#'   the error.
#' @return `x`, invisibly.
#' @keywords internal
check_size_range <- function(x, cap, arg, cap_name) {
  if (is.null(x)) {
    return(invisible(x))
  }
  if (
    !is.numeric(x) ||
      length(x) != 2L ||
      anyNA(x) ||
      x[[1L]] <= 0 ||
      x[[1L]] > x[[2L]] ||
      x[[2L]] > cap
  ) {
    stop(
      sprintf(
        "`%s` must be two positive numbers, smallest first, the largest at most %s (%s).",
        arg,
        cap_name,
        format(cap)
      ),
      call. = FALSE
    )
  }
  invisible(x)
}
