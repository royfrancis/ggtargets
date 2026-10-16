# The node shapes, as ggplot2 point shapes (`pch`). 21 to 25 are the shapes
# that take both a fill and a border color, like a metro station.
node_shape_pch <- c(
  circle = 21L,
  square = 22L,
  diamond = 23L,
  triangle = 24L,
  triangle_down = 25L
)

# How `targets::tar_visnetwork()`'s legend words each vertex type.
node_shape_labels <- c(
  stem = "Regular target",
  pattern = "Dynamic branches",
  `function` = "Function",
  object = "Object"
)

# The fill of a node shape's legend key, when the fill is not itself a legend:
# `targets::tar_visnetwork()`'s own color for its shape keys.
node_shape_key_fill <- "#899DA4"

#' Resolve **node_shape** into one shape per node
#'
#' Used by [dag_layers()]. `NULL` follows the nodes' `type` column if they
#' have one, drawing any value that `shapes` doesn't name as a circle without
#' comment, so a `type` column that isn't about shapes changes nothing. A node
#' column name maps that column, and warns about unmatched values.
#'
#' @param nodes The layout's `nodes` data frame (`name`, plus any extra node
#'   attributes).
#' @param node_shape `NULL`, a column name, or a single shape name.
#' @param shapes A named vector (value -> shape), or an unnamed vector of
#'   shapes assigned to the distinct values in order.
#' @return A list with `shapes` (one shape name per node, named by node
#'   `name`), `legend` (whether to show a shape legend: at least two distinct
#'   shapes are drawn), `title` (the column name, or `NULL`), and, for a
#'   column, `values` (the column, named by node), `breaks` (the legend
#'   entries), `labels` (their text) and `map` (value -> `pch`); otherwise
#'   these four are `NULL`.
#' @keywords internal
resolve_node_shape <- function(nodes, node_shape, shapes) {
  all_one <- function(shape) {
    list(
      shapes = stats::setNames(rep(shape, nrow(nodes)), nodes$name),
      legend = FALSE,
      title = NULL,
      values = NULL,
      breaks = NULL,
      labels = NULL,
      map = NULL
    )
  }

  auto <- is.null(node_shape)
  if (auto) {
    if (!"type" %in% names(nodes)) {
      return(all_one("circle"))
    }
    column <- "type"
  } else {
    if (!is.character(node_shape) || length(node_shape) != 1) {
      stop(
        "`node_shape` must be NULL, a column name, or a single shape.",
        call. = FALSE
      )
    }
    column <- node_shape
    if (!column %in% names(nodes)) {
      if (!column %in% names(node_shape_pch)) {
        stop(
          sprintf(
            "`node_shape` ('%s') is not a node column name or a valid shape (%s).",
            column,
            paste(names(node_shape_pch), collapse = ", ")
          ),
          call. = FALSE
        )
      }
      return(all_one(column))
    }
  }

  check_shapes(shapes)
  values <- nodes[[column]]
  lv <- if (is.factor(values)) levels(values) else unique(as.character(values))
  lv <- lv[!is.na(lv)]
  named <- !is.null(names(shapes)) && any(names(shapes) != "")
  if (named) {
    map <- stats::setNames(unname(shapes[lv]), lv)
    unmatched <- lv[is.na(map)]
    if (length(unmatched) > 0 && !auto) {
      warning(
        "Values with no match in `shapes` drawn as circles: ",
        paste(unmatched, collapse = ", "),
        call. = FALSE
      )
    }
    map[is.na(map)] <- "circle"
  } else {
    if (length(shapes) < length(lv)) {
      stop(
        "`shapes` has fewer shapes than there are distinct values.",
        call. = FALSE
      )
    }
    map <- stats::setNames(shapes[seq_along(lv)], lv)
  }

  # by position, not by name: a name subscript of "" never matches
  node_shapes <- unname(map[match(as.character(values), names(map))])
  node_shapes[is.na(node_shapes)] <- "circle"
  node_shapes <- stats::setNames(node_shapes, nodes$name)

  # the legend lists the values that occur, in the order of `shapes` (so the
  # targets legend is always stem, pattern, function, object), then the rest
  present <- lv[lv %in% as.character(values)]
  breaks <- c(
    intersect(names(shapes), present),
    setdiff(present, names(shapes))
  )
  labels <- unname(node_shape_labels[breaks])
  labels[is.na(labels)] <- breaks[is.na(labels)]

  list(
    shapes = node_shapes,
    legend = length(unique(node_shapes)) > 1L,
    title = column,
    values = stats::setNames(values, nodes$name),
    breaks = breaks,
    labels = labels,
    map = stats::setNames(unname(node_shape_pch[map]), names(map))
  )
}

#' Check that `shapes` is a vector of valid shape names
#' @param shapes The `shapes` argument of [dag_layers()].
#' @return `shapes`, invisibly.
#' @keywords internal
check_shapes <- function(shapes) {
  if (!is.character(shapes) || length(shapes) == 0L || anyNA(shapes)) {
    stop("`shapes` must be a character vector of shape names.", call. = FALSE)
  }
  bad <- setdiff(unique(shapes), names(node_shape_pch))
  if (length(bad) > 0L) {
    stop(
      sprintf(
        "`shapes` has unknown shape(s): %s. Use %s.",
        paste(bad, collapse = ", "),
        paste(names(node_shape_pch), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(shapes)
}
