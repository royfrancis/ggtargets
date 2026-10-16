# The output of dagviz.style.colors.palette(21): 7 hue steps (hue / 6) at 3
# HLS lightness levels (0.55, 0.7, 0.4), saturation 0.5. Its 7th hue step
# wraps around to the 1st, so each block of 7 holds a duplicate; those
# (positions 7, 14 and 21) are dropped here, leaving 18 distinct colors.
metro_base_colors <- c(
  "#c65353",
  "#c6c653",
  "#53c653",
  "#53c6c6",
  "#5353c6",
  "#c653c6",
  "#d98c8c",
  "#d9d98c",
  "#8cd98c",
  "#8cd9d9",
  "#8c8cd9",
  "#d98cd9",
  "#993333",
  "#999933",
  "#339933",
  "#339999",
  "#333399",
  "#993399"
)

# Minimum CIE Lab distance between any two generated palette colors.
metro_min_distance <- 5

lab_of <- function(hex) {
  grDevices::convertColor(
    t(grDevices::col2rgb(hex)) / 255,
    from = "sRGB",
    to = "Lab"
  )
}

#' The default dagviz metro color palette
#'
#' `n` distinct colors, never recycled. The first 18 are
#' `dagviz.style.colors.palette()`'s colors (6 hues at 3 lightness levels,
#' in dagviz's order, minus the 3 duplicates that its 7th hue step produces
#' by wrapping around to the 1st). Further colors are generated
#' deterministically in HCL space, each kept only if it is perceptibly
#' different from all earlier ones. `metro_palette(k)` is always the first
#' `k` colors of `metro_palette(n)` for `k <= n`. Colors get harder to tell
#' apart as `n` grows large (beyond roughly 100).
#'
#' @param n Number of colors to generate.
#' @return A character vector of `n` distinct hex colors.
#' @export
#' @examples
#' metro_palette(5)
#'
#' # as a row of color swatches
#' swatches <- data.frame(x = 1:24, color = metro_palette(24))
#' ggplot2::ggplot(swatches, ggplot2::aes(x, 1, fill = I(color))) +
#'   ggplot2::geom_tile() +
#'   ggplot2::theme_void()
metro_palette <- function(n) {
  # validate before coercing: as.integer() would silently truncate 1.9 to a
  # one-color palette (and quietly accept "5" / TRUE)
  if (
    !is.numeric(n) || length(n) != 1 || !is.finite(n) || n < 0 || n != trunc(n)
  ) {
    stop("`n` must be a single non-negative whole number.", call. = FALSE)
  }
  n <- as.integer(n)
  base <- metro_base_colors
  if (n <= length(base)) {
    return(base[seq_len(n)])
  }

  # Colors beyond the base 18: HCL candidates whose hue advances by the
  # golden angle (so neighbors in the sequence are far apart) while lightness
  # and chroma cycle. A candidate is kept only if it is at least
  # `metro_min_distance` (Lab) from every color kept so far, so the result is
  # deterministic, prefix-stable and never repeats.
  kept <- base
  labs <- lab_of(base)
  lightness <- c(50, 65, 78, 42, 58, 35, 70)
  chroma <- c(55, 40, 65)
  i <- 0L
  max_tries <- 200L * (n - length(base)) + 1000L
  while (length(kept) < n) {
    if (i >= max_tries) {
      stop("Could not generate that many distinct colors.", call. = FALSE)
    }
    hex <- tolower(grDevices::hcl(
      h = (i * 137.508) %% 360,
      c = chroma[i %/% 7L %% 3L + 1L],
      l = lightness[i %% 7L + 1L]
    ))
    i <- i + 1L
    lab <- lab_of(hex)
    d <- sqrt(rowSums((labs - rep(lab, each = nrow(labs)))^2))
    if (all(d >= metro_min_distance)) {
      kept <- c(kept, hex)
      labs <- rbind(labs, lab)
    }
  }
  kept
}

# Whether `x` is a color R understands. `NA` is not one here: col2rgb() takes
# it for transparent white, but as `node_color` or `edge_color` it would give
# every node or edge a missing color, which ggplot2 drops without a word.
is_valid_color <- function(x) {
  if (anyNA(x)) {
    return(FALSE)
  }
  tryCatch(
    {
      grDevices::col2rgb(x)
      TRUE
    },
    error = function(e) FALSE
  )
}

# The color of a value that has none: a missing value, or one that a named
# palette doesn't name.
value_na_color <- "#94a4ac"

#' Map a column's distinct values to a palette
#'
#' Used by [resolve_node_color()] for a node column: distinct
#' values (factor levels, or else order of first appearance) are mapped to
#' `palette`. A named palette is matched by value, with unmatched values
#' set to gray and a warning; an unnamed vector that is too short is an
#' error; a function is called with the number of distinct values.
#'
#' A missing value (`NA`) is not a value: it takes no color from `palette`,
#' is not in `map` (so it has no legend entry, as for shapes), and is the
#' same gray as an unmatched value, but without the warning. Its color is
#' always defined, because the edges drawn in a node's color (`"source"`,
#' `"target"`) would otherwise be `NA`, and ggplot2 drops a line with a missing
#' color without a word.
#'
#' @param values The raw column values (one per row, possibly repeated).
#' @param palette A `function(n)`, an unnamed color vector, or a named
#'   color vector matched by value.
#' @return A list with `colors` (one per element of `values`, never `NA`) and
#'   `map` (a named vector, value -> color, with no entry for `NA`).
#' @keywords internal
resolve_value_palette <- function(values, palette) {
  lv <- if (is.factor(values)) levels(values) else unique(as.character(values))
  lv <- lv[!is.na(lv)]
  n <- length(lv)
  if (is.function(palette)) {
    colors <- palette(n)
    if (length(colors) < n) {
      stop(
        "`palette` returned fewer colors than there are distinct values.",
        call. = FALSE
      )
    }
    map <- stats::setNames(colors[seq_len(n)], lv)
  } else if (!is.null(names(palette)) && any(names(palette) != "")) {
    map <- stats::setNames(unname(palette[lv]), lv)
    unmatched <- lv[is.na(map)]
    if (length(unmatched) > 0) {
      map[is.na(map)] <- value_na_color
      warning(
        "Values with no match in `palette` set to gray: ",
        paste(unmatched, collapse = ", "),
        call. = FALSE
      )
    }
  } else {
    if (length(palette) < n) {
      stop(
        "`palette` has fewer colors than there are distinct values.",
        call. = FALSE
      )
    }
    map <- stats::setNames(palette[seq_len(n)], lv)
  }
  # by position, not by name: a name subscript of "" never matches
  colors <- unname(map[match(as.character(values), names(map))])
  colors[is.na(values)] <- value_na_color
  list(colors = colors, map = map)
}

#' Resolve **node_color** into one color per node
#'
#' @param nodes The layout's `nodes` data frame (`name`, `row`,
#'   `color_index`, plus any extra node attributes).
#' @param node_color `NULL`, a column name, or a single color.
#' @param palette A `function(n)`, an unnamed color vector, or (when
#'   `node_color` names a column) a named color vector. With
#'   `node_color = NULL`, each node takes the color of its `color_index`, `n`
#'   is the number of those indices (one per node in the metro layout), and
#'   too few colors is an error.
#' @return A list with `colors` (named by node `name`), `legend` (whether
#'   to show a fill legend), `title` (the legend title, or `NULL`), and,
#'   for a column, `values` (the column, named by node) and `map` (value
#'   -> color); otherwise these two are `NULL`.
#' @keywords internal
resolve_node_color <- function(nodes, node_color, palette) {
  if (is.null(node_color)) {
    # one color per color_index (the metro layout gives each node its own),
    # not per value of a column
    n_colors <- max(-1L, nodes$color_index) + 1L
    pal <- if (is.function(palette)) palette(n_colors) else palette
    if (length(pal) < n_colors) {
      stop("`palette` has fewer colors than there are nodes.", call. = FALSE)
    }
    colors <- pal[nodes$color_index + 1]
    return(list(
      colors = stats::setNames(colors, nodes$name),
      legend = FALSE,
      title = NULL,
      values = NULL,
      map = NULL
    ))
  }
  if (!is.character(node_color) || length(node_color) != 1) {
    stop(
      "`node_color` must be NULL, a column name, or a single color.",
      call. = FALSE
    )
  }
  if (node_color %in% names(nodes)) {
    res <- resolve_value_palette(nodes[[node_color]], palette)
    # a column of nothing but NA has no values to show in a legend
    has_values <- length(res$map) > 0L
    return(list(
      colors = stats::setNames(res$colors, nodes$name),
      legend = has_values,
      title = if (has_values) node_color,
      values = stats::setNames(nodes[[node_color]], nodes$name),
      map = res$map
    ))
  }
  if (!is_valid_color(node_color)) {
    stop(
      sprintf(
        "`node_color` ('%s') is not a node column name or a valid color.",
        node_color
      ),
      call. = FALSE
    )
  }
  list(
    colors = stats::setNames(rep(node_color, nrow(nodes)), nodes$name),
    legend = FALSE,
    title = NULL,
    values = NULL,
    map = NULL
  )
}

#' Resolve **edge_color** into one color per edge
#'
#' @param edge_paths The layout's `edge_paths` data frame (one or more rows
#'   per edge, with `from`/`to` columns).
#' @param edge_color `"source"`, `"target"`, or a single color.
#' @param node_colors The `colors` element returned by [resolve_node_color()]
#'   (a vector named by node name), used for `"source"`/`"target"`.
#' @return A color vector named `"from|to"`, one per distinct edge.
#' @keywords internal
resolve_edge_color <- function(edge_paths, edge_color, node_colors) {
  pairs <- unique(edge_paths[c("from", "to")])
  key <- paste(pairs$from, pairs$to, sep = "|")
  if (!is.character(edge_color) || length(edge_color) != 1) {
    stop(
      "`edge_color` must be \"source\", \"target\", or a single color.",
      call. = FALSE
    )
  }
  if (identical(edge_color, "source")) {
    return(stats::setNames(unname(node_colors[pairs$from]), key))
  }
  if (identical(edge_color, "target")) {
    return(stats::setNames(unname(node_colors[pairs$to]), key))
  }
  if (!is_valid_color(edge_color)) {
    stop(
      sprintf(
        "`edge_color` ('%s') is not \"source\", \"target\", or a valid color.",
        edge_color
      ),
      call. = FALSE
    )
  }
  stats::setNames(rep(edge_color, nrow(pairs)), key)
}
