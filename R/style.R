#' Plot a DAG or a DAG layout
#'
#' `ggplot_dag()` is `ggplot() + dag_layers(lay, ...) + dag_theme()`.
#' Given a `dag` instead of a `dag_layout`, it first lays it out with
#' [dag_layout()], in the `style` you give it ([style_metro()] by default).
#'
#' By default (`fixed_size = TRUE`) the panel gets an absolute size of
#' `zoom` px (at 96 dpi) per data unit, via `theme(panel.widths,
#' panel.heights)`. Nodes, lines and text then keep the same size whatever
#' the pipeline's length: a bigger graph gives a bigger plot, not smaller
#' nodes. Use [dag_size()] (or [ggsave_dag()]) to get or save the plot at
#' that size. On a device smaller than the plot, such as a small RStudio
#' Plots pane, a large graph is clipped: lower `zoom`, or save it with
#' [ggsave_dag()].
#'
#' With `fixed_size = FALSE` the panel stretches to fill the device, as
#' any ggplot does. Nodes, line widths and text are set in absolute units,
#' so they keep their size while the spacing between nodes stretches, and
#' their proportions to it vary with the graph's size.
#'
#' Adding a *complete* theme afterwards (e.g. `+ ggplot2::theme_void()`)
#' replaces the panel size and so turns fixed sizing off. Add individual
#' `theme()` settings instead.
#'
#' @param x A `dag_layout` (from [dag_layout()]) or a `dag` (from
#'   [as_dag()] or [tar_dag()]).
#' @param node_color,palette,edge_color,node_shape,shapes,node_size,edge_width
#'   Passed to [dag_layers()].
#' @param style A [style_metro()] object, used to lay out `x` when it is a
#'   `dag` (ignored for a `dag_layout`, which carries its own).
#' @param fixed_size If `TRUE` (the default), fix the panel's absolute
#'   size so element sizes are constant across graphs; if `FALSE`, stretch
#'   it to the device.
#' @param zoom Uniform size multiplier for nodes, lines and text (with
#'   `fixed_size = TRUE` only; ignored with a warning otherwise).
#' @return A `ggplot` object.
#' @export
#' @examples
#' dag <- as_dag(data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")))
#' ggplot_dag(dag)
#' ggplot_dag(dag, edge_color = "target")
#' ggplot_dag(dag, style = style_metro(node_radius = 9, scale = 12))
#' ggplot_dag(dag, zoom = 0.5)
#'
#' # nodes shaped (and colored) by a column
#' nodes <- data.frame(name = c("a", "b", "c"), role = c("in", "mid", "out"))
#' ggplot_dag(
#'   as_dag(data.frame(from = c("a", "b"), to = c("b", "c")), nodes = nodes),
#'   node_color = "role",
#'   node_shape = "role",
#'   shapes = c("diamond", "circle", "triangle")
#' )
#' dag_size(ggplot_dag(dag))
#'
#' # a collapsed graph, with node size and edge width showing the counts
#' nodes <- data.frame(
#'   name = c("raw", "clean", "tidy", "fit", "eval", "report"),
#'   stage = c("data", "data", "data", "model", "model", "report")
#' )
#' edges <- data.frame(
#'   from = c("raw", "clean", "tidy", "tidy", "fit", "eval"),
#'   to = c("clean", "tidy", "fit", "eval", "eval", "report")
#' )
#' small <- dag_collapse(as_dag(edges, nodes = nodes), "stage")
#' ggplot_dag(small, node_color = "stage", node_size = "n", edge_width = "n")
ggplot_dag <- function(
  x,
  node_color = NULL,
  edge_color = "source",
  palette = metro_palette,
  node_shape = NULL,
  shapes = targets_node_shapes(),
  node_size = NULL,
  edge_width = NULL,
  style = style_metro(),
  fixed_size = TRUE,
  zoom = 1
) {
  check_zoom(zoom)
  if (!fixed_size && zoom != 1) {
    warning("`zoom` is ignored when `fixed_size = FALSE`.", call. = FALSE)
    zoom <- 1
  }
  layout <- if (inherits(x, "dag_layout")) x else dag_layout(x, style = style)
  p <- ggplot2::ggplot() +
    dag_layers(
      layout,
      node_color = node_color,
      edge_color = edge_color,
      palette = palette,
      node_shape = node_shape,
      shapes = shapes,
      node_size = node_size,
      edge_width = edge_width,
      zoom = zoom
    ) +
    dag_theme()
  # after dag_theme(): a complete theme there would discard the panel size
  if (fixed_size) {
    # absolute panel size: `zoom` px (96 dpi) per data unit of the view box
    vb <- layout$viewbox
    p <- p +
      ggplot2::theme(
        panel.widths = grid::unit(max(vb$width, 1) * zoom / 96, "in"),
        panel.heights = grid::unit(max(abs(vb$height), 1) * zoom / 96, "in")
      )
  }
  p
}

# `zoom` is checked on two entry points: ggplot_dag() (before its own
# `zoom != 1` test) and dag_layers(). One definition, so they can't drift.
check_zoom <- function(zoom) {
  if (!is.numeric(zoom) || length(zoom) != 1 || is.na(zoom) || zoom <= 0) {
    stop("`zoom` must be a single positive number.", call. = FALSE)
  }
  invisible(zoom)
}

# dag_size() measures text on the open device, or on a pdf one when none is
# open, but the plot is then drawn on another, and devices measure text
# slightly differently (a Windows png device measured a legend 1.5% taller than
# the pdf one). A legend that spills over the figure's ends has no room to
# spare, so it is given this fraction more, or it is cut off.
dag_size_margin <- 0.04

#' The size of a DAG plot
#'
#' For a plot drawn with a fixed panel size (the [ggplot_dag()] default),
#' returns the full plot's size, including any legend or title added with
#' [ggplot2::labs()]; a legend taller than the graph makes the plot taller,
#' so it isn't cut off (with a few percent to spare, as devices measure text
#' slightly differently). Otherwise, returns the view box size at 1 px per data
#' unit (96 dpi), read from the `coord_fixed()` limits that [dag_layers()]
#' sets. Useful for sizing figures, e.g. knitr's `fig.width`/`fig.height`.
#'
#' @param plot A plot from [ggplot_dag()] or [tar_ggplot()].
#' @return A named numeric vector, `c(width =, height =)`, in inches.
#' @export
#' @examples
#' dag <- as_dag(data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")))
#' dag_size(ggplot_dag(dag))
#' dag_size(ggplot_dag(dag, node_color = "name"))  # wider: includes the legend
dag_size <- function(plot) {
  if (!is.null(plot$theme$panel.widths) && !is.null(plot$theme$panel.heights)) {
    # ggplotGrob() opens a device if none is open; don't leave Rplots.pdf behind
    if (grDevices::dev.cur() == 1) {
      grDevices::pdf(NULL)
      on.exit(grDevices::dev.off(), add = TRUE)
    }
    gt <- ggplot2::ggplotGrob(plot)
    heights <- grid::convertHeight(gt$heights, "in", valueOnly = TRUE)
    # A legend taller than the panel (a few nodes, many legend entries) isn't
    # laid out in the plot's own height: it is centered on the panel and
    # spills over both ends. The figure is centered on the whole plot, though,
    # and a title (or anything else above or below the panel) puts the panel
    # off that center: the legend then reaches `|above - below| / 2` further
    # towards the lighter side. So the figure must be as tall as the legend
    # plus that offset, and `dag_size_margin` for text measured differently on
    # the device that draws it.
    boxes <- gt$grobs[gt$layout$name %in% c("guide-box-right", "guide-box-left")]
    legend_height <- max(0, vapply(
      boxes,
      function(box) {
        if (inherits(box, "gtable")) {
          grid::convertHeight(sum(box$heights), "in", valueOnly = TRUE)
        } else {
          0
        }
      },
      numeric(1)
    ))
    panel <- gt$layout[gt$layout$name == "panel", ]
    above <- sum(heights[seq_len(panel$t[1] - 1L)])
    below <- sum(heights[-seq_len(panel$b[1])])
    return(c(
      width = grid::convertWidth(sum(gt$widths), "in", valueOnly = TRUE),
      height = max(
        sum(heights),
        (legend_height + abs(above - below)) * (1 + dag_size_margin)
      )
    ))
  }
  limits <- plot$coordinates$limits
  if (is.null(limits) || is.null(limits$x) || is.null(limits$y)) {
    stop(
      "`plot` has no coord_fixed() limits; build it with ggplot_dag() or dag_layers().",
      call. = FALSE
    )
  }
  c(
    width = max(diff(limits$x), 1) / 96,
    height = max(abs(diff(limits$y)), 1) / 96
  )
}

#' Save a DAG plot at its true pixel size
#'
#' Sizes the output with [dag_size()], so that one data unit equals one SVG
#' pixel (times `ggplot_dag(zoom =)`) at `scale` 1, at 96 dpi.
#'
#' `scale` raises resolution (`dpi = 96 * scale`) rather than the physical
#' `width`/`height`: line widths and text are set in absolute units (points),
#' so scaling the canvas size at a fixed dpi would make them relatively
#' thinner/smaller against the (now larger) node spacing. Scaling dpi
#' instead keeps every element's proportions exactly as dagviz draws them,
#' just at higher resolution.
#'
#' For a fixed-size plot (the [ggplot_dag()] default) the output also
#' makes room for any legend or title. For a plot with
#' `fixed_size = FALSE` (or manual assembly), the size is the view box's
#' alone, so a legend or title shrinks the panel to fit.
#'
#' @param filename File to write to, passed to [ggplot2::ggsave()].
#' @param plot A plot from [ggplot_dag()] (or any ggplot with `coord_fixed()`
#'   limits set the same way, e.g. via manual assembly with [dag_layers()]).
#' @param scale Resolution multiplier (applied to `dpi`, not physical size).
#' @param ... Passed on to [ggplot2::ggsave()].
#' @return The result of [ggplot2::ggsave()], invisibly.
#' @export
#' @examples
#' dag <- as_dag(data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")))
#' p <- ggplot_dag(dag)
#' ggsave_dag(tempfile(fileext = ".png"), p)
ggsave_dag <- function(filename, plot = ggplot2::last_plot(), scale = 1, ...) {
  # not a style preference: `scale` becomes ggsave()'s `dpi`, and a negative
  # one crashes the png device outright (segfault), taking the session down
  if (!is.numeric(scale) || length(scale) != 1 || is.na(scale) || scale <= 0) {
    stop("`scale` must be a single positive number.", call. = FALSE)
  }
  size <- dag_size(plot)
  ggplot2::ggsave(
    filename,
    plot = plot,
    width = size[["width"]],
    height = size[["height"]],
    units = "in",
    dpi = 96 * scale,
    ...
  )
}
