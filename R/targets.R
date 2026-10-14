#' Error clearly if targets isn't installed
#'
#' A small wrapper around `requireNamespace()` so `local_mocked_bindings()`
#' can simulate "targets not installed" in tests without needing an actual
#' missing installation (a bare `requireNamespace()` call can't be mocked
#' this way, since it isn't a binding in this package's own namespace).
#' @keywords internal
check_targets_installed <- function() {
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop(
      "This needs the targets package. Install it with install.packages(\"targets\").",
      call. = FALSE
    )
  }
}

#' The **targets** status color palette
#'
#' The colors `targets::tar_visnetwork()` uses for target status, as a
#' hard-coded copy - they belong to `targets`, not `ggtargets`, and are
#' checked against the installed `targets` version in the package's own
#' tests, so a drift is caught rather than silently shipped. Fits the
#' `palette` argument of [tar_ggplot()]/[ggplot_dag()]/[dag_layers()]
#' directly, e.g.
#' `tar_ggplot(node_color = "status", palette = targets_status_colors())`.
#'
#' @return A named character vector of hex colors, one per status.
#' @export
#' @examples
#' targets_status_colors()
#'
#' dag <- as_dag(
#'   data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")),
#'   nodes = data.frame(name = c("a", "b", "c"), status = c("uptodate", "outdated", "errored"))
#' )
#' ggplot_dag(dag, node_color = "status", palette = targets_status_colors())
targets_status_colors <- function() {
  c(
    completed = "#E1BD6D",
    uptodate = "#354823",
    outdated = "#78B7C5",
    dispatched = "#DC863B",
    canceled = "#FAD510",
    errored = "#C93312",
    queued = "#D2D2D0",
    skipped = "#7500D1",
    none = "#94a4ac"
  )
}

#' The **targets** node shapes
#'
#' The shape `targets::tar_visnetwork()` draws for each vertex `type`, as a
#' hard-coded copy - they belong to `targets`, not `ggtargets`, and are
#' checked against the installed `targets` version in the package's own
#' tests. It is the default `shapes` of [tar_ggplot()]/[ggplot_dag()]/
#' [dag_layers()], so a pipeline's nodes are drawn by type: a regular target
#' (`stem`) is a circle, a dynamic-branching target (`pattern`) a square, and
#' an imported `function` and `object` an up and a down triangle. Imports are
#' only in the graph with `tar_ggplot(targets_only = FALSE)`.
#'
#' @return A named character vector of shape names, one per type. The shapes
#'   a node can take are `"circle"`, `"square"`, `"diamond"`, `"triangle"`
#'   and `"triangle_down"`.
#' @export
#' @examples
#' targets_node_shapes()
#'
#' dag <- as_dag(
#'   data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")),
#'   nodes = data.frame(
#'     name = c("a", "b", "c"),
#'     type = c("stem", "stem", "pattern")
#'   )
#' )
#' ggplot_dag(dag)
targets_node_shapes <- function() {
  c(
    stem = "circle",
    pattern = "square",
    `function` = "triangle",
    object = "triangle_down"
  )
}

#' Read the current **targets** pipeline as a **dag**
#'
#' A thin wrapper around [targets::tar_network()]: runs it, then converts
#' its `vertices`/`edges` result with [as_dag()], keeping `type` (which
#' [ggplot_dag()] draws as the node shape, see [targets_node_shapes()]),
#' `status`, `description`, `seconds`, `bytes` and `branches`.
#'
#' @param targets_only Passed to [targets::tar_network()]; `TRUE` (unlike
#'   `tar_network()`'s own default of `FALSE`) because imported functions
#'   and objects clutter a metro plot.
#' @param envir Environment to run the target script in. Forwarded
#'   explicitly (rather than left to `tar_network()`'s own default) so it
#'   resolves to the caller's environment, not a `ggtargets` function frame.
#' @param ... Passed to [targets::tar_network()]: `names`, `shortcut`,
#'   `allow`, `exclude`, `outdated`, `reporter`, `callr_function`,
#'   `callr_arguments`, `script`, `store`. `names`/`allow`/`exclude` accept
#'   tidyselect helpers exactly as in `tar_visnetwork()`.
#' @return A `dag` (see [as_dag()]).
#' @export
#' @examplesIf requireNamespace("targets", quietly = TRUE)
#' targets::tar_dir({ # doesn't pollute the working directory
#'   targets::tar_script(list(
#'     targets::tar_target(x, 1),
#'     targets::tar_target(y, x + 1)
#'   ))
#'   ggplot_dag(tar_dag())
#' })
tar_dag <- function(targets_only = TRUE, envir = parent.frame(), ...) {
  check_targets_installed()
  nw <- targets::tar_network(targets_only = targets_only, envir = envir, ...)
  as_dag(list(vertices = nw$vertices, edges = nw$edges))
}

#' Plot the current **targets** pipeline
#'
#' The headline one-call function: [tar_dag()] + [dag_layout()] +
#' [ggplot_dag()]. Mirrors `targets::tar_visnetwork()`'s filtering
#' arguments.
#'
#' To plot a pipeline in another folder: `script` and `store` locate the
#' pipeline's files, but `tar_network()` runs `_targets.R` from the current
#' working directory, and most pipelines use relative paths (e.g.
#' `tar_source("R")`). Use `withr::with_dir("path/to/project", tar_ggplot())`.
#'
#' As in `tar_visnetwork()`, the node shape shows its `type`: a circle for a
#' regular target, a square for a dynamic-branching one, and triangles for
#' imported functions and objects (only in the graph with `targets_only =
#' FALSE`). A legend names the shapes when more than one is drawn. See
#' [targets_node_shapes()].
#'
#' @param style,node_color,edge_color,palette,node_shape,shapes Passed to
#'   [dag_layout()]/[ggplot_dag()].
#' @param fixed_size,zoom Passed to [ggplot_dag()]: by default, the plot
#'   has a fixed size so nodes, lines and text look the same in every
#'   pipeline.
#' @param targets_only,envir,... Passed to [tar_dag()].
#' @return A `ggplot` object.
#' @export
#' @examplesIf requireNamespace("targets", quietly = TRUE)
#' # each call is its own tar_dir(), so each plot actually prints - inside
#' # one tar_dir({ ... }) block, only the last expression auto-prints
#' targets::tar_dir({
#'   targets::tar_script(list(
#'     targets::tar_target(x, 1),
#'     targets::tar_target(y, x + 1)
#'   ))
#'   tar_ggplot()
#' })
#' targets::tar_dir({
#'   targets::tar_script(list(
#'     targets::tar_target(x, 1),
#'     targets::tar_target(y, x + 1)
#'   ))
#'   tar_ggplot(node_color = "status", palette = targets_status_colors())
#' })
tar_ggplot <- function(
  style = style_metro(),
  node_color = NULL,
  edge_color = "source",
  palette = metro_palette,
  node_shape = NULL,
  shapes = targets_node_shapes(),
  fixed_size = TRUE,
  zoom = 1,
  targets_only = TRUE,
  envir = parent.frame(),
  ...
) {
  dag <- tar_dag(targets_only = targets_only, envir = envir, ...)
  layout <- dag_layout(dag, style = style)
  ggplot_dag(
    layout,
    node_color = node_color,
    edge_color = edge_color,
    palette = palette,
    node_shape = node_shape,
    shapes = shapes,
    fixed_size = fixed_size,
    zoom = zoom
  )
}
