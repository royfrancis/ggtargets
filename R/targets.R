# Whether targets is installed: a binding of this package's own, so that
# `local_mocked_bindings()` can simulate "targets not installed" in tests
# without an actual missing installation (a bare `requireNamespace()` call
# can't be mocked this way).
targets_installed <- function() {
  requireNamespace("targets", quietly = TRUE)
}

#' Error clearly if targets isn't installed
#'
#' Every function that needs `targets` calls this first.
#' @keywords internal
check_targets_installed <- function() {
  if (!targets_installed()) {
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
#' `description` is the one free-form field `targets` has for a target, and
#' can hold the stage it belongs to, for [dag_collapse()] to collapse the
#' pipeline by: see "Collapsing a pipeline by stage" in [tar_ggplot()].
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
#' The headline one-call function: [tar_dag()], then [dag_collapse()] if you
#' give a `collapse_by`, then [dag_layout()] and [ggplot_dag()]. Mirrors
#' `targets::tar_visnetwork()`'s filtering arguments.
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
#' @section Collapsing a pipeline by stage:
#' `targets` has no tag or category field for a target, but it has one free-form
#' text field, `description`, and `collapse_by = "description"` collapses the
#' pipeline by it: the targets that share a description become one node,
#' labeled with how many they are (`ingest (4)`), and the dependencies between
#' stages become one edge each (see [dag_collapse()]). With `node_size = "n"`
#' and `edge_width = "n"`, the size of a node and the width of an edge show
#' those counts.
#'
#' A description can be set for each target, `tar_target(raw, read_data(),
#' description = "ingest")`, or for a block of them with `tar_option_set()`,
#' which applies to every target defined after it:
#'
#' ```r
#' # _targets.R
#' library(targets)
#' tar_option_set(description = "ingest")
#' ingest <- list(tar_target(raw, read_data()), tar_target(clean, tidy(raw)))
#' tar_option_set(description = "model")
#' model <- list(tar_target(fit, fit_model(clean)))
#' tar_option_set(description = character(0)) # back to no description
#' c(ingest, model)
#' ```
#'
#' ```r
#' tar_ggplot(collapse_by = "description", node_size = "n", edge_width = "n")
#' ```
#'
#' Things to know:
#' * End a block with `tar_option_set(description = character(0))`. The option
#'   stays set for every target defined after it, including those in files
#'   loaded later with `tar_source()`.
#' * `tarchetypes::tar_map()` appends the mapped values to each description by
#'   default (`descriptions = tidyselect::everything()`), so the targets it
#'   makes would be stages of their own. Pass `descriptions = NULL` to keep the
#'   stage's description.
#' * The descriptions must match exactly: `"ingest"` and `"Ingest "` are two
#'   stages.
#' * A target with no description is a node of its own. A stage can't be named
#'   like such a target (an error: the two would be merged).
#' * Stages that depend on each other both ways (A -> B -> A) are an error that
#'   names the edges behind it.
#' * `description` is also the label of a target in [targets::tar_manifest()]
#'   and [targets::tar_visnetwork()], so a short stage name is a good one, and
#'   a sentence is not.
#' * `node_color = "status"` colors a stage by its status when all its targets
#'   share one. A stage whose targets differ has no status, and is drawn gray
#'   with no legend entry, as is any node with a missing value in the
#'   `node_color` column.
#' * For metadata of your own, add a column to the nodes of [tar_dag()]'s result
#'   (its names can't be `row`, `col`, `x`, `y` or `color_index`), then call
#'   [dag_collapse()] and [ggplot_dag()] yourself.
#'
#' @param style,node_color,edge_color,palette,node_shape,shapes,node_size,edge_width
#'   Passed to [dag_layout()]/[ggplot_dag()]. With `collapse_by`, the `n` of
#'   the collapsed graph's nodes and edges (the number of targets and of
#'   dependencies each stands for) is what `node_size = "n"` and
#'   `edge_width = "n"` draw.
#' @param collapse_by `NULL` (the default), or the name of a node column to
#'   collapse the pipeline by: the targets that share a value are merged
#'   into one node (see [dag_collapse()]). [tar_dag()] returns the columns
#'   `type`, `status`, `description`, `seconds`, `bytes` and `branches`, and
#'   `description` is the one to put a stage name in: see "Collapsing a
#'   pipeline by stage" above.
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
#'
#' # the pipeline in stages, kept in the descriptions of its targets
#' targets::tar_dir({
#'   targets::tar_script(list(
#'     targets::tar_target(raw, 1, description = "ingest"),
#'     targets::tar_target(clean, raw + 1, description = "ingest"),
#'     targets::tar_target(fit, clean * 2, description = "model"),
#'     targets::tar_target(out, fit, description = "report")
#'   ))
#'   tar_ggplot(collapse_by = "description", node_size = "n", edge_width = "n")
#' })
tar_ggplot <- function(
  style = style_metro(),
  node_color = NULL,
  edge_color = "source",
  palette = metro_palette,
  node_shape = NULL,
  shapes = targets_node_shapes(),
  node_size = NULL,
  edge_width = NULL,
  collapse_by = NULL,
  fixed_size = TRUE,
  zoom = 1,
  targets_only = TRUE,
  envir = parent.frame(),
  ...
) {
  dag <- tar_dag(targets_only = targets_only, envir = envir, ...)
  if (!is.null(collapse_by)) {
    dag <- dag_collapse(dag, collapse_by)
  }
  layout <- dag_layout(dag, style = style)
  ggplot_dag(
    layout,
    node_color = node_color,
    edge_color = edge_color,
    palette = palette,
    node_shape = node_shape,
    shapes = shapes,
    node_size = node_size,
    edge_width = edge_width,
    fixed_size = fixed_size,
    zoom = zoom
  )
}
