#' Build a **dag** from an edge list or other graph representation
#'
#' `as_dag()` is the entry point for any DAG that isn't read from a
#' `targets` pipeline (see [tar_dag()]). It normalizes the input into a
#' `dag`: a classed list with `nodes` and `edges` data frames, keeping
#' insertion order throughout, the way `networkx` does.
#'
#' @param x An edge-list data frame with `from` and `to` columns, or a
#'   `list` with `vertices` and `edges` elements (the shape
#'   [targets::tar_network()] returns; see [tar_dag()]). Any other columns
#'   of the edge-list data frame are kept as edge attributes; a numeric one
#'   can set the edges' width (`edge_width`, see [ggplot_dag()]).
#' @param nodes Optional data frame of nodes, with at least a `name` column,
#'   listed before any additional nodes implied only by `x`'s endpoints. Its
#'   other columns are node attributes, but not named `row`, `col`, `x`, `y`
#'   or `color_index`: the layout uses those names for its own columns
#'   (see [dag_layout()]), so these names are reserved, and using one is an
#'   error.
#' @param ... Passed on to methods.
#' @return A `dag`: a classed list with `nodes` (columns `name`, `label`,
#'   plus any extra attributes, which `node_color`, `node_shape` and, for a
#'   numeric one, `node_size` can name) and `edges` (columns `from`, `to`,
#'   plus any extra columns of `x`, which only `edge_width` uses for
#'   drawing) data frames, in insertion order.
#' @export
#' @examples
#' edges <- data.frame(from = c("a", "a", "b"), to = c("b", "c", "c"))
#' dag <- as_dag(edges)
#' print(dag)
#' ggplot_dag(dag)
#'
#' # a node table, with extra attributes for coloring
#' nodes <- data.frame(name = c("a", "b", "c"), type = c("x", "y", "x"))
#' dag2 <- as_dag(edges, nodes = nodes)
#' ggplot_dag(dag2, node_color = "type")
as_dag <- function(x, ...) {
  UseMethod("as_dag")
}

#' @rdname as_dag
#' @export
as_dag.data.frame <- function(x, nodes = NULL, ...) {
  missing_cols <- setdiff(c("from", "to"), names(x))
  if (length(missing_cols) > 0) {
    stop(
      "as_dag(): `x` is missing column(s): ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(nodes) && !"name" %in% names(nodes)) {
    stop("as_dag(): `nodes` is missing column: name", call. = FALSE)
  }
  if (!is.null(nodes)) {
    check_node_columns(names(nodes), "as_dag()", "`nodes`")
  }
  edges <- x
  edges$from <- as.character(edges$from)
  edges$to <- as.character(edges$to)
  check_node_names(edges$from, "`x$from`")
  check_node_names(edges$to, "`x$to`")
  if (!is.null(nodes)) {
    check_node_names(as.character(nodes$name), "`nodes$name`")
  }
  edges <- edges[!duplicated(edges[c("from", "to")]), , drop = FALSE]
  rownames(edges) <- NULL

  self_loops <- unique(edges$from[edges$from == edges$to])
  if (length(self_loops) > 0) {
    stop(
      "as_dag(): self-loop(s) found: ",
      paste(self_loops, collapse = ", "),
      call. = FALSE
    )
  }

  node_names <- character(0)
  extra <- NULL
  if (!is.null(nodes)) {
    node_names <- as.character(nodes$name)
    extra <- nodes
  }
  node_names <- unique(c(node_names, c(rbind(edges$from, edges$to))))

  cyc <- find_cycle(node_names, edges)
  if (!is.null(cyc)) {
    stop(
      "as_dag(): the graph has a cycle: ",
      paste(cyc, collapse = " -> "),
      call. = FALSE
    )
  }

  nodes_df <- data.frame(
    name = node_names,
    label = node_names,
    stringsAsFactors = FALSE
  )
  if (!is.null(extra)) {
    extra_cols <- setdiff(names(extra), c("name", "label"))
    for (col in extra_cols) {
      nodes_df[[col]] <- extra[[col]][match(nodes_df$name, extra$name)]
    }
    if ("label" %in% names(extra)) {
      # as.character(): ifelse() would turn a factor into its integer codes
      lab <- as.character(extra$label[match(nodes_df$name, extra$name)])
      nodes_df$label <- ifelse(is.na(lab), nodes_df$name, lab)
    }
  }

  structure(list(nodes = nodes_df, edges = edges), class = "dag")
}

#' @rdname as_dag
#' @details
#' `as_dag.list()` deliberately narrows the return value described above.
#' It is the [tar_dag()] path, so it keeps only the `vertices` columns a
#' metro plot can use - `type`, `status`, `description`, `seconds`,
#' `bytes` and `branches` - and no extra `edges` columns. To color by
#' other node attributes, pass an edge-list data frame and a `nodes`
#' table to `as_dag.data.frame()` instead.
#'
#' Node names are identities, and the package matches edge colors on them
#' downstream, so every method rejects a name that is `NA`, empty
#' or contains `"|"` or a carriage return.
#' @export
as_dag.list <- function(x, ...) {
  if (!all(c("vertices", "edges") %in% names(x))) {
    return(as_dag.default(x, ...))
  }
  vertices <- as.data.frame(x$vertices)
  edges <- as.data.frame(x$edges)
  keep_cols <- intersect(
    c("type", "status", "description", "seconds", "bytes", "branches"),
    names(vertices)
  )
  nodes <- data.frame(
    name = as.character(vertices$name),
    stringsAsFactors = FALSE
  )
  for (col in keep_cols) {
    nodes[[col]] <- vertices[[col]]
  }
  as_dag.data.frame(
    data.frame(
      from = as.character(edges$from),
      to = as.character(edges$to),
      stringsAsFactors = FALSE
    ),
    nodes = nodes
  )
}

#' @rdname as_dag
#' @export
as_dag.default <- function(x, ...) {
  stop(
    "as_dag() doesn't know how to convert an object of class ",
    paste(class(x), collapse = "/"),
    " to a dag. ",
    "Convert it to an edge-list data frame (columns `from` and `to`) first.",
    call. = FALSE
  )
}

#' Reject node names that can't serve as identities
#'
#' Node names are this package's join keys: downstream, edge colors are
#' matched on `paste(from, to, sep = "|")` (see
#' [resolve_edge_color()]), and `NA` has no identity at all. Neither has an
#' empty name, which can't even be looked up by name in an environment (as
#' [find_cycle()] does). Rejecting `|` and carriage returns in the one
#' constructor every `dag` passes through is what makes those keys
#' collision-free everywhere else, without the rendering path needing a
#' collision-proof key scheme of its own.
#'
#' Names only: node attributes such as `status`, `seconds` and `bytes`
#' legitimately carry `NA` and are untouched.
#'
#' @param x A character vector of node names.
#' @param what Which argument `x` came from, for the error message.
#' @return `x`, invisibly.
#' @keywords internal
check_node_names <- function(x, what) {
  if (anyNA(x)) {
    stop("as_dag(): ", what, " has missing (NA) node name(s).", call. = FALSE)
  }
  if (any(x == "")) {
    stop("as_dag(): ", what, " has empty (\"\") node name(s).", call. = FALSE)
  }
  bad <- unique(x[grepl("[|\r]", x)])
  if (length(bad) > 0) {
    stop(
      "as_dag(): node names can't contain \"|\" or a carriage return; ",
      # encodeString(): a raw carriage return in the message would overwrite
      # the line it's printed on, and mangle the error's own snapshot
      what,
      " has: ",
      paste(encodeString(bad, quote = "\""), collapse = ", "),
      call. = FALSE
    )
  }
  invisible(x)
}

# The columns of a layout's `nodes` besides `name` and `label`: `dag_layout()`
# copies a dag's extra node columns next to them, so a dag column of the same
# name would overwrite one (a column `x` replaced the node's coordinates). The
# names are reserved instead. test-dag.R checks this list against the layout's
# own columns, so a new one can't be forgotten.
reserved_node_columns <- c("row", "col", "x", "y", "color_index")

#' Reject node columns that the layout needs for itself
#'
#' A node column named like one of the layout's own (see
#' `reserved_node_columns`) would silently overwrite it in [dag_layout()]:
#' the nodes would be drawn where the column says, or colored by it. An error
#' is the only choice that never draws a wrong plot: a warning would still
#' have to say which of the two columns wins.
#'
#' Checked by [as_dag()], and again by [dag_layout()], since a dag edited
#' after `as_dag()` (`d$nodes$x <- 1`) or built by hand never passes through
#' it.
#'
#' @param columns A character vector of node column names.
#' @param fn,what Which function raised the error, and which argument the
#'   columns came from, for the message.
#' @return `columns`, invisibly.
#' @keywords internal
check_node_columns <- function(columns, fn, what) {
  bad <- intersect(columns, reserved_node_columns)
  if (length(bad) > 0L) {
    stop(
      sprintf(
        "%s: %s has column(s) named like the layout's own: %s. These names are reserved (%s); rename the column(s).",
        fn,
        what,
        paste(bad, collapse = ", "),
        paste(reserved_node_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(columns)
}

#' Find one cycle in a graph, if any, by iterative depth-first search
#'
#' Used by [as_dag()] to name a cycle in its error, rather than just report
#' that one exists. Iterative (an explicit stack), not the recursive
#' one-call-per-node version this started as: every [tar_dag()] call runs
#' this, and a real pipeline can have thousands of targets in a chain,
#' which overflowed R's call stack ("node stack overflow") well before
#' that.
#' @param nodes A character vector of node names (non-empty, not `NA`).
#' @param edges A data frame with `from` and `to` character columns, naming
#'   only nodes in `nodes`.
#' @return A character vector naming one cycle, from a node back to itself
#'   (`c("a", "b", "a")`), or `NULL` if there is none.
#' @keywords internal
find_cycle <- function(nodes, edges) {
  succ <- split(edges$to, factor(edges$from, levels = nodes))
  state <- new.env(hash = TRUE, parent = emptyenv()) # 0/absent = unvisited, 1 = in progress, 2 = done
  pos <- new.env(hash = TRUE, parent = emptyenv()) # next child index to visit, per stack frame

  for (start in nodes) {
    if (!is.null(state[[start]])) {
      next
    }
    stack <- start
    state[[start]] <- 1L
    pos[[start]] <- 1L
    while (length(stack) > 0L) {
      cur <- stack[length(stack)]
      children <- succ[[cur]]
      i <- pos[[cur]]
      if (i > length(children)) {
        state[[cur]] <- 2L
        stack <- stack[-length(stack)]
        next
      }
      pos[[cur]] <- i + 1L
      child <- children[i]
      child_state <- state[[child]]
      if (!is.null(child_state) && child_state == 1L) {
        idx <- match(child, stack)
        return(c(stack[idx:length(stack)], child))
      } else if (is.null(child_state)) {
        state[[child]] <- 1L
        pos[[child]] <- 1L
        stack <- c(stack, child)
      }
    }
  }
  NULL
}

# "1 node", "2 nodes": for the one-line summaries below
count_noun <- function(n, noun) {
  sprintf("%d %s%s", n, noun, if (n == 1L) "" else "s")
}

#' @export
format.dag <- function(x, ...) {
  sprintf(
    "<dag> %s, %s",
    count_noun(nrow(x$nodes), "node"),
    count_noun(nrow(x$edges), "edge")
  )
}

#' @export
print.dag <- function(x, ...) {
  cat(format(x), "\n", sep = "")
  invisible(x)
}

#' @export
format.dag_layout <- function(x, ...) {
  n_edges <- if (is.null(x$edge_paths) || nrow(x$edge_paths) == 0) {
    0L
  } else {
    nrow(unique(x$edge_paths[c("from", "to")]))
  }
  n_cols <- length(unique(x$nodes$col))
  sprintf(
    "<dag_layout> %s, %s, %s",
    count_noun(nrow(x$nodes), "node"),
    count_noun(n_edges, "edge"),
    count_noun(n_cols, "column")
  )
}

#' @export
print.dag_layout <- print.dag
