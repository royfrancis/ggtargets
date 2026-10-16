#' Collapse a DAG by a node column
#'
#' Collapses a `dag`: the nodes that share a value of the `by` column are
#' merged into one node, named after that value, and the edges are projected
#' onto these new nodes. A pipeline of fifty targets in four stages becomes a
#' graph of four nodes, drawn by the same functions ([ggplot_dag()],
#' [tar_ggplot()]) as any other `dag`.
#'
#' Any node column works as `by`, so there is nothing special about its name:
#' a column of your own (passed to [as_dag()] as `nodes`), or one that
#' [tar_dag()] returns - `description`, `type` or `status`. `targets` has no
#' tag or category field of its own, and `description` is its one free-form
#' text, so a pipeline's stages go there: see "Collapsing a pipeline by
#' stage" in [tar_ggplot()]. For any other metadata, add a column to
#' `dag$nodes` yourself first (not named `row`, `col`, `x`, `y` or
#' `color_index`).
#'
#' A node with no value in `by` (`NA` or an empty string) stays on its own,
#' and a value that is also the name of such a node is an error, since the
#' two would be merged.
#'
#' * **Nodes.** `name` is the value of `by`, and `label` is that value with
#'   the member count, `"model (7)"`. A node with no `by` value is not
#'   merged: it stays as it is, in its own place. The order is the
#'   order of first appearance, so each merged node sits where its first
#'   member was. `n` counts the members (replacing any `n` column you
#'   already have, so `n` itself is not a useful `by`).
#' * **Other node columns.** `seconds`, `bytes` and `branches` are summed
#'   (`NA` if every member is `NA`). Any other column keeps its value when
#'   all the members share it, and is `NA` otherwise; in particular the `by`
#'   column itself keeps its value (even if it is one of the columns that
#'   are otherwise summed), so `node_color = by` colors the collapsed
#'   graph.
#' * **Edges.** An edge between two members of one group disappears. The
#'   others are drawn between the groups, once per pair of groups, with an
#'   `n` column that counts the edges it stands for. Any other edge column is
#'   dropped.
#'
#' Merging nodes can create a cycle, which a DAG can't have: with `a -> b ->
#' c`, `a` and `c` in group `X` and `b` in group `Y`, the collapsed graph has `X -> Y
#' -> X`. That is an error, naming the groups and the edges behind it. Group
#' by a different column, or move the node that closes the loop.
#'
#' @param dag A `dag`, from [as_dag()] or [tar_dag()].
#' @param by The name of a node column (a single string). Its values are
#'   used as text.
#' @param label_n If `TRUE` (the default), a merged node's label ends with its
#'   member count, `"model (7)"`; otherwise it is just the value.
#' @return A `dag`, with the columns described above: nodes `name`, `label`,
#'   `n` and the rolled-up columns, and edges `from`, `to` and `n`.
#' @export
#' @examples
#' nodes <- data.frame(
#'   name = c("raw", "clean", "fit", "eval", "report"),
#'   stage = c("data", "data", "model", "model", "report")
#' )
#' edges <- data.frame(
#'   from = c("raw", "clean", "fit", "eval"),
#'   to = c("clean", "fit", "eval", "report")
#' )
#' dag <- as_dag(edges, nodes = nodes)
#' small <- dag_collapse(dag, "stage")
#' small$nodes
#' small$edges
#' ggplot_dag(small, node_color = "stage")
#'
#' # a grouping that closes a loop is an error
#' bad <- as_dag(
#'   data.frame(from = c("a", "b"), to = c("b", "c")),
#'   nodes = data.frame(name = c("a", "b", "c"), group = c("X", "Y", "X"))
#' )
#' try(dag_collapse(bad, "group"))
#' @examplesIf requireNamespace("targets", quietly = TRUE)
#' # the targets' own `description` as the key
#' targets::tar_dir({
#'   targets::tar_script(list(
#'     targets::tar_target(x, 1, description = "input"),
#'     targets::tar_target(y, x + 1, description = "input"),
#'     targets::tar_target(z, y * 2, description = "output")
#'   ))
#'   ggplot_dag(dag_collapse(tar_dag(), "description"))
#' })
dag_collapse <- function(dag, by, label_n = TRUE) {
  if (!inherits(dag, "dag")) {
    stop(
      "dag_collapse(): `dag` must be a dag, from as_dag() or tar_dag().",
      call. = FALSE
    )
  }
  if (!is.character(by) || length(by) != 1L || is.na(by)) {
    stop("dag_collapse(): `by` must be a single node column name.", call. = FALSE)
  }
  if (!by %in% names(dag$nodes)) {
    stop(
      sprintf(
        "dag_collapse(): `by` ('%s') is not a node column; the columns are %s.",
        by,
        paste(names(dag$nodes), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (!is.logical(label_n) || length(label_n) != 1L || is.na(label_n)) {
    stop("dag_collapse(): `label_n` must be TRUE or FALSE.", call. = FALSE)
  }

  nodes <- dag$nodes
  value <- as.character(nodes[[by]])
  # "" is how a spreadsheet says "no value", so it stays on its own like NA
  grouped <- !is.na(value) & nzchar(value)

  # The values become node names, so they must be valid ones (see
  # check_node_names()), and must not collide with the nodes left alone:
  # as_dag() would merge two nodes of one name without a word.
  bad <- unique(value[grouped & grepl("[|\r]", value)])
  if (length(bad) > 0L) {
    stop(
      sprintf(
        "dag_collapse(): the values of `%s` become node names, which can't contain \"|\" or a carriage return: %s",
        by,
        paste(encodeString(bad, quote = "\""), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  clash <- intersect(nodes$name[!grouped], value[grouped])
  if (length(clash) > 0L) {
    stop(
      sprintf(
        "dag_collapse(): `%s` has value(s) that are also the name of a node with no `%s`: %s. They would be merged; rename one.",
        by,
        by,
        paste(clash, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # `target`: the node each original node ends up as. Names are in order of
  # first appearance, so the collapsed dag's order follows the dag's.
  target <- ifelse(grouped, value, nodes$name)
  new_names <- unique(target)
  k <- length(new_names)
  group <- match(target, new_names)
  first <- match(seq_len(k), group)
  n <- tabulate(group, k)
  merged <- grouped[first]

  out <- data.frame(name = new_names, stringsAsFactors = FALSE)
  label <- nodes$label[first]
  label[merged] <- if (label_n) {
    sprintf("%s (%d)", new_names[merged], n[merged])
  } else {
    new_names[merged]
  }
  out$label <- label
  for (col in setdiff(names(nodes), c("name", "label"))) {
    x <- nodes[[col]]
    out[[col]] <- if (
      col != by && col %in% c("seconds", "bytes", "branches") && is.numeric(x)
    ) {
      collapse_sum(x, group)
    } else {
      collapse_uniform(x, group, k)
    }
  }
  out$n <- n

  edges <- dag$edges
  from <- target[match(edges$from, nodes$name)]
  to <- target[match(edges$to, nodes$name)]
  across <- from != to
  key <- paste(from[across], to[across], sep = "|") # names have no "|"
  keep <- !duplicated(key)
  pairs <- data.frame(
    from = from[across][keep],
    to = to[across][keep],
    stringsAsFactors = FALSE
  )
  pairs$n <- tabulate(match(key, key[keep]), nrow(pairs))

  cyc <- find_cycle(new_names, pairs)
  if (!is.null(cyc)) {
    hops <- vapply(
      seq_len(length(cyc) - 1L),
      function(i) {
        hit <- which(from == cyc[[i]] & to == cyc[[i + 1L]])
        more <- length(hit) - 1L
        sprintf(
          "  %s -> %s: %s -> %s%s",
          cyc[[i]],
          cyc[[i + 1L]],
          edges$from[[hit[[1L]]]],
          edges$to[[hit[[1L]]]],
          if (more > 0L) sprintf(" (and %d more)", more) else ""
        )
      },
      character(1)
    )
    stop(
      sprintf(
        "dag_collapse(): collapsing by `%s` creates a cycle: %s\nThrough these edges:\n%s",
        by,
        paste(cyc, collapse = " -> "),
        paste(hops, collapse = "\n")
      ),
      call. = FALSE
    )
  }

  as_dag(pairs, nodes = out)
}

#' Sum a node column over the members of each group
#'
#' `NA` if every member is `NA`, so a group without a measurement doesn't
#' read as zero.
#' @param x A numeric vector, one value per original node.
#' @param group For each node, the index of the node it is merged into
#'   (`1..k`, all present).
#' @return A numeric vector of length `k`.
#' @keywords internal
collapse_sum <- function(x, group) {
  seen <- !is.na(x)
  x[!seen] <- 0L
  total <- as.vector(rowsum(x, group))
  total[as.vector(rowsum(as.integer(seen), group)) == 0L] <- NA
  total
}

#' A node column's value over the members of each group, if they share it
#'
#' The group's value when every member has it, `NA` otherwise. Keeps the
#' column's type.
#' @param x A vector, one value per original node.
#' @param group For each node, the index of the node it is merged into
#'   (`1..k`, all present).
#' @param k The number of groups.
#' @return A vector of length `k`, of `x`'s type.
#' @keywords internal
collapse_uniform <- function(x, group, k) {
  out <- x[match(seq_len(k), group)]
  mixed <- vapply(
    split(seq_along(x), factor(group, levels = seq_len(k))),
    function(i) length(unique(x[i])) > 1L,
    logical(1)
  )
  out[mixed] <- NA
  out
}
