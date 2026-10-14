#' Node order for the metro layout
#'
#' Reproduces `networkx.topological_sort()` as used by dagviz: Kahn's
#' algorithm processed by generations, using node-table and edge-table row
#' order (never re-sorted) to break ties exactly as networkx 3.x does.
#' Assumes an acyclic graph: [as_dag()] rejects cycles up front.
#'
#' @param nodes Character vector of node names, in insertion order.
#' @param edges Data frame with `from` and `to` character columns, in
#'   insertion order, with no duplicate edges.
#' @return `nodes`, permuted into row order for the metro layout.
#' @keywords internal
metro_order <- function(nodes, edges) {
  n <- length(nodes)
  indeg <- stats::setNames(integer(n), nodes)
  if (nrow(edges) > 0) {
    counts <- table(factor(edges$to, levels = nodes))
    indeg[names(counts)] <- as.integer(counts)
  }
  succ <- split(edges$to, factor(edges$from, levels = nodes))

  zero <- nodes[indeg == 0L]
  order <- character(0)
  while (length(zero) > 0L) {
    gen <- zero
    zero <- character(0)
    for (node in gen) {
      for (child in succ[[node]]) {
        indeg[child] <- indeg[child] - 1L
        if (indeg[child] == 0L) {
          zero <- c(zero, child)
        }
      }
    }
    order <- c(order, gen)
  }
  order
}

#' Lane assignment for the metro layout
#'
#' Reproduces dagviz's `AbstractPlot`/`AbstractRow` column bookkeeping
#' (`abstract.py`): each row inherits the previous row's still-active lanes,
#' predecessors mark and possibly free their lane, and the node takes the
#' most recently freed lane (LIFO) or opens a new one to the left.
#'
#' A subtlety reproduced here: a cell's `start_row` (the row rendering reads
#' to find "the lane's start node") lags one row behind `remaining`/`color`
#' when a lane is reused in the same row it was freed: the row that reuses a
#' lane still reports the *previous* owner's start row, and only the row
#' after that reports the new owner's row. This matches dagviz's separate
#' `next_row` bookkeeping (`add_node` always updates it, but a cell's own
#' `start_row` is fixed when the cell object is created).
#'
#' @param nodes Character vector of node names, in insertion order.
#' @param edges Data frame with `from` and `to` character columns.
#' @param order Row order, as returned by [metro_order()].
#' @return A list with `cells` (a data frame with one row per active
#'   `(row, column)` cell, columns `row`, `label`, `column`, `is_node`,
#'   `is_input`, `color`, `remaining`, `start_row`) and `columns` (a named
#'   integer vector `c(start =)`, the leftmost column).
#' @keywords internal
metro_lanes <- function(nodes, edges, order) {
  succ <- split(edges$to, factor(edges$from, levels = nodes))
  pred <- split(edges$from, factor(edges$to, levels = nodes))

  lanes <- list() # column (as character) -> list(color, remaining, next_row)
  gaps <- integer(0) # LIFO stack of freed columns
  mapping <- stats::setNames(integer(0), character(0)) # node -> its home column
  columns_start <- 0L
  color_counter <- 0L
  cell_rows <- vector("list", length(order))

  for (i in seq_along(order)) {
    node <- order[i]
    row_idx <- i - 1L

    row_cells <- lapply(lanes, function(l) {
      list(
        color = l$color,
        remaining = l$remaining,
        start_row = l$next_row,
        next_row = l$next_row,
        is_input = FALSE,
        is_node = FALSE
      )
    })

    for (p in pred[[node]]) {
      col <- mapping[[p]]
      key <- as.character(col)
      row_cells[[key]]$is_input <- TRUE
      row_cells[[key]]$remaining <- row_cells[[key]]$remaining - 1L
      if (row_cells[[key]]$remaining == 0L) {
        gaps <- c(gaps, col)
      }
    }

    nsuccs <- length(succ[[node]])
    if (length(gaps) > 0L) {
      col <- gaps[length(gaps)]
      gaps <- gaps[-length(gaps)]
    } else {
      col <- columns_start
      columns_start <- columns_start - 1L
    }
    key <- as.character(col)
    color <- color_counter
    color_counter <- color_counter + 1L
    mapping[node] <- col

    if (is.null(row_cells[[key]])) {
      row_cells[[key]] <- list(
        color = color,
        remaining = nsuccs,
        start_row = row_idx,
        next_row = row_idx,
        is_input = FALSE,
        is_node = TRUE
      )
    } else {
      row_cells[[key]]$is_node <- TRUE
      row_cells[[key]]$remaining <- nsuccs
      row_cells[[key]]$color <- color
      row_cells[[key]]$next_row <- row_idx
    }
    if (nsuccs == 0L) {
      gaps <- c(gaps, col)
    }

    keys <- names(row_cells)
    keys <- keys[order(as.integer(keys))]
    cell_rows[[i]] <- data.frame(
      row = row_idx,
      label = node,
      column = as.integer(keys),
      is_node = vapply(row_cells[keys], `[[`, logical(1), "is_node"),
      is_input = vapply(row_cells[keys], `[[`, logical(1), "is_input"),
      color = vapply(row_cells[keys], `[[`, integer(1), "color"),
      remaining = vapply(row_cells[keys], `[[`, integer(1), "remaining"),
      start_row = vapply(row_cells[keys], `[[`, integer(1), "start_row"),
      stringsAsFactors = FALSE
    )

    lanes <- Filter(function(l) l$remaining != 0L, row_cells)
    lanes <- lapply(lanes, function(l) {
      list(color = l$color, remaining = l$remaining, next_row = l$next_row)
    })
  }

  cells <- rbind_or_empty(
    cell_rows,
    data.frame(
      row = integer(0),
      label = character(0),
      column = integer(0),
      is_node = logical(0),
      is_input = logical(0),
      color = integer(0),
      remaining = integer(0),
      start_row = integer(0)
    )
  )
  rownames(cells) <- NULL
  list(cells = cells, columns = c(start = columns_start))
}

#' Combine row data frames, falling back to a typed empty template
#'
#' `do.call(rbind, rows)` returns `NULL` for an empty list, which loses the
#' expected columns/types for downstream code; substitute `empty` in that
#' case.
#'
#' @param rows A list of same-shaped data frames (possibly empty).
#' @param empty A zero-row data frame with the expected columns/types.
#' @return A data frame.
#' @keywords internal
rbind_or_empty <- function(rows, empty) {
  if (length(rows) == 0L) {
    return(empty)
  }
  do.call(rbind, rows)
}

#' Metro-style pixel geometry
#'
#' Turns [metro_lanes()]'s `cells` table into SVG-pixel primitives, exactly
#' reproducing dagviz's `render.py`/`style/metro.py`.
#'
#' Rather than a literal port of `render.py`'s per-row, left-to-right walk
#' (which only exists to tell "before the node" from "after the node" while
#' it walks), this classifies each input cell by comparing its column to
#' its row's node column directly: a cell whose column is *less than* the
#' node's column was walked *before* the node (dagviz's `node_pos > 0`
#' branch, which confusingly calls `place_right_arc` - the arc bulges
#' *toward* the node, i.e. right), and a cell whose column is *greater* was
#' walked *after* it (`node_pos < 0`, `place_left_arc`). A cell whose column
#' *equals* the node's column is the "reused in the same row" case
#' (`node_pos == 0`, a straight `vline_node`, no arc).
#'
#' @param cells The `cells` data frame from [metro_lanes()].
#' @param columns The `columns` vector from [metro_lanes()].
#' @param style A `dag_style_metro` object, as returned by [style_metro()].
#' @return A list with `nodes`, `segments`, `arcs`, `labels`, `edge_paths`
#'   (each a data frame) and `viewbox` (a list), as described in
#'   [dag_layout()].
#' @keywords internal
metro_geometry <- function(cells, columns, style) {
  scale <- style$scale
  r <- style$arc_radius
  shift <- -columns[["start"]]

  coord <- function(col, row, dx = 0, dy = 0) {
    list(
      x = scale * 2 * (col + shift) + r * (dx + 1),
      y = scale * 2 * row + r * (dy + 1)
    )
  }

  nodes_cells <- cells[cells$is_node, , drop = FALSE]
  node_col_by_row <- stats::setNames(nodes_cells$column, nodes_cells$row)
  owner_label <- stats::setNames(
    nodes_cells$label,
    paste(nodes_cells$row, nodes_cells$column)
  )
  owner_color <- stats::setNames(
    nodes_cells$color,
    paste(nodes_cells$row, nodes_cells$column)
  )
  owner_of <- function(row, col) {
    key <- paste(row, col)
    list(label = unname(owner_label[key]), color = unname(owner_color[key]))
  }

  cells$node_column <- node_col_by_row[as.character(cells$row)]
  input_cells <- cells[!cells$is_node & cells$is_input, , drop = FALSE]
  input_cells$side <- ifelse(
    input_cells$column < input_cells$node_column,
    "left",
    "right"
  )

  # ---- nodes ------------------------------------------------------------
  n_pt <- coord(nodes_cells$column, nodes_cells$row)
  nodes <- data.frame(
    name = nodes_cells$label,
    label = nodes_cells$label,
    row = nodes_cells$row,
    col = nodes_cells$column,
    x = n_pt$x,
    y = n_pt$y,
    color_index = nodes_cells$color,
    stringsAsFactors = FALSE
  )
  nodes <- nodes[order(nodes$row), ]
  rownames(nodes) <- NULL

  ord <- 0L
  add_seg <- function(layer, p0, p1, from, to) {
    ord <<- ord + 1L
    data.frame(
      layer = layer,
      x = p0$x,
      y = p0$y,
      xend = p1$x,
      yend = p1$y,
      from = from,
      to = to,
      order = ord,
      stringsAsFactors = FALSE
    )
  }

  # ---- vlines -------------------------------------------------------------
  # A vline is triggered either by a "same column as the node" reuse
  # (own_col: straight vline_node) or by a lane's *last* consumer elsewhere
  # (last_input: vline_arc, ending at the top of that row's arc). dagviz
  # emits these interleaved, in the row-walk's own order: by triggering row,
  # then by column within that row (both branches call `next_row = self`
  # unconditionally, so whichever column's touch happens to finish a lane
  # in a given row is drawn in column order alongside any other lane that
  # also finishes that same row).
  own_col <- cells[cells$is_node & cells$is_input, , drop = FALSE]
  last_input <- input_cells[input_cells$remaining == 0L, , drop = FALSE]
  vline_cand <- rbind(
    data.frame(
      row = own_col$row,
      column = own_col$column,
      start_row = own_col$start_row,
      own_row = rep(TRUE, nrow(own_col)),
      label = own_col$label,
      stringsAsFactors = FALSE
    ),
    data.frame(
      row = last_input$row,
      column = last_input$column,
      start_row = last_input$start_row,
      own_row = rep(FALSE, nrow(last_input)),
      label = last_input$label,
      stringsAsFactors = FALSE
    )
  )
  vline_cand <- vline_cand[order(vline_cand$row, vline_cand$column), ]
  vline_rows <- vector("list", nrow(vline_cand))
  for (k in seq_len(nrow(vline_cand))) {
    v <- vline_cand[k, ]
    owner <- owner_of(v$start_row, v$column)
    p1 <- if (v$own_row) {
      coord(v$column, v$row)
    } else {
      coord(v$column, v$row, dy = -1)
    }
    vline_rows[[k]] <- add_seg(
      "vline",
      coord(v$column, v$start_row),
      p1,
      owner$label,
      v$label
    )
  }

  # ---- arcs -----------------------------------------------------------
  # ---- arcs + left/right hlines + bridges --------------------------------
  # dagviz's render() walks one row's columns left to right and draws each
  # side's arcs *as it encounters them*, only emitting that side's hline
  # once the whole side has been walked (left: right when the node is
  # reached; right: right after the row's last column). So within a row,
  # in dagviz's own single shared <g>, the true order is: left arcs
  # (ascending column), left hline, right arcs (ascending column), right
  # hline. `arcs` and `segments` are separate tables, but their `order`
  # values are drawn from one shared counter here so sorting the union by
  # `order` reconstructs that sequence.
  arc_n_points <- 17L
  arc_geom <- function(col_i, row_i, side) {
    center <- coord(col_i, row_i)
    sign <- if (side == "left") 1 else -1 # left -> place_right_arc (bulges right)
    cc_x <- center$x + sign * r
    cc_y <- center$y - r
    a0 <- atan2(center$y - r - cc_y, center$x - cc_x) # angle to top point
    a1 <- atan2(center$y - cc_y, (center$x + sign * r) - cc_x) # angle to side point
    angles <- seq(a0, a1, length.out = arc_n_points)
    list(x = cc_x + r * cos(angles), y = cc_y + r * sin(angles))
  }
  add_hline <- function(p0, p1, from, to) {
    ord <<- ord + 1L
    hline_rows[[length(hline_rows) + 1L]] <<- data.frame(
      layer = "hline",
      x = p0$x,
      y = p0$y,
      xend = p1$x,
      yend = p1$y,
      from = from,
      to = to,
      order = ord,
      stringsAsFactors = FALSE
    )
    bridge_rows[[length(bridge_rows) + 1L]] <<- data.frame(
      layer = "bridge",
      x = p0$x,
      y = p0$y,
      xend = p1$x,
      yend = p1$y,
      from = from,
      to = to,
      order = ord,
      stringsAsFactors = FALSE
    )
  }

  arc_rows <- vector("list", nrow(input_cells))
  hline_rows <- list()
  bridge_rows <- list()
  by_row <- split(seq_len(nrow(input_cells)), input_cells$row)
  for (row_i in names(by_row)[order(as.integer(names(by_row)))]) {
    row_num <- as.integer(row_i)
    node_col <- node_col_by_row[[row_i]]
    idx <- by_row[[row_i]]
    for (side in c("left", "right")) {
      side_idx <- idx[input_cells$side[idx] == side]
      side_idx <- side_idx[order(input_cells$column[side_idx])]
      for (i in side_idx) {
        owner <- owner_of(input_cells$start_row[i], input_cells$column[i])
        pts <- arc_geom(input_cells$column[i], row_num, side)
        ord <- ord + 1L
        arc_rows[[i]] <- data.frame(
          id = i,
          x = pts$x,
          y = pts$y,
          from = owner$label,
          to = input_cells$label[i],
          order = ord,
          stringsAsFactors = FALSE
        )
      }
      if (length(side_idx) > 0L) {
        edge_i <- if (side == "left") {
          side_idx[1]
        } else {
          side_idx[length(side_idx)]
        }
        owner <- owner_of(
          input_cells$start_row[edge_i],
          input_cells$column[edge_i]
        )
        if (side == "left") {
          add_hline(
            coord(input_cells$column[edge_i], row_num, dx = 1),
            coord(node_col, row_num),
            owner$label,
            owner_of(row_num, node_col)$label
          )
        } else {
          add_hline(
            coord(node_col, row_num),
            coord(input_cells$column[edge_i], row_num, dx = -1),
            owner$label,
            owner_of(row_num, node_col)$label
          )
        }
      }
    }
  }

  # ---- leaders + labels -------------------------------------------------
  last_col_by_row <- if (nrow(cells) == 0L) {
    data.frame(row = integer(0), column = integer(0))
  } else {
    stats::aggregate(column ~ row, data = cells, FUN = max)
  }
  leader_rows <- vector("list", nrow(nodes_cells))
  label_rows <- vector("list", nrow(nodes_cells))
  for (i in seq_len(nrow(nodes_cells))) {
    row_i <- nodes_cells$row[i]
    node_col <- nodes_cells$column[i]
    last_col <- last_col_by_row$column[last_col_by_row$row == row_i]
    p0 <- coord(node_col, row_i, dx = 1)
    p1 <- coord(last_col + 1, row_i, dx = -0.4)
    ord <- ord + 1L
    leader_rows[[i]] <- data.frame(
      layer = "leader",
      x = p0$x,
      y = p0$y,
      xend = p1$x,
      yend = p1$y,
      from = nodes_cells$label[i],
      to = nodes_cells$label[i],
      order = ord,
      stringsAsFactors = FALSE
    )
    lp <- coord(last_col + 1, row_i)
    label_rows[[i]] <- data.frame(
      name = nodes_cells$label[i],
      label = nodes_cells$label[i],
      x = lp$x,
      y = lp$y,
      stringsAsFactors = FALSE
    )
  }

  segments <- rbind_or_empty(
    c(leader_rows, vline_rows, bridge_rows, hline_rows),
    data.frame(
      layer = character(0),
      x = numeric(0),
      y = numeric(0),
      xend = numeric(0),
      yend = numeric(0),
      from = character(0),
      to = character(0),
      order = integer(0)
    )
  )
  segments <- segments[order(segments$order), ]
  rownames(segments) <- NULL
  arcs <- rbind_or_empty(
    arc_rows,
    data.frame(
      id = integer(0),
      x = numeric(0),
      y = numeric(0),
      from = character(0),
      to = character(0),
      order = integer(0)
    )
  )
  rownames(arcs) <- NULL
  labels <- rbind_or_empty(
    label_rows,
    data.frame(
      name = character(0),
      label = character(0),
      x = numeric(0),
      y = numeric(0)
    )
  )
  rownames(labels) <- NULL

  # ---- view box -----------------------------------------------------------
  # Only points that go through coord() contribute; hand-computed arc
  # endpoints (the actual drawn path) do not - only each arc's own center.
  box_x <- c(nodes$x, segments$x, segments$xend, labels$x)
  box_y <- c(nodes$y, segments$y, segments$yend, labels$y)
  if (nrow(input_cells) > 0L) {
    centers <- coord(input_cells$column, input_cells$row)
    box_x <- c(box_x, centers$x)
    box_y <- c(box_y, centers$y)
  }
  viewbox <- if (length(box_x) == 0L) {
    # An empty dag has no points to box;
    # dagviz itself has no defined behavior here (its box starts at +-Inf
    # and nothing ever updates it). Fall back to an empty, finite box.
    list(
      left = 0,
      top = 0,
      bottom = 0,
      right = if (is.null(style$minimal_width)) 0 else style$minimal_width
    )
  } else {
    # With `minimal_width = NULL`, `right` only reaches the labels' anchor
    # points here; dag_layout() extends it to the end of the label text
    # (the display labels are not known yet).
    list(
      left = min(box_x) - scale,
      top = min(box_y) - scale,
      bottom = max(box_y) + scale,
      right = if (is.null(style$minimal_width)) {
        max(box_x) + scale
      } else {
        max(style$minimal_width, max(box_x) * 2)
      }
    )
  }
  viewbox$width <- viewbox$right - viewbox$left
  viewbox$height <- viewbox$bottom - viewbox$top

  # ---- edge_paths ---------------------------------------------------------
  # One full point-path per individual graph edge, for edge_color and
  # highlighting, as opposed to `segments`/`arcs` above, which
  # draw exactly what dagviz draws - a lane shared by all of a node's
  # out-edges gets one vline, not one per edge. Every incoming edge for a
  # row generates exactly one is_input touch for that row (`add_input()` is
  # called once per predecessor), so `own_col` + `input_cells` together
  # already enumerate the graph's edges exactly once each.
  path_rows <- vector("list", nrow(own_col) + nrow(input_cells))
  k <- 0L
  if (nrow(own_col) > 0L) {
    for (i in seq_len(nrow(own_col))) {
      row_i <- own_col$row[i]
      col_i <- own_col$column[i]
      owner <- owner_of(own_col$start_row[i], col_i)
      p0 <- coord(col_i, own_col$start_row[i])
      p1 <- coord(col_i, row_i)
      k <- k + 1L
      path_rows[[k]] <- data.frame(
        from = owner$label,
        to = own_col$label[i],
        piece = "vline",
        x = c(p0$x, p1$x),
        y = c(p0$y, p1$y),
        order = c(1L, 2L),
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(input_cells) > 0L) {
    for (i in seq_len(nrow(input_cells))) {
      row_i <- input_cells$row[i]
      col_i <- input_cells$column[i]
      node_col <- input_cells$node_column[i]
      owner <- owner_of(input_cells$start_row[i], col_i)
      vp0 <- coord(col_i, input_cells$start_row[i])
      vp1 <- coord(col_i, row_i, dy = -1)
      arc_pts <- arc_rows[[i]]
      if (input_cells$side[i] == "left") {
        hp0 <- coord(col_i, row_i, dx = 1)
        hp1 <- coord(node_col, row_i)
      } else {
        hp0 <- coord(col_i, row_i, dx = -1)
        hp1 <- coord(node_col, row_i)
      }
      k <- k + 1L
      path_rows[[k]] <- data.frame(
        from = owner$label,
        to = input_cells$label[i],
        piece = c(
          "vline",
          "vline",
          rep("arc", nrow(arc_pts)),
          "hline",
          "hline"
        ),
        x = c(vp0$x, vp1$x, arc_pts$x, hp0$x, hp1$x),
        y = c(vp0$y, vp1$y, arc_pts$y, hp0$y, hp1$y),
        order = seq_len(2L + nrow(arc_pts) + 2L),
        stringsAsFactors = FALSE
      )
    }
  }
  edge_paths <- rbind_or_empty(
    path_rows[seq_len(k)],
    data.frame(
      from = character(0),
      to = character(0),
      piece = character(0),
      x = numeric(0),
      y = numeric(0),
      order = integer(0)
    )
  )
  rownames(edge_paths) <- NULL

  list(
    nodes = nodes,
    segments = segments,
    arcs = arcs,
    labels = labels,
    edge_paths = edge_paths,
    viewbox = viewbox
  )
}

#' The metro style
#'
#' Reproduces dagviz's `StyleConfig` defaults (`style/metro.py`) as a
#' `dag_style_metro` object: round nodes (the default shape; see `node_shape`
#' of [dag_layers()]), thick colored edges with rounded bends, and labels to
#' the right.
#'
#' @param scale The size of a single grid cell.
#' @param node_radius The radius of a node, in pixels: of a circle, and the
#'   size that other shapes (see `node_shape` of [dag_layers()]) are drawn to
#'   match. A triangle or diamond reaches further from its center than a
#'   circle, up to about 1.6 times `node_radius`, and the plot ends only
#'   `scale` pixels from the nodes' centers, so with a `node_radius` of about 7
#'   or more (at the default `scale`) a triangle in the first or last row may be
#'   cut off: raise `scale` too.
#' @param node_fill Optional single fill color for every node, used only
#'   when `node_color` (a separate argument to the plotting functions) is
#'   left at its default. Unlike `node_color`, it never affects edge
#'   colors (matching dagviz: `node_fill` recolors `place_node()` only,
#'   never `_place_edge()`). `NULL` (the default) uses the resolved
#'   palette instead.
#' @param node_stroke,node_stroke_width The color and width of a node's
#'   border.
#' @param edge_stroke_width The line width of an edge.
#' @param label_font_family,label_font_size,label_font_color The font
#'   family, size (in pixels) and color of a node's label. Accept either
#'   dagviz's own `StyleConfig` values (the defaults: `"sans-serif"`,
#'   `"inherit"`, `"currentColor"`) or their R equivalents directly.
#' @param label_arrow_stroke,label_arrow_dash_array The color and linetype
#'   of the leader line from a node to its label. `label_arrow_dash_array`
#'   accepts dagviz's SVG `stroke-dasharray` value (the default, `"2"`) or
#'   an R `linetype` code (e.g. `"22"`) directly.
#' @param arc_radius The radius of an input arc.
#' @param bridge_color The color of the white "bridge" drawn under a
#'   horizontal line where it crosses other edges.
#' @param minimal_width How far the plot extends to the right of its
#'   labels. `NULL` (the default) ends the plot just past the longest
#'   label, measured from its text. A number is dagviz's rule instead: the
#'   plot is at least that wide in pixels (dagviz uses `500`) and twice as
#'   wide as the right-most label's start, since an SVG can't measure its
#'   own text. The plot is then often much wider than the graph.
#' @return A `dag_style_metro` object.
#' @export
#' @examples
#' style_metro()
#' dag <- as_dag(data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")))
#' ggplot_dag(dag, style = style_metro(node_radius = 9, arc_radius = 12, scale = 12))
style_metro <- function(
  scale = 10,
  node_radius = 6,
  node_fill = NULL,
  node_stroke = "white",
  node_stroke_width = 2,
  edge_stroke_width = 2,
  label_font_family = "sans-serif",
  label_font_size = "inherit",
  label_font_color = "currentColor",
  label_arrow_stroke = "lightgrey",
  label_arrow_dash_array = "2",
  arc_radius = 15,
  bridge_color = "white",
  minimal_width = NULL
) {
  # Map dagviz's SVG-specific StyleConfig values to R equivalents (Section
  # 5); an already-R-native value (e.g. a user passing "sans" directly)
  # passes through unchanged.
  if (identical(label_font_family, "sans-serif")) {
    label_font_family <- "sans"
  }
  if (identical(label_font_size, "inherit")) {
    label_font_size <- 16
  }
  if (identical(label_font_color, "currentColor")) {
    label_font_color <- "black"
  }
  if (identical(label_arrow_stroke, "lightgrey")) {
    label_arrow_stroke <- "lightgray"
  }
  if (identical(label_arrow_dash_array, "2")) {
    label_arrow_dash_array <- "22"
  }

  params <- list(
    scale = scale,
    node_radius = node_radius,
    node_fill = node_fill,
    node_stroke = node_stroke,
    node_stroke_width = node_stroke_width,
    edge_stroke_width = edge_stroke_width,
    label_font_family = label_font_family,
    label_font_size = label_font_size,
    label_font_color = label_font_color,
    label_arrow_stroke = label_arrow_stroke,
    label_arrow_dash_array = label_arrow_dash_array,
    arc_radius = arc_radius,
    bridge_color = bridge_color,
    minimal_width = minimal_width
  )
  structure(params, class = "dag_style_metro")
}

#' Lay out a DAG for drawing
#'
#' Turns an [as_dag()] `dag` into a `dag_layout`, the plain data that
#' [dag_layers()] and [ggplot_dag()] draw from. Coordinates are in
#' dagviz's SVG pixel space, with y growing downward ([dag_theme()]
#' reverses the y scale to match).
#'
#' @param dag A `dag`, as returned by [as_dag()] or [tar_dag()].
#' @param style A [style_metro()] object.
#' @return A `dag_layout`: a classed list of
#'   * `nodes`: one row per node, in row order - `name`, `label`, `row`,
#'     `col`, `x`, `y`, `color_index`, plus the `dag`'s extra node columns.
#'   * `segments`: straight lines, `layer` (`"leader"`, `"vline"`,
#'     `"bridge"` or `"hline"`), `x`, `y`, `xend`, `yend`, `from`, `to`
#'     and `order` (dagviz's draw order, shared with `arcs`).
#'   * `arcs`: one point path per arc (`id`), with `x`, `y`, `from`, `to`
#'     and `order`.
#'   * `labels`: `name`, `label`, `x`, `y`.
#'   * `edge_paths`: one full point path per graph edge - `from`, `to`,
#'     `piece` (`"vline"`, `"arc"` or `"hline"`), `x`, `y`, and `order`
#'     (point order along the path) - used for `edge_color` other than
#'     `"source"`, and for highlighting paths.
#'   * `viewbox`: a list of `left`, `top`, `bottom`, `right`, `width` and
#'     `height`.
#'   * `style`: the [style_metro()] object.
#' @export
#' @examples
#' dag <- as_dag(data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")))
#' lay <- dag_layout(dag, style_metro())
#' print(lay)
#' lay$nodes
dag_layout <- function(dag, style = style_metro()) {
  order <- metro_order(dag$nodes$name, dag$edges)
  lanes <- metro_lanes(dag$nodes$name, dag$edges, order)
  geom <- metro_geometry(lanes$cells, lanes$columns, style)

  label_of <- stats::setNames(dag$nodes$label, dag$nodes$name)
  geom$nodes$label <- unname(label_of[geom$nodes$name])
  geom$labels$label <- unname(label_of[geom$labels$name])

  extra_node_cols <- setdiff(names(dag$nodes), c("name", "label"))
  for (col in extra_node_cols) {
    geom$nodes[[col]] <- dag$nodes[[col]][match(
      geom$nodes$name,
      dag$nodes$name
    )]
  }

  if (is.null(style$minimal_width) && nrow(geom$labels) > 0L) {
    # Widen the view box to the end of the longest label. The text is
    # measured on a pdf device (Helvetica metrics), so pad it by 15% in case
    # the plot's own "sans" is wider (e.g. DejaVu Sans): a clipped label is
    # worse than a little extra white space.
    label_end <- geom$labels$x +
      1.15 *
        label_width_px(
          geom$labels$label,
          style$label_font_family,
          style$label_font_size
        )
    # Round the width up to whole pixels, so a saved image is exactly as wide
    # as the view box (ggsave_dag() truncates a fractional pixel).
    width <- ceiling(
      max(geom$viewbox$right, max(label_end) + style$scale) - geom$viewbox$left
    )
    geom$viewbox$right <- geom$viewbox$left + width
    geom$viewbox$width <- width
  }

  structure(
    list(
      nodes = geom$nodes,
      segments = geom$segments,
      arcs = geom$arcs,
      labels = geom$labels,
      edge_paths = geom$edge_paths,
      viewbox = geom$viewbox,
      style = style
    ),
    class = "dag_layout"
  )
}

#' Width of each label's text, in pixels
#'
#' Measured with grid in `family` at `size` pixels (96 dpi), so the result
#' is in layout data units at `zoom = 1`. grid measures on the open device,
#' or on a null pdf device when none is open (as [dag_size()] does), so it
#' is exact for Helvetica-like fonts and only approximate for others. If
#' the font can't be measured, assumes 0.6 em per character.
#'
#' @param labels A character vector.
#' @param family,size The label font family and size in pixels
#'   (`label_font_family`, `label_font_size` in [style_metro()]).
#' @return A numeric vector, one width per label.
#' @keywords internal
label_width_px <- function(labels, family, size) {
  tryCatch(
    suppressWarnings({
      if (grDevices::dev.cur() == 1) {
        grDevices::pdf(NULL)
        on.exit(grDevices::dev.off(), add = TRUE)
      }
      gp <- grid::gpar(fontfamily = family, fontsize = size * 0.75)
      vapply(
        labels,
        function(label) {
          grid::convertWidth(
            grid::grobWidth(grid::textGrob(label, gp = gp)),
            "in",
            valueOnly = TRUE
          ) *
            96
        },
        numeric(1),
        USE.NAMES = FALSE
      )
    }),
    error = function(e) 0.6 * size * nchar(labels)
  )
}

#' Combine hlines and arcs into one ordered set of paths
#'
#' dagviz draws hlines and arcs interleaved, in one SVG group, in `order`.
#' `geom_path()` draws groups in the order they appear in its data, so this
#' builds one data frame - one hline as a 2-point path, one arc as its
#' already-computed point path - sorted by that shared `order`.
#' @keywords internal
metro_hline_arc_paths <- function(hline, arcs, node_colors) {
  empty <- data.frame(
    x = numeric(0),
    y = numeric(0),
    group = character(0),
    order = integer(0),
    color_ = character(0)
  )
  parts <- list()
  if (nrow(hline) > 0) {
    for (i in seq_len(nrow(hline))) {
      parts[[length(parts) + 1]] <- data.frame(
        x = c(hline$x[i], hline$xend[i]),
        y = c(hline$y[i], hline$yend[i]),
        group = paste0("h", i),
        order = hline$order[i],
        color_ = unname(node_colors[hline$from[i]]),
        stringsAsFactors = FALSE
      )
    }
  }
  if (nrow(arcs) > 0) {
    for (id in unique(arcs$id)) {
      a <- arcs[arcs$id == id, ]
      parts[[length(parts) + 1]] <- data.frame(
        x = a$x,
        y = a$y,
        group = paste0("a", id),
        order = a$order[1],
        color_ = unname(node_colors[a$from[1]]),
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(parts) == 0) {
    return(empty)
  }
  combined <- do.call(rbind, parts)
  combined <- combined[order(combined$order), ]
  # GeomPath$draw_panel() re-sorts its data by `group` before drawing
  # (`data <- data[order(data$group), ]`), and ggplot2 assigns character
  # groups their integer id alphabetically - so leaving `group` as "a1",
  # "a10", "h2", ... would draw in alphabetical, not dagviz, order.
  # Re-encoding it as 1, 2, 3, ... in the *already correctly sorted* row
  # order makes that later sort a no-op.
  combined$group <- match(combined$group, unique(combined$group))
  combined
}

#' Draw a DAG layout as ggplot2 layers
#'
#' Returns a list of ggplot2 layers in draw order, ready to add to a
#' `ggplot()` call (or combined with [dag_theme()] by [ggplot_dag()]).
#'
#' @param layout A `dag_layout`, as returned by [dag_layout()].
#' @param node_color,palette,edge_color Color resolution arguments: see
#'   the pkgdown site's Colors article for the full set of rules and
#'   examples. `palette` must supply at least one color per lane (or per
#'   distinct value of a `node_color` column); colors are never recycled,
#'   and the default [metro_palette()] never repeats one.
#' @param node_shape The shape of a node: `NULL` (the default), a node
#'   column name, or a single shape name. A shape is `"circle"`, `"square"`,
#'   `"diamond"`, `"triangle"` or `"triangle_down"`. `NULL` follows the
#'   nodes' `type` column if they have one (as a pipeline from [tar_dag()]
#'   does), and otherwise draws circles; a `type` value that `shapes` doesn't
#'   name is a circle. A column name maps that column to shapes and warns
#'   about values `shapes` doesn't name; a shape name draws every node as
#'   that shape. A legend names the shapes whenever more than one is drawn;
#'   with `node_color` set to the same column, it is merged with the color
#'   legend. Remove it with `+ ggplot2::guides(shape = "none")`.
#' @param shapes Which shape each value of the `node_shape` column gets: a
#'   named character vector (value -> shape), or an unnamed one of shapes
#'   assigned to the distinct values in order, which must supply at least
#'   one per value. Defaults to [targets_node_shapes()].
#' @param zoom Multiplier for node size, line widths and text size, to match
#'   a panel sized at `zoom` px per data unit (see [ggplot_dag()]).
#' @return A list of ggplot2 layers.
#' @export
#' @examples
#' dag <- as_dag(data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")))
#' lay <- dag_layout(dag, style_metro())
#' ggplot2::ggplot() + dag_layers(lay) + dag_theme()
dag_layers <- function(
  layout,
  node_color = NULL,
  edge_color = "source",
  palette = metro_palette,
  node_shape = NULL,
  shapes = targets_node_shapes(),
  zoom = 1
) {
  check_zoom(zoom)
  if (is.null(palette)) {
    palette <- metro_palette
  }
  style <- layout$style
  # nc drives EDGE colors ("source"/"target" reference it) and, absent an
  # explicit node_color or style$node_fill, node fill too. node_fill
  # is dagviz's own shortcut: it recolors nodes only, never
  # edges (`place_node` uses `node_fill or self.colors[color]`, but
  # `_place_edge` always uses `self.colors[color]`) - so it must not flow
  # through node_color, which edge_color = "source"/"target" also reads.
  nc <- resolve_node_color(layout$nodes, node_color, palette)
  node_fill <- if (is.null(node_color) && !is.null(style$node_fill)) {
    list(
      colors = stats::setNames(
        rep(style$node_fill, nrow(layout$nodes)),
        layout$nodes$name
      ),
      legend = FALSE,
      title = NULL,
      values = NULL,
      map = NULL
    )
  } else {
    nc
  }
  ns <- resolve_node_shape(layout$nodes, node_shape, shapes)
  # Node sizes, line widths and text are in absolute units, unlike the layout
  # (data units), so `zoom` must scale them by hand to match a panel that
  # ggplot_dag() has sized to `zoom` px per data unit.
  px <- function(w) w * zoom / ggplot2::.pt

  layers <- list()
  seg <- layout$segments

  leader <- seg[seg$layer == "leader", ]
  layers[[length(layers) + 1]] <- ggplot2::geom_segment(
    data = leader,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      xend = .data$xend,
      yend = .data$yend
    ),
    color = style$label_arrow_stroke,
    linetype = style$label_arrow_dash_array,
    linewidth = px(1)
  )

  # dagviz's own draw order is leaders, vlines, bridges, hlines+arcs, nodes,
  # labels - the bridges must come *after* the vlines, or a vline painted
  # on top erases the white crossing-gap a bridge is there to create.
  if (identical(edge_color, "source")) {
    vline <- seg[seg$layer == "vline", ]
    if (nrow(vline) > 0) {
      vline$color_ <- unname(nc$colors[vline$from])
      layers[[length(layers) + 1]] <- ggplot2::geom_segment(
        data = vline,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          xend = .data$xend,
          yend = .data$yend,
          color = I(.data$color_)
        ),
        linewidth = px(style$edge_stroke_width)
      )
    }
  } else {
    ec <- resolve_edge_color(layout$edge_paths, edge_color, nc$colors)
    vp <- layout$edge_paths[layout$edge_paths$piece == "vline", ]
    if (nrow(vp) > 0) {
      key <- paste(vp$from, vp$to, sep = "|")
      vp <- vp[order(key, vp$order), ]
      starts <- vp[c(TRUE, FALSE), ]
      ends <- vp[c(FALSE, TRUE), ]
      vline_df <- data.frame(
        x = starts$x,
        y = starts$y,
        xend = ends$x,
        yend = ends$y,
        color_ = unname(ec[paste(starts$from, starts$to, sep = "|")]),
        stringsAsFactors = FALSE
      )
      layers[[length(layers) + 1]] <- ggplot2::geom_segment(
        data = vline_df,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          xend = .data$xend,
          yend = .data$yend,
          color = I(.data$color_)
        ),
        linewidth = px(style$edge_stroke_width)
      )
    }
  }

  bridge <- seg[seg$layer == "bridge", ]
  if (nrow(bridge) > 0) {
    layers[[length(layers) + 1]] <- ggplot2::geom_segment(
      data = bridge,
      ggplot2::aes(
        x = .data$x,
        y = .data$y,
        xend = .data$xend,
        yend = .data$yend
      ),
      color = style$bridge_color,
      linewidth = px(
        style$edge_stroke_width +
          2 * min(style$edge_stroke_width, style$node_stroke_width)
      )
    )
  }

  if (identical(edge_color, "source")) {
    combined <- metro_hline_arc_paths(
      seg[seg$layer == "hline", ],
      layout$arcs,
      nc$colors
    )
    if (nrow(combined) > 0) {
      layers[[length(layers) + 1]] <- ggplot2::geom_path(
        data = combined,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          group = .data$group,
          color = I(.data$color_)
        ),
        linewidth = px(style$edge_stroke_width)
      )
    }
  } else {
    ah <- layout$edge_paths[layout$edge_paths$piece %in% c("arc", "hline"), ]
    if (nrow(ah) > 0) {
      key <- paste(ah$from, ah$to, sep = "|")
      ah$color_ <- unname(ec[key])
      ah <- ah[order(key, ah$order), ]
      # see metro_hline_arc_paths(): re-encode as an integer in draw order,
      # since GeomPath re-sorts by `group` (alphabetically, for a
      # character group) before drawing.
      ah$group_ <- match(
        paste(ah$from, ah$to, sep = "|"),
        unique(paste(ah$from, ah$to, sep = "|"))
      )
      layers[[length(layers) + 1]] <- ggplot2::geom_path(
        data = ah,
        ggplot2::aes(
          x = .data$x,
          y = .data$y,
          group = .data$group_,
          color = I(.data$color_)
        ),
        linewidth = px(style$edge_stroke_width)
      )
    }
  }

  # One point per node: shapes 21 to 25 take a fill (the node color) and a
  # border (`node_stroke`). A point's `size` and `stroke` are absolute, like
  # line widths and text, so `zoom` scales them. A pch-21 circle's diameter
  # to the middle of its border is its font size, `size * .pt + lwd` px, and
  # `lwd = stroke * .stroke / 2`; so this gives a circle of `node_radius`
  # with a `node_stroke_width` border, as dagviz's SVG circle.
  nodes <- layout$nodes
  pts <- data.frame(
    x = nodes$x,
    y = nodes$y,
    fill_ = unname(node_fill$colors[nodes$name]),
    stringsAsFactors = FALSE
  )
  mapping <- ggplot2::aes(x = .data$x, y = .data$y)
  if (node_fill$legend) {
    # scale_fill_manual() is a discrete scale; a numeric node_color column
    # (e.g. targets' `seconds`/`bytes`) must still map to
    # distinct palette colors, not be treated as a
    # continuous fill.
    pts$value_ <- factor(
      unname(as.character(node_fill$values[nodes$name])),
      levels = names(node_fill$map)
    )
    mapping$fill <- ggplot2::aes(fill = .data$value_)$fill
  } else {
    mapping$fill <- ggplot2::aes(fill = I(.data$fill_))$fill
  }
  node_diameter <- max(2 * style$node_radius - style$node_stroke_width, 1)
  node_args <- list(
    data = pts,
    mapping = mapping,
    colour = style$node_stroke,
    size = zoom * node_diameter / ggplot2::.pt,
    stroke = zoom * 2 * style$node_stroke_width / ggplot2::.stroke
  )
  if (ns$legend) {
    pts$shape_ <- factor(
      as.character(ns$values[nodes$name]),
      levels = ns$breaks
    )
    node_args$data <- pts
    node_args$mapping$shape <- ggplot2::aes(shape = .data$shape_)$shape
  } else {
    # one shape (or no node): a constant, which a legend key (of the fill
    # legend) also takes
    node_args$shape <- if (nrow(nodes) > 0L) {
      node_shape_pch[[ns$shapes[[1L]]]]
    } else {
      node_shape_pch[["circle"]]
    }
  }
  layers[[length(layers) + 1]] <- do.call(ggplot2::geom_point, node_args)

  # A shape legend and a color legend of the same column are one legend: the
  # same title, breaks and labels make ggplot2 merge them. Otherwise they are
  # two, color first as in tar_visnetwork(); a key then needs the other's
  # aesthetic set, or it is a hollow shape (shape) or a white dot (fill).
  merged <- ns$legend && node_fill$legend && identical(node_fill$title, ns$title)
  two <- ns$legend && node_fill$legend && !merged
  if (node_fill$legend) {
    layers[[length(layers) + 1]] <- ggplot2::scale_fill_manual(
      values = node_fill$map,
      name = node_fill$title,
      breaks = if (merged) ns$breaks else names(node_fill$map),
      labels = if (merged) ns$labels else ggplot2::waiver(),
      guide = if (two) {
        ggplot2::guide_legend(
          order = 1L,
          override.aes = list(shape = node_shape_pch[["circle"]])
        )
      } else {
        "legend"
      }
    )
  }
  if (ns$legend) {
    layers[[length(layers) + 1]] <- ggplot2::scale_shape_manual(
      values = ns$map,
      name = ns$title,
      breaks = ns$breaks,
      labels = ns$labels,
      na.value = node_shape_pch[["circle"]],
      guide = if (merged) {
        "legend"
      } else {
        ggplot2::guide_legend(
          order = 2L,
          override.aes = list(fill = node_shape_key_fill)
        )
      }
    )
  }

  font_pt <- style$label_font_size * 0.75
  layers[[length(layers) + 1]] <- ggplot2::geom_text(
    data = layout$labels,
    ggplot2::aes(x = .data$x, y = .data$y, label = .data$label),
    hjust = 0,
    vjust = 0.5,
    family = style$label_font_family,
    color = style$label_font_color,
    size = px(font_pt)
  )

  # coord_fixed() lives here (not dag_theme(), which has no layout) so
  # that manual assembly - ggplot() + dag_layers(lay) + dag_theme() -
  # still gets exact viewbox limits, and so
  # ggsave_dag() can read them back from plot$coordinates$limits without a
  # separately stored field.
  vb <- layout$viewbox
  layers[[length(layers) + 1]] <- ggplot2::coord_fixed(
    xlim = c(vb$left, vb$right),
    ylim = c(vb$top, vb$bottom),
    expand = FALSE
  )

  layers
}

#' The theme and scales for a DAG plot
#'
#' `theme_void()` with zero plot margins and a white background, plus
#' `scale_y_reverse()`: layout coordinates have y growing downward, as in
#' dagviz's SVG. The theme is an ordinary ggplot2 theme, so any of its
#' elements can be overridden by adding `theme()` settings afterwards.
#'
#' A legend gets a small margin on its right (`legend.box.margin`, 5.5 pt),
#' so that it doesn't touch the edge of the image. A plot without a legend
#' has no such margin.
#'
#' @return A list of ggplot2 theme/scale components.
#' @export
#' @examples
#' dag <- as_dag(data.frame(from = c("a", "a", "b"), to = c("b", "c", "c")))
#' lay <- dag_layout(dag, style_metro())
#' ggplot2::ggplot() + dag_layers(lay) + dag_theme()
dag_theme <- function() {
  list(
    ggplot2::scale_y_reverse(),
    ggplot2::theme_void() +
      ggplot2::theme(
        plot.margin = ggplot2::margin(0, 0, 0, 0),
        # legend text would otherwise end flush with the right edge of the image
        legend.box.margin = ggplot2::margin(0, 5.5, 0, 0),
        plot.background = ggplot2::element_rect(fill = "white", color = NA),
        panel.background = ggplot2::element_rect(fill = "white", color = NA)
      )
  )
}
