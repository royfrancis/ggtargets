# Structural invariants of dag_layout(), checked on 200 random DAGs
# generated in R. These hold for any DAG regardless of shape (no shared
# row/column, arcs stay in the view box, every edge gets exactly one
# edge_paths entry, layouts are deterministic).

random_dag <- function(seed, n_max = 15) {
  withr::local_seed(seed)
  n <- sample(2:n_max, 1)
  p <- sample(c(0.1, 0.2, 0.3, 0.4), 1)
  names <- sprintf("n%02d", seq_len(n))
  names <- sample(names) # insertion order differs from index order
  from <- character(0)
  to <- character(0)
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i < j && runif(1) < p) {
        from <- c(from, sprintf("n%02d", i))
        to <- c(to, sprintf("n%02d", j))
      }
    }
  }
  as_dag(
    data.frame(from = from, to = to, stringsAsFactors = FALSE),
    nodes = data.frame(name = names, stringsAsFactors = FALSE)
  )
}

all_dags <- function() {
  random_dags <- lapply(seq_len(200), random_dag)
  names(random_dags) <- paste0("random_r_", seq_len(200))
  random_dags
}

.all_dags <- all_dags() # computed once for this file, not per test_that()

test_that("edge_paths covers every edge exactly once, from source center to target center", {
  skip_on_cran()
  round_key <- function(x, y) paste(round(x, 6), round(y, 6))
  pieces <- character(0)

  for (dag_name in names(.all_dags)) {
    d <- .all_dags[[dag_name]]
    lay <- dag_layout(d, style_metro())
    ep <- lay$edge_paths
    pieces <- c(pieces, ep$piece)
    got_pairs <- unique(paste(ep$from, ep$to))
    exp_pairs <- paste(d$edges$from, d$edges$to)
    expect_setequal(got_pairs, exp_pairs) # the actual edge set, not just its size
    expect_identical(length(got_pairs), nrow(d$edges), info = dag_name)

    node_pos <- stats::setNames(paste(lay$nodes$x, lay$nodes$y), lay$nodes$name)
    by_pair <- split(ep, paste(ep$from, ep$to))

    # index vline columns and hline rows by (x | y) once, instead of
    # rescanning lay$segments for every edge
    vline_seg <- lay$segments[lay$segments$layer == "vline", ]
    vline_by_x <- split(vline_seg, vline_seg$x)
    hline_seg <- lay$segments[lay$segments$layer == "hline", ]
    hline_by_y <- split(hline_seg, round(hline_seg$y, 6))

    # index arcs by their rounded (start, end) point key once
    arc_split <- split(lay$arcs, lay$arcs$id)
    arc_keys <- vapply(
      arc_split,
      function(a) {
        paste(round_key(a$x[1], a$y[1]), round_key(a$x[nrow(a)], a$y[nrow(a)]))
      },
      character(1)
    )

    for (key in got_pairs) {
      g <- by_pair[[key]]
      g <- g[order(g$order), ]
      expect_identical(
        paste(g$x[1], g$y[1]),
        node_pos[[g$from[1]]],
        info = paste(dag_name, key)
      )
      expect_identical(
        paste(g$x[nrow(g)], g$y[nrow(g)]),
        node_pos[[g$to[1]]],
        info = paste(dag_name, key)
      )

      # vline piece's y-range sits inside its column's drawn vline
      vp <- g[g$piece == "vline", ]
      if (nrow(vp) == 2) {
        seg_v <- vline_by_x[[as.character(vp$x[1])]]
        expect_true(
          !is.null(seg_v),
          info = paste(dag_name, key, "vline column exists")
        )
        if (!is.null(seg_v)) {
          y_lo <- min(vp$y)
          y_hi <- max(vp$y)
          covered <- any(
            pmin(seg_v$y, seg_v$yend) <= y_lo + 1e-6 &
              pmax(seg_v$y, seg_v$yend) >= y_hi - 1e-6
          )
          expect_true(covered, info = paste(dag_name, key, "vline covered"))
        }
      }

      # arc piece equals one arcs row exactly (by start/end point key)
      ap <- g[g$piece == "arc", ]
      if (nrow(ap) > 0) {
        want <- paste(
          round_key(ap$x[1], ap$y[1]),
          round_key(ap$x[nrow(ap)], ap$y[nrow(ap)])
        )
        match_id <- which(arc_keys == want)
        expect_true(
          length(match_id) > 0,
          info = paste(dag_name, key, "arc matches")
        )
        if (length(match_id) > 0) {
          cand <- arc_split[[match_id[1]]]
          expect_equal(
            nrow(cand),
            nrow(ap),
            info = paste(dag_name, key, "arc point count")
          )
        }
      }

      # hline piece's x-range sits inside its row's hline
      hp <- g[g$piece == "hline", ]
      if (nrow(hp) == 2) {
        seg_h <- hline_by_y[[as.character(round(hp$y[1], 6))]]
        expect_true(
          !is.null(seg_h),
          info = paste(dag_name, key, "hline row exists")
        )
        if (!is.null(seg_h)) {
          x_lo <- min(hp$x)
          x_hi <- max(hp$x)
          covered <- any(
            pmin(seg_h$x, seg_h$xend) <= x_lo + 1e-6 &
              pmax(seg_h$x, seg_h$xend) >= x_hi - 1e-6
          )
          expect_true(covered, info = paste(dag_name, key, "hline covered"))
        }
      }
    }
  }
  # the checks above run per piece, so the random DAGs must have all three
  expect_true(all(c("vline", "arc", "hline") %in% pieces))
})

test_that("no two nodes share a row, and every row has exactly one node", {
  for (dag_name in names(.all_dags)) {
    d <- .all_dags[[dag_name]]
    lay <- dag_layout(d, style_metro())
    expect_identical(
      nrow(lay$nodes),
      length(unique(lay$nodes$row)),
      info = dag_name
    )
    expect_identical(
      sort(lay$nodes$row),
      seq_len(nrow(lay$nodes)) - 1L,
      info = dag_name
    )
  }
})

test_that("at any row, no column is occupied by two lanes", {
  for (dag_name in names(.all_dags)) {
    d <- .all_dags[[dag_name]]
    order <- metro_order(d$nodes$name, d$edges)
    cells <- metro_lanes(d$nodes$name, d$edges, order)$cells
    dup <- duplicated(cells[c("row", "column")])
    expect_false(any(dup), info = dag_name)
  }
})

test_that("every arc lies within the view box", {
  for (dag_name in names(.all_dags)) {
    d <- .all_dags[[dag_name]]
    lay <- dag_layout(d, style_metro())
    if (nrow(lay$arcs) == 0) {
      next
    }
    vb <- lay$viewbox
    expect_true(
      all(lay$arcs$x >= vb$left - 1e-6 & lay$arcs$x <= vb$right + 1e-6),
      info = dag_name
    )
    expect_true(
      all(lay$arcs$y >= vb$top - 1e-6 & lay$arcs$y <= vb$bottom + 1e-6),
      info = dag_name
    )
  }
})

test_that("layouts are deterministic", {
  for (dag_name in names(.all_dags)[1:10]) {
    d <- .all_dags[[dag_name]]
    lay1 <- dag_layout(d, style_metro())
    lay2 <- dag_layout(d, style_metro())
    expect_identical(lay1$nodes, lay2$nodes, info = dag_name)
    expect_identical(lay1$segments, lay2$segments, info = dag_name)
    expect_identical(lay1$arcs, lay2$arcs, info = dag_name)
  }
})

test_that("edge_width draws every random DAG, with a bridge under each hline as wide as its edges", {
  skip_on_cran()
  n_bridges <- 0L
  for (dag_name in names(.all_dags)) {
    d <- .all_dags[[dag_name]]
    d$edges$w <- seq_len(nrow(d$edges))
    lay <- dag_layout(d)
    p <- ggplot_dag(lay, edge_width = "w")
    expect_silent(ggplot2::ggplot_build(p))

    style <- lay$style
    bridge <- lay$segments[lay$segments$layer == "bridge", ]
    n_bridges <- n_bridges + nrow(bridge)
    plain <- style$edge_stroke_width + 2 * min(style$edge_stroke_width, style$node_stroke_width)
    keys <- paste(lay$edges$from, lay$edges$to, sep = "|")
    # every edge the ordinary width: every bridge is the ordinary bridge
    same <- stats::setNames(rep(style$edge_stroke_width, length(keys)), keys)
    expect_equal(
      metro_bridge_widths(bridge, lay$edge_paths, same, style),
      rep(plain, nrow(bridge)),
      info = dag_name
    )
    # and no bridge is under nothing
    varied <- stats::setNames(as.numeric(d$edges$w), keys)
    expect_true(
      all(metro_bridge_widths(bridge, lay$edge_paths, varied, style) > 0),
      info = dag_name
    )
  }
  # the checks above are per bridge, so some random DAG must have one
  expect_gt(n_bridges, 0L)
})
