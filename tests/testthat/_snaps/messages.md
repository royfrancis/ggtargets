# as_dag() messages

    Code
      as_dag(data.frame(from = "a", to = "a"))
    Condition
      Error:
      ! as_dag(): self-loop(s) found: a

---

    Code
      as_dag(data.frame(from = c("a", "b", "c"), to = c("b", "c", "a")))
    Condition
      Error:
      ! as_dag(): the graph has a cycle: a -> b -> c -> a

---

    Code
      as_dag(matrix(1:4, 2, 2))
    Condition
      Error:
      ! as_dag() doesn't know how to convert an object of class matrix/array to a dag. Convert it to an edge-list data frame (columns `from` and `to`) first.

---

    Code
      as_dag(list(foo = 1))
    Condition
      Error:
      ! as_dag() doesn't know how to convert an object of class list to a dag. Convert it to an edge-list data frame (columns `from` and `to`) first.

---

    Code
      as_dag(data.frame(a = 1))
    Condition
      Error:
      ! as_dag(): `x` is missing column(s): from, to

---

    Code
      as_dag(data.frame(from = 1, to = 2), nodes = data.frame(x = 1))
    Condition
      Error:
      ! as_dag(): `nodes` is missing column: name

# as_dag() node-name validation messages

    Code
      as_dag(data.frame(from = c("a", NA), to = c("b", "c")))
    Condition
      Error:
      ! as_dag(): `x$from` has missing (NA) node name(s).

---

    Code
      as_dag(data.frame(from = c("a", "b"), to = c("b", NA)))
    Condition
      Error:
      ! as_dag(): `x$to` has missing (NA) node name(s).

---

    Code
      as_dag(data.frame(from = "a", to = "b"), nodes = data.frame(name = c("a", "b",
        NA)))
    Condition
      Error:
      ! as_dag(): `nodes$name` has missing (NA) node name(s).

---

    Code
      as_dag(data.frame(from = c("a|b", "a"), to = c("c", "b|c")))
    Condition
      Error:
      ! as_dag(): node names can't contain "|" or a carriage return; `x$from` has: "a|b"

---

    Code
      as_dag(data.frame(from = "a\rb", to = "c"))
    Condition
      Error:
      ! as_dag(): node names can't contain "|" or a carriage return; `x$from` has: "a\rb"

---

    Code
      as_dag(data.frame(from = c("a", ""), to = c("b", "c")))
    Condition
      Error:
      ! as_dag(): `x$from` has empty ("") node name(s).

# metro_palette() messages

    Code
      metro_palette(-1)
    Condition
      Error:
      ! `n` must be a single non-negative whole number.

---

    Code
      metro_palette(c(1, 2))
    Condition
      Error:
      ! `n` must be a single non-negative whole number.

---

    Code
      metro_palette(NA)
    Condition
      Error:
      ! `n` must be a single non-negative whole number.

---

    Code
      metro_palette(1.9)
    Condition
      Error:
      ! `n` must be a single non-negative whole number.

---

    Code
      metro_palette(19)
    Condition
      Error:
      ! Could not generate that many distinct colors.

# style_metro() unknown argument message

    Code
      style_metro(not_a_real_arg = 1)
    Condition
      Error in `style_metro()`:
      ! unused argument (not_a_real_arg = 1)

# resolve_node_color()/resolve_edge_color() messages

    Code
      resolve_node_color(nodes, "not_a_column_or_color", metro_palette)
    Condition
      Error:
      ! `node_color` ('not_a_column_or_color') is not a node column name or a valid color.

---

    Code
      resolve_node_color(rbind(nodes, data.frame(name = "b", row = 1, color_index = 1)),
      NULL, "red")
    Condition
      Error:
      ! `palette` has fewer colors than there are nodes.

---

    Code
      resolve_edge_color(edge_paths, "not_a_color", c(a = "red", b = "green"))
    Condition
      Error:
      ! `edge_color` ('not_a_color') is not "source", "target", or a valid color.

---

    Code
      resolve_value_palette(c("x", "y"), c(x = "red"))
    Condition
      Warning:
      Values with no match in `palette` set to gray: y
    Output
      $colors
      [1] "red"     "#94a4ac"
      
      $map
              x         y 
          "red" "#94a4ac" 
      

---

    Code
      resolve_value_palette(c("x", "y", "z"), c("red", "blue"))
    Condition
      Error:
      ! `palette` has fewer colors than there are distinct values.

---

    Code
      resolve_value_palette(c("x", "y", "z"), function(n) c("red", "blue"))
    Condition
      Error:
      ! `palette` returned fewer colors than there are distinct values.

---

    Code
      resolve_node_color(nodes, 123, metro_palette)
    Condition
      Error:
      ! `node_color` must be NULL, a column name, or a single color.

---

    Code
      resolve_node_color(nodes, NA_character_, metro_palette)
    Condition
      Error:
      ! `node_color` ('NA') is not a node column name or a valid color.

---

    Code
      resolve_edge_color(edge_paths, 123, c(a = "red", b = "green"))
    Condition
      Error:
      ! `edge_color` must be "source", "target", or a single color.

---

    Code
      resolve_edge_color(edge_paths, NA_character_, c(a = "red", b = "green"))
    Condition
      Error:
      ! `edge_color` ('NA') is not "source", "target", or a valid color.

# resolve_node_shape() messages

    Code
      resolve_node_shape(nodes, "hexagon", shapes)
    Condition
      Error:
      ! `node_shape` ('hexagon') is not a node column name or a valid shape (circle, square, diamond, triangle, triangle_down).

---

    Code
      resolve_node_shape(nodes, 1, shapes)
    Condition
      Error:
      ! `node_shape` must be NULL, a column name, or a single shape.

---

    Code
      resolve_node_shape(nodes, NULL, c(stem = "hexagon"))
    Condition
      Error:
      ! `shapes` has unknown shape(s): hexagon. Use circle, square, diamond, triangle, triangle_down.

---

    Code
      resolve_node_shape(nodes, NULL, 21)
    Condition
      Error:
      ! `shapes` must be a character vector of shape names.

---

    Code
      resolve_node_shape(nodes, "type", "circle")
    Condition
      Error:
      ! `shapes` has fewer shapes than there are distinct values.

---

    Code
      invisible(resolve_node_shape(nodes, "type", shapes))
    Condition
      Warning:
      Values with no match in `shapes` drawn as circles: other

---

    Code
      ggplot_dag(as_dag(data.frame(from = "a", to = "b")), node_shape = "hexagon")
    Condition
      Error:
      ! `node_shape` ('hexagon') is not a node column name or a valid shape (circle, square, diamond, triangle, triangle_down).

# ggsave_dag() no-viewbox message

    Code
      ggsave_dag(withr::local_tempfile(fileext = ".png"), p)
    Condition
      Error:
      ! `plot` has no coord_fixed() limits; build it with ggplot_dag() or dag_layers().

# ggsave_dag()'s scale= and dag_layers()'s zoom= messages

    Code
      ggsave_dag(withr::local_tempfile(fileext = ".png"), p, scale = -1)
    Condition
      Error:
      ! `scale` must be a single positive number.

---

    Code
      dag_layers(dag_layout(d, style_metro()), zoom = -1)
    Condition
      Error:
      ! `zoom` must be a single positive number.

---

    Code
      invisible(ggplot_dag(d, fixed_size = FALSE, zoom = 2))
    Condition
      Warning:
      `zoom` is ignored when `fixed_size = FALSE`.

# dag_layout() input message

    Code
      dag_layout(edges)
    Condition
      Error:
      ! dag_layout(): `dag` must be a dag, from as_dag() or tar_dag().

---

    Code
      ggplot_dag(edges)
    Condition
      Error:
      ! dag_layout(): `dag` must be a dag, from as_dag() or tar_dag().

# tar_dag() message when targets is not installed

    Code
      tar_dag()
    Condition
      Error:
      ! This needs the targets package. Install it with install.packages("targets").

# dag_collapse() messages

    Code
      dag_collapse(data.frame(from = "a", to = "b"), "g")
    Condition
      Error:
      ! dag_collapse(): `dag` must be a dag, from as_dag() or tar_dag().

---

    Code
      dag_collapse(d, 1)
    Condition
      Error:
      ! dag_collapse(): `by` must be a single node column name.

---

    Code
      dag_collapse(d, c("stage", "type"))
    Condition
      Error:
      ! dag_collapse(): `by` must be a single node column name.

---

    Code
      dag_collapse(d, "nope")
    Condition
      Error:
      ! dag_collapse(): `by` ('nope') is not a node column; the columns are name, label, stage, type, seconds.

---

    Code
      dag_collapse(d, "stage", label_n = NA)
    Condition
      Error:
      ! dag_collapse(): `label_n` must be TRUE or FALSE.

---

    Code
      dag_collapse(loop, "group")
    Condition
      Error:
      ! dag_collapse(): collapsing by `group` creates a cycle: X -> Y -> X
      Through these edges:
        X -> Y: a -> b (and 1 more)
        Y -> X: b -> c

---

    Code
      dag_collapse(clash, "group")
    Condition
      Error:
      ! dag_collapse(): `group` has value(s) that are also the name of a node with no `group`: b. They would be merged; rename one.

---

    Code
      dag_collapse(bad, "group")
    Condition
      Error:
      ! dag_collapse(): the values of `group` become node names, which can't contain "|" or a carriage return: "x|y", "z\rw"

# node_size and edge_width messages

    Code
      ggplot_dag(d, node_size = "nope")
    Condition
      Error:
      ! `node_size` ('nope') is not a node column.

---

    Code
      ggplot_dag(d, node_size = "stage")
    Condition
      Error:
      ! `node_size` ('stage') must be a numeric column.

---

    Code
      ggplot_dag(d, node_size = 1)
    Condition
      Error:
      ! `node_size` must be NULL or the name of a numeric node column.

---

    Code
      ggplot_dag(d, node_size = c("n", "n"))
    Condition
      Error:
      ! `node_size` must be NULL or the name of a numeric node column.

---

    Code
      ggplot_dag(d, edge_width = "nope")
    Condition
      Error:
      ! `edge_width` ('nope') is not an edge column.

---

    Code
      ggplot_dag(d, edge_width = TRUE)
    Condition
      Error:
      ! `edge_width` must be NULL or the name of a numeric edge column.

---

    Code
      ggplot_dag(d, edge_width = "kind")
    Condition
      Error:
      ! `edge_width` ('kind') must be a numeric column.

---

    Code
      ggplot_dag(d, edge_width = "stage")
    Condition
      Error:
      ! `edge_width` ('stage') is not an edge column.

# style_metro()'s node_size_range and edge_width_range messages

    Code
      style_metro(node_size_range = 5)
    Condition
      Error:
      ! `node_size_range` must be two positive numbers, smallest first, the largest at most `scale` (10).

---

    Code
      style_metro(node_size_range = c(8, 4))
    Condition
      Error:
      ! `node_size_range` must be two positive numbers, smallest first, the largest at most `scale` (10).

---

    Code
      style_metro(node_size_range = c(0, 4))
    Condition
      Error:
      ! `node_size_range` must be two positive numbers, smallest first, the largest at most `scale` (10).

---

    Code
      style_metro(node_size_range = c(4, 11))
    Condition
      Error:
      ! `node_size_range` must be two positive numbers, smallest first, the largest at most `scale` (10).

---

    Code
      style_metro(node_size_range = c("a", "b"))
    Condition
      Error:
      ! `node_size_range` must be two positive numbers, smallest first, the largest at most `scale` (10).

---

    Code
      style_metro(node_size_range = c(4, NA))
    Condition
      Error:
      ! `node_size_range` must be two positive numbers, smallest first, the largest at most `scale` (10).

---

    Code
      style_metro(edge_width_range = c(1, 6))
    Condition
      Error:
      ! `edge_width_range` must be two positive numbers, smallest first, the largest at most `scale / 2` (5).

---

    Code
      style_metro(scale = 20, edge_width_range = c(1, 11))
    Condition
      Error:
      ! `edge_width_range` must be two positive numbers, smallest first, the largest at most `scale / 2` (10).

# reserved node column messages

    Code
      as_dag(edges, nodes = data.frame(name = c("a", "b"), x = 1:2))
    Condition
      Error:
      ! as_dag(): `nodes` has column(s) named like the layout's own: x. These names are reserved (row, col, x, y, color_index); rename the column(s).

---

    Code
      as_dag(edges, nodes = data.frame(name = c("a", "b"), y = 1:2, kind = "k",
      color_index = 1:2))
    Condition
      Error:
      ! as_dag(): `nodes` has column(s) named like the layout's own: y, color_index. These names are reserved (row, col, x, y, color_index); rename the column(s).

---

    Code
      dag_layout(d)
    Condition
      Error:
      ! dag_layout(): `dag$nodes` has column(s) named like the layout's own: row. These names are reserved (row, col, x, y, color_index); rename the column(s).

