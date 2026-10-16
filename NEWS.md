# ggtargets 0.0.0.9001

## Collapse graph

- `dag_collapse(dag, by)` collapses a DAG by any node column: the nodes
  that share a value are merged into one, labeled with its member count,
  and the edges between groups are drawn once per pair, with an `n` that
  counts them. A pipeline of fifty targets in four stages becomes four
  nodes. `by` can be a column of your own (via `as_dag(nodes =)`) or one
  of the columns `tar_dag()` returns: `description`, the one free-form
  per-target field `targets` has, `type` or `status`. A grouping
  that makes a cycle is an error naming the groups and the edges behind
  it. `tar_ggplot(collapse_by =)` does it in one call. A pipeline's stages
  go in the `description` of its targets, for a block of them with
  `tar_option_set(description = )`: see "Collapsing a pipeline by stage"
  in `?tar_ggplot`, the get-started vignette and the "Collapsing a
  pipeline" article. A stage whose targets have different statuses has no
  status, and is drawn gray.
- `node_size` and `edge_width` (in `ggplot_dag()`, `dag_layers()` and
  `tar_ggplot()`) draw a numeric node or edge column as the node's radius
  and the edge's width, for any DAG: for a collapsed graph, `node_size = "n"` and
  `edge_width = "n"` show how many targets and dependencies each node and
  edge stands for. The ranges are `style_metro(node_size_range,
  edge_width_range)`, kept within the grid's spacing. Edges that share a line
  in the metro layout are drawn whole, the widest first, so the shared line
  is as wide as the widest edge on it, and each bridge is as wide as the
  widest edge it lies under. `dag_layout()` now also returns the dag's
  `edges`, with their extra columns.

## Fixes

- A node with no value in the `node_color` column (`NA`) is drawn gray,
  with no legend entry and no warning from a named palette. The edges
  drawn in its color (`edge_color = "source"` or `"target"`) used to be
  dropped: they had no color, and ggplot2 removed them without a word when
  it drew the plot, so `node_color = "branches"` lost the edges out of
  every stem. `NA` no longer takes a color from the palette either: with a
  palette function or an unnamed vector, the other values of a column that
  has `NA` move up a color, and a palette one color short of the values
  plus `NA` no longer errors.
- A node column can't be named `row`, `col`, `x`, `y` or `color_index`,
  the names of the layout's own columns, which it would silently overwrite
  (a column `x` replaced the nodes' coordinates): `as_dag()` and
  `dag_layout()` stop with an error naming the column.
- `dag_collapse()` keeps the `by` column's own value when `by` is
  `seconds`, `bytes` or `branches` (it summed it, like the others), and
  leaves a node whose `by` value is an empty string on its own, like one
  with an `NA`.
- `as_dag()` stops on an empty node name (it failed with "attempt to use
  zero-length variable name"), and keeps a factor `label` column as
  text (it turned it into integer codes). A node column value of `""`
  gets its palette color or shape like any other value.
- `node_color = NA_character_` and `edge_color = NA_character_` are an
  error, as they would give every node or edge a missing color.
  `dag_layout()` stops with a clear message when it is not given a `dag`.
- Summaries print "1 node", "1 edge" and "1 column" in the singular.

# ggtargets 0.0.0.9000

Initial release.

## Plotting

- `metro_palette()` never repeats a color: the 18 distinct dagviz colors
  (dagviz's 21-color cycle repeats three of them) are followed by
  generated, perceptibly distinct colors. A too-short vector `palette`
  is an error, not recycled.

- `tar_ggplot()` plots the current `targets` pipeline in one call, in
  dagviz's "metro" style: one row per target, thick colored edges with
  rounded bends, and a white "bridge" wherever two edges cross. It
  mirrors `targets::tar_visnetwork()`'s filtering arguments
  (`names`/`allow`/`exclude` as tidyselect, `targets_only`, `script`,
  `store`, ...).
- `as_dag()` builds the same kind of plot from any directed acyclic
  graph: an edge-list data frame, or a `targets::tar_network()`-shaped
  list. `tar_dag()` is a thin wrapper reading the current pipeline this
  way.
- `node_color`/`palette` color nodes by a node column, a single color,
  or dagviz's own default palette (`metro_palette()`);
  `targets_status_colors()` gives `targets`'s own status palette for
  `node_color = "status"`. `edge_color` colors edges by their source
  node (the default), their target node, or a single color.
- Nodes are drawn by their `type`, as `targets::tar_visnetwork()` does
  (`targets_node_shapes()`): a circle for a regular target, a square for
  a dynamic-branching one, and an up and a down triangle for an imported
  function and object (with `tar_ggplot(targets_only = FALSE)`). A
  legend names the shapes whenever more than one is drawn, merged with
  the color legend if `node_color` is the same column. `node_shape`
  picks the column (or one shape for every node) and `shapes` maps its
  values to shapes (`"circle"`, `"square"`, `"diamond"`, `"triangle"`,
  `"triangle_down"`), so any DAG's nodes can be shaped by a column too.
  Nodes are drawn as points, so with `fixed_size = FALSE` they keep
  their size, like lines and text, while the spacing stretches; legend
  keys are circles.
- A legend taller than the graph (a few nodes with a status and a shape
  legend) is not cut off: `dag_size()` and `ggsave_dag()` make the plot
  as tall as the legend.
- Plots have a fixed physical size (one data unit is `zoom` px at 96
  dpi), so nodes, lines and text look the same in every pipeline.
  `dag_size()` returns that size and `ggsave_dag()` saves at it, at
  dagviz's own scale (and, with `style_metro(minimal_width = 500)`, its
  canvas width).

## Styling and ggplot2

- `style_metro()` exposes every visual detail of the metro style:
  node/arc size, edge width, label font, bridge color, and more -
  accepting either dagviz's own `StyleConfig` values or their R
  equivalents.
- Plots end just past their longest label, rather than leaving a wide
  blank area on the right: the label text is measured to size the plot.
  `style_metro(minimal_width = 500)` gives dagviz's wider canvas.
- `ggplot_dag(lay, ...)` and manual assembly
  (`ggplot() + dag_layers(dag_layout(dag)) + dag_theme()`) both return
  plain `ggplot` objects that compose with the rest of ggplot2 -
  titles, themes, patchwork, and so on.

## Fidelity

- The metro layout - node order, column/lane assignment, and pixel
  geometry - matches dagviz 0.6.0's own SVG output exactly on all 67
  reference fixtures (drawn from real Snakemake rulegraphs via
  snakevision, hand-built edge cases, and random graphs), including
  draw order within dagviz's own SVG groups. The one deliberate
  difference is the width of the plot, which is fitted to the labels by
  default (see above); with `minimal_width = 500` the view box matches too.
