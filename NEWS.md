# ggtargets 0.0.0.9000

Initial release.

## Plotting

- `metro_palette()` never repeats a color: the 18 distinct dagviz colors
  (dagviz's 21-color cycle repeats three of them) are followed by
  generated, perceptibly distinct colors. A too-short vector `palette`
  is now an error instead of being recycled.

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
- `Nodes are drawn by their `type`, as `targets::tar_visnetwork()` does
  (`targets_node_shapes()`): a circle for a regular target, a square for
  a dynamic-branching one, and an up and a down triangle for an imported
  function and object (with `tar_ggplot(targets_only = FALSE)`). A
  legend names the shapes whenever more than one is drawn, merged with
  the color legend if `node_color` is the same column. `node_shape`
  picks the column (or one shape for every node) and `shapes` maps its
  values to shapes (`"circle"`, `"square"`, `"diamond"`, `"triangle"`,
  `"triangle_down"`), so any DAG's nodes can be shaped by a column too.
  Nodes are now drawn as points rather than polygons, so with
  `fixed_size = FALSE` they keep their size, like lines and text, while
  the spacing stretches; legend keys are circles rather than squares.
- `A legend taller than the graph (a few nodes with a status and a shape
  legend) no longer gets cut off: `dag_size()` and `ggsave_dag()` make
  the plot as tall as the legend.
- `Plots have a fixed physical size (one data unit is `zoom` px at 96
  dpi), so nodes, lines and text look the same in every pipeline.
  `dag_size()` returns that size and `ggsave_dag()` saves at it,
  matching dagviz's own output.

## Styling and ggplot2

- `style_metro()` exposes every visual detail of the metro style:
  node/arc size, edge width, label font, bridge color, and more -
  accepting either dagviz's own `StyleConfig` values or their R
  equivalents.
- Plots end just past their longest label instead of leaving a wide
  blank area on the right: the label text is measured to size the plot.
  `style_metro(minimal_width = 500)` restores dagviz's wider canvas.
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
