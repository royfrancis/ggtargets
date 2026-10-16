# Coverage for the as_dag() methods: construction, deduplication and
# node-name validation, for the data frame method and for the
# tar_network()-shaped list method (hand-built lists, so these need no
# targets and run on CRAN; test-targets.R runs the list method end to end
# through tar_dag(), but only off CRAN).
#
# The other as_dag() guarantees are tested elsewhere, not missing: the
# self-loop and cycle errors are snapshotted in test-messages.R, and
# find_cycle() itself below.

test_that("as_dag() drops duplicate edges, keeping the first", {
  edges <- data.frame(
    from = c("a", "a", "b"),
    to = c("b", "b", "c"),
    weight = c(1, 2, 3)
  )
  d <- as_dag(edges)
  expect_equal(nrow(d$edges), 2)
  expect_equal(d$edges$weight, c(1, 3))
})

test_that("as_dag() keeps extra node and edge columns", {
  edges <- data.frame(
    from = "a",
    to = "b",
    kind = "solid",
    stringsAsFactors = FALSE
  )
  nodes <- data.frame(
    name = c("a", "b"),
    type = c("x", "y"),
    stringsAsFactors = FALSE
  )
  d <- as_dag(edges, nodes = nodes)
  expect_identical(d$nodes$type, c("x", "y"))
  expect_identical(d$edges$kind, "solid")
})

test_that("as_dag() output has class 'dag'", {
  d <- as_dag(data.frame(from = "a", to = "b"))
  expect_s3_class(d, "dag")
})

test_that("as_dag() converts factor from/to and factor nodes$name to character", {
  edges <- data.frame(from = factor(c("a", "b")), to = factor(c("b", "c")))
  nodes <- data.frame(name = factor(c("a", "b", "c")), stringsAsFactors = FALSE)
  d <- as_dag(edges, nodes = nodes)
  expect_type(d$edges$from, "character")
  expect_type(d$edges$to, "character")
  expect_type(d$nodes$name, "character")
})

test_that("as_dag() keeps a factor `label` column as text", {
  # ifelse() on a factor gives its integer codes, not its levels
  nodes <- data.frame(name = c("a", "b"), label = factor(c("Alpha", NA)))
  d <- as_dag(data.frame(from = "a", to = "b"), nodes = nodes)
  expect_identical(d$nodes$label, c("Alpha", "b"))
})

test_that("as_dag() gives the default-method error on a character path, a matrix, and an igraph object", {
  expect_error(as_dag("x.dot"), "doesn't know how to convert")
  expect_error(as_dag(matrix(1:4, 2, 2)), "doesn't know how to convert")

  skip_if_not_installed("igraph")
  g <- igraph::graph_from_edgelist(matrix(c("a", "b"), ncol = 2))
  expect_error(as_dag(g), "doesn't know how to convert")
})

test_that("as_dag() handles an empty graph (0 nodes, 0 edges)", {
  d <- as_dag(data.frame(from = character(0), to = character(0)))
  expect_equal(nrow(d$nodes), 0)
  expect_equal(nrow(d$edges), 0)
})

test_that("find_cycle() doesn't overflow the call stack on a long chain (regression)", {
  # tar_dag() runs this on every call; a real pipeline can have thousands
  # of targets in a dependency chain. This used to overflow R's call
  # stack via one recursive call per node.
  edges <- data.frame(
    from = paste0("n", 1:2999),
    to = paste0("n", 2:3000),
    stringsAsFactors = FALSE
  )
  expect_no_error(d <- as_dag(edges))
  expect_equal(nrow(d$nodes), 3000)
})

test_that("find_cycle() names a cycle from a node back to itself, or returns NULL", {
  # a -> b -> a is a cycle; a -> c is not part of it
  edges <- data.frame(
    from = c("a", "b", "a"),
    to = c("b", "a", "c"),
    stringsAsFactors = FALSE
  )
  expect_identical(find_cycle(c("a", "b", "c"), edges), c("a", "b", "a"))
  expect_null(find_cycle(c("a", "b", "c"), edges[c(1, 3), ]))
})

# Node names are join keys: edge attributes and colors are matched on
# paste(from, to, sep = "|") downstream, so as_dag() is the one place that
# guarantees a name can serve as an identity. Each case below silently
# produced wrong output before that check existed.

test_that("as_dag() rejects NA node names, from nodes= and from either endpoint", {
  edges <- data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
  expect_error(
    as_dag(
      edges,
      nodes = data.frame(name = c("a", "b", NA), stringsAsFactors = FALSE)
    ),
    "`nodes$name` has missing",
    fixed = TRUE
  )
  expect_error(
    as_dag(data.frame(from = c("a", NA), to = c("b", "c"))),
    "`x$from` has missing",
    fixed = TRUE
  )
  expect_error(
    as_dag(data.frame(from = c("a", "b"), to = c("b", NA))),
    "`x$to` has missing",
    fixed = TRUE
  )
})

test_that("as_dag() rejects empty node names, from nodes= and from either endpoint", {
  # an empty name can't be looked up by name (find_cycle() does), so it used
  # to fail with "attempt to use zero-length variable name"
  edges <- data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
  expect_error(
    as_dag(
      edges,
      nodes = data.frame(name = c("a", "b", ""), stringsAsFactors = FALSE)
    ),
    "`nodes$name` has empty",
    fixed = TRUE
  )
  expect_error(
    as_dag(data.frame(from = c("a", ""), to = c("b", "c"))),
    "`x$from` has empty",
    fixed = TRUE
  )
  expect_error(
    as_dag(data.frame(from = c("a", "b"), to = c("b", ""))),
    "`x$to` has empty",
    fixed = TRUE
  )
})

test_that("as_dag() rejects node names containing the '|' join delimiter", {
  # a|b -> c and a -> b|c both key as "a|b|c": the second edge used to take
  # the first's attributes and color, and share its draw group
  expect_error(
    as_dag(data.frame(
      from = c("a|b", "a"),
      to = c("c", "b|c"),
      stringsAsFactors = FALSE
    )),
    "can't contain"
  )
})

test_that("as_dag() rejects node names containing a carriage return", {
  # the dedup key used to be paste(from, to, sep = "\r"), so a\rb -> c and
  # a -> b\rc both keyed as "a\rb\rc" and one of the two edges was silently
  # dropped. Two independent fixes close that: the key is gone (dedup is
  # duplicated() on the columns now), and these names never get that far.
  expect_error(
    as_dag(data.frame(from = "a\rb", to = "c", stringsAsFactors = FALSE)),
    "can't contain"
  )
  expect_error(
    as_dag(data.frame(
      from = c("a\rb", "a"),
      to = c("c", "b\rc"),
      stringsAsFactors = FALSE
    )),
    "can't contain"
  )
})

test_that("as_dag() leaves NA in node attributes alone - names only", {
  d <- as_dag(
    data.frame(from = "a", to = "b", stringsAsFactors = FALSE),
    nodes = data.frame(
      name = c("a", "b"),
      status = c("ok", NA),
      stringsAsFactors = FALSE
    )
  )
  expect_identical(d$nodes$status, c("ok", NA))
})

test_that("as_dag() falls back to the node name for an NA label, and keeps the first of a duplicated node", {
  edges <- data.frame(from = c("a", "b"), to = c("b", "c"))
  nodes <- data.frame(
    name = c("c", "a", "b", "a"),
    label = c("See", NA, "Bee", "Ay"),
    kind = c("k3", "k1", "k2", "k4"),
    stringsAsFactors = FALSE
  )
  d <- as_dag(edges, nodes = nodes)
  expect_identical(d$nodes$name, c("c", "a", "b"))
  expect_identical(d$nodes$label, c("See", "a", "Bee"))
  expect_identical(d$nodes$kind, c("k3", "k1", "k2"))
})

# as_dag.list(): the tar_network() path. The lists are built by hand
# (fake_network() in helper-network.R), so none of this needs targets.

test_that("as_dag() on a tar_network()-shaped list keeps only the plottable vertex columns", {
  d <- as_dag(fake_network())
  expect_s3_class(d, "dag")
  # the six kept columns come in a fixed order (not the vertices' own), with
  # `path` dropped and `label` filled in from `name`
  expect_identical(
    names(d$nodes),
    c(
      "name",
      "label",
      "type",
      "status",
      "description",
      "seconds",
      "bytes",
      "branches"
    )
  )
  expect_identical(d$nodes$name, c("raw", "clean", "model"))
  expect_identical(d$nodes$label, d$nodes$name)
  expect_identical(d$nodes$type, c("stem", "pattern", "stem"))
  expect_identical(d$nodes$status, c("uptodate", "outdated", "errored"))
  expect_identical(d$nodes$description, c(NA, NA, "fit"))
  expect_identical(d$nodes$seconds, c(0.5, NA, 12))
  expect_identical(d$nodes$bytes, c(120, NA, 4096))
  expect_identical(d$nodes$branches, c(NA, 3L, NA)) # NA for stems survives
  expect_identical(d$edges$from, c("raw", "clean"))
  expect_identical(d$edges$to, c("clean", "model"))
})

test_that("as_dag() on a list ignores the order of the vertex columns and drops extra edge columns", {
  net <- fake_network()
  net$vertices <- net$vertices[rev(names(net$vertices))]
  net$edges$weight <- c(1, 2)
  d <- as_dag(net)
  expect_identical(
    names(d$nodes),
    c(
      "name",
      "label",
      "type",
      "status",
      "description",
      "seconds",
      "bytes",
      "branches"
    )
  )
  expect_identical(d$nodes$name, c("raw", "clean", "model"))
  expect_identical(d$nodes$bytes, c(120, NA, 4096))
  expect_identical(names(d$edges), c("from", "to"))
})

test_that("as_dag() on a list keeps vertex order and isolated vertices, and appends endpoints missing from the vertices", {
  net <- list(
    vertices = data.frame(
      name = c("lonely", "b", "a"),
      status = c("x", "y", "z"),
      stringsAsFactors = FALSE
    ),
    edges = data.frame(
      from = c("a", "a"),
      to = c("b", "ghost"),
      stringsAsFactors = FALSE
    )
  )
  d <- as_dag(net)
  expect_identical(d$nodes$name, c("lonely", "b", "a", "ghost"))
  expect_identical(d$nodes$status, c("x", "y", "z", NA))
  expect_equal(nrow(d$edges), 2)
})

test_that("as_dag() on a list works with only a name column, and with no edges", {
  vertices <- data.frame(name = c("a", "b"), stringsAsFactors = FALSE)
  d <- as_dag(list(
    vertices = vertices,
    edges = data.frame(from = "a", to = "b", stringsAsFactors = FALSE)
  ))
  expect_identical(names(d$nodes), c("name", "label"))

  # a pipeline of independent targets: tar_network() gives zero edge rows...
  d_none <- as_dag(list(
    vertices = vertices,
    edges = data.frame(from = character(0), to = character(0))
  ))
  expect_identical(d_none$nodes$name, c("a", "b"))
  expect_equal(nrow(d_none$edges), 0)

  # ...and an edge table with no columns at all means the same
  d_bare <- as_dag(list(vertices = vertices, edges = data.frame()))
  expect_identical(d_bare$nodes$name, c("a", "b"))
  expect_equal(nrow(d_bare$edges), 0)
})

test_that("as_dag() on a list converts factor names and endpoints to character", {
  d <- as_dag(list(
    vertices = data.frame(name = factor(c("a", "b", "c"))),
    edges = data.frame(from = factor(c("a", "b")), to = factor(c("b", "c")))
  ))
  expect_type(d$nodes$name, "character")
  expect_type(d$edges$from, "character")
  expect_type(d$edges$to, "character")
})

test_that("as_dag() on a list that isn't tar_network()-shaped gives the default-method error", {
  vertices <- data.frame(name = "a")
  edges <- data.frame(from = "a", to = "b")
  expect_error(
    as_dag(list(vertices = vertices)),
    "doesn't know how to convert an object of class list",
    fixed = TRUE
  )
  expect_error(
    as_dag(list(edges = edges)),
    "doesn't know how to convert an object of class list",
    fixed = TRUE
  )
  expect_error(as_dag(list()), "class list", fixed = TRUE)
})

test_that("as_dag() on a list still rejects cycles, NA names and '|' names", {
  vertices <- data.frame(name = c("a", "b"), stringsAsFactors = FALSE)
  expect_error(
    as_dag(list(
      vertices = vertices,
      edges = data.frame(from = c("a", "b"), to = c("b", "a"))
    )),
    "cycle: a -> b -> a",
    fixed = TRUE
  )
  expect_error(
    as_dag(list(
      vertices = data.frame(name = c("a", NA), stringsAsFactors = FALSE),
      edges = data.frame(from = "a", to = "b")
    )),
    "`nodes$name` has missing",
    fixed = TRUE
  )
  expect_error(
    as_dag(list(
      vertices = data.frame(name = c("a|b", "c"), stringsAsFactors = FALSE),
      edges = data.frame(from = "a|b", to = "c")
    )),
    "can't contain"
  )
})

# find_cycle() is an iterative depth-first search with three node states;
# these pin the paths a plain "seen it" check would get wrong.

test_that("find_cycle() returns NULL when a node is reached twice but there is no cycle (a diamond)", {
  edges <- data.frame(
    from = c("a", "a", "b", "c"),
    to = c("b", "c", "d", "d"),
    stringsAsFactors = FALSE
  )
  expect_null(find_cycle(c("a", "b", "c", "d"), edges))
})

test_that("find_cycle() names just the cycle, past an acyclic component and a lead-in", {
  edges <- data.frame(
    from = c("p", "x", "a", "b"),
    to = c("q", "a", "b", "a"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    find_cycle(c("p", "q", "x", "a", "b"), edges),
    c("a", "b", "a")
  )
})

test_that("as_dag() rejects node columns named like the layout's own", {
  edges <- data.frame(from = "a", to = "b")
  for (col in reserved_node_columns) {
    nodes <- data.frame(name = c("a", "b"), junk = 1:2)
    names(nodes)[2] <- col
    expect_error(as_dag(edges, nodes = nodes), "reserved", info = col)
  }
  # near misses, and the dag's own keys, are fine
  nodes <- data.frame(
    name = c("a", "b"),
    label = c("A", "B"),
    X = 1,
    xx = 2,
    Row = 3,
    colour = 4,
    n = 5
  )
  expect_no_error(as_dag(edges, nodes = nodes))
  # a dag with no nodes table has nothing to clash
  expect_no_error(as_dag(edges))
})

test_that("the reserved node columns are exactly the layout's own besides name and label", {
  # if a column is added to the layout, a dag column of that name would
  # overwrite it: add it to reserved_node_columns
  lay <- dag_layout(as_dag(data.frame(from = "a", to = "b")))
  expect_setequal(
    reserved_node_columns,
    setdiff(names(lay$nodes), c("name", "label"))
  )
})

test_that("a dag edited after as_dag(), or built by hand, is checked when it is laid out", {
  d <- fake_dag()
  d$nodes$x <- 1:3
  expect_error(dag_layout(d), "reserved")
  expect_error(ggplot_dag(d), "reserved")
  testthat::local_mocked_bindings(tar_dag = function(...) d)
  expect_error(tar_ggplot(), "reserved")

  by_hand <- structure(
    list(
      nodes = data.frame(name = "a", label = "a", y = 1),
      edges = data.frame(from = character(0), to = character(0))
    ),
    class = "dag"
  )
  expect_error(dag_layout(by_hand), "reserved")
})

test_that("what tar_dag() returns never clashes with the layout's columns", {
  expect_no_error(dag_layout(as_dag(fake_network())))
})
