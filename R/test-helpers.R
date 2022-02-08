# generate a random acyclic graph
make_random_acyclic_graph <- function(n, labels = FALSE) {
  # the generated graph
  graph <- vector("list", n)

  # randomly permutated sequence of items
  items <- sample.int(n)

  # assign all pending items as dependencies until none are left
  while(length(items) > 0) {
    # first item can be a dependency of any other item
    item <- items[[1]]
    items <- items[-1]

    # pick x items at random
    ii <- make_random_selection(items)
    graph[[item]] <- make_random_selection(items)
  }

  # add labels if required
  if(is_true(labels)) {
    labels <- paste0("label", seq_along(graph))
    graph <- lapply(graph, function(i) labels[i])
    names(graph) <- labels
  }

  graph
}

make_random_selection <- function(x) {
  n <- length(x)
  size <- sample.int(n + 1L, 1L) - 1L
  if(size == 0L) integer() else x[sample.int(n, size)]
}

# check that topological sort actually produces correct result
expect_topo_sorted <- function(x, precedence_graph) {
  if(length(x) != length(precedence_graph)) {
    msg <- fmt("sorted output length is {length(x)}, graph length is {length(precedence_graph)}")
    testthat::fail(msg)
  }

  # compute number of dependencies
  items <- c(precedence_graph, recursive = TRUE)
  if(length(items) == 0) items <- integer()
  n_dep <- tabulate(items, length(precedence_graph))

  for (item in x) {
    # item cannot have dependencies
    if (n_dep[item] != 0) {
      # write out an error message
      msg <- fmt("'{item}' is not correctly sorted")
      testthat::fail(msg)
    }

    # decrease the dependency count for any dependent item
    ii <- precedence_graph[[item]]
    n_dep[ii] <- n_dep[ii] - 1L
  }

  testthat::succeed("graph is topologically sorted")
}


# takes a dependency graph in list encoding (i follows all x[[i]])
# and generates all supported graph encodings from it
#
# used to test that graph encodings are processed correctly
build_graph_encodings <- function(graph) {
  # validate the graph
  i <- c(graph, recursive = TRUE)
  is.integer(i) && all(i > 0) && all (i <= length(graph)) || abort("invalid graph")

  # build the precedence graph
  precedence_graph <- vector("list", length(graph))
  for(i in seq_along(graph)) {
    for(j in graph[[i]]) precedence_graph[[j]] <- c(precedence_graph[[j]], i)
  }

  # build the matrices
  precedence_matrix <- matrix(0L, ncol = length(graph), nrow = length(graph))
  dependency_matrix <- matrix(0L, ncol = length(graph), nrow = length(graph))
  for(i in seq_along(graph)) {
    for(j in graph[[i]]) {
      precedence_matrix[j, i] <- 1L
      dependency_matrix[i, j] <- 1L
    }
  }

  list(
    list_precedence   = precedence_graph,
    list_dependency   = graph,
    matrix_precedence = precedence_matrix,
    matrix_dependency = dependency_matrix
  )
}
