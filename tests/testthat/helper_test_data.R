# fix empty components in a graph
fixup_graph <- function(g) {
  g[list_sizes(g) == 0L] <- list(integer())
  g
}

make_labeled_graph <- function(g) {
  if(is.list(g)) {
    g <- lapply(g, as.character)
    names(g) <- as.character(seq_along(g))
  } else
  if(is.matrix((g))) {
    rownames(g) <- as.character(seq_len(nrow(g)))
    colnames(g) <- as.character(seq_len(ncol(g)))
  } else {
    abort("unknown graph type")
  }

  g
}


test_data <- list(
  # test case 1 (simple graph)
  list(
    list_precedence   = list(c(2L, 3L), 3L, integer()),
    list_dependency   = list(integer(), 1L, c(1L, 2L)),
    matrix_precedence = rbind(c(0L, 1L, 1L), c(0L, 0L, 1L), c(0L, 0L, 0L)),
    matrix_dependency = rbind(c(0L, 0L, 0L), c(1L, 0L, 0L), c(1L, 1L, 0L)),
    n_dependencies    = c(0L, 1L, 2L),
    stable_sort       = c(1L, 2L, 3L)
  ),
  # test case 2 (empty graph)
  list(
    list_precedence   = list(),
    list_dependency   = list(),
    matrix_precedence = matrix(integer(), nrow = 0, ncol = 0),
    matrix_dependency = matrix(integer(), nrow = 0, ncol = 0),
    n_dependencies    = integer(),
    stable_sort       = integer()
  ),
  # test case 3 (graph without dependencies)
  list(
    list_precedence   = list(integer(), integer(), integer()),
    list_dependency   = list(integer(), integer(), integer()),
    matrix_precedence = matrix(0L, nrow = 3, ncol = 3),
    matrix_dependency = matrix(0L, nrow = 3, ncol = 3),
    n_dependencies    = c(0L, 0L, 0L),
    stable_sort       = c(1L, 2L, 3L)
  ),
  # test case 4 (stable sort order test, 2 should be sorted before 3)
  list(
    list_precedence   = list(2L, integer(), integer()),
    list_dependency   = list(integer(), 1L, integer()),
    matrix_precedence = rbind(c(0L, 1L, 0L), c(0L, 0L, 0L), c(0L, 0L, 0L)),
    matrix_dependency = rbind(c(0L, 0L, 0L), c(1L, 0L, 0L), c(0L, 0L, 0L)),
    n_dependencies    = c(0L, 1L, 0L),
    stable_sort       = c(1L, 2L, 3L)
  ),
  # test case 5 (sort order test, 3 should be sorted before 2)
  list(
    list_precedence   = list(integer(), integer(), 2L),
    list_dependency   = list(integer(), 3L, integer()),
    matrix_precedence = rbind(c(0L, 0L, 0L), c(0L, 0L, 0L), c(0L, 1L, 0L)),
    matrix_dependency = rbind(c(0L, 0L, 0L), c(0L, 0L, 1L), c(0L, 0L, 0L)),
    n_dependencies    = c(0L, 1L, 0L),
    stable_sort       = c(1L, 3L, 2L)
  )
)


# add labeled variants to the test data
for(i in seq_along(test_data)) {
  test_data[[i]]$list_precedence_labeled   <- make_labeled_graph(test_data[[i]]$list_precedence)
  test_data[[i]]$list_dependency_labeled   <- make_labeled_graph(test_data[[i]]$list_dependency)
  test_data[[i]]$matrix_precedence_labeled <- make_labeled_graph(test_data[[i]]$matrix_precedence)
  test_data[[i]]$matrix_dependency_labeled <- make_labeled_graph(test_data[[i]]$matrix_dependency)
}
