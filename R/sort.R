#' Topological sorting
#'
#' @param x The dependency graph. It must be either a list of integer vectors
#'   (where `x[[i]]` are the items `i` depends on) or as a square matrix of
#'   integer or logical flags (where `x[i,j] == 1` indicates that `i` depends
#'   on `j`). The graph can optionally have names (will be taken from row names
#'   for the matrix).
#' @param dependency_type named string argument specifying how to interpret the
#'   graph. This must be either "precedes" (parent nodes in the graph come
#'   before their children) or "follows" (parent nodes in the graph follow
#'   their children). This can also be specified as an attribute of the same
#'   name on the graph input `x`
#' @param labels optional named character vector of item labels. If provided, the
#'   sorted output will use these labels. The default labels are taken from the
#'   names of `x` (row names if it is a matrix), if any are provided. Set to
#'   `NULL` to suppress labels.
#' @inheritParams rlang::args_dots_empty
#' @return Items in their order of precedence (earlier items first). This is
#'   either an integer vector of item indices or a character vector of item
#'   labels (if labels were provided).
#'
#' @details
#'
#' The dependency structure can be encoded in a number of different ways for
#' flexibility (see examples).
#'
#' `stable_topological_sort()` guarantees stable sort order (items without mutual
#' dependencies will be sorted in the order of occurrence). `topological_sort()` makes
#' no such guarantees and might offer improved performance in future versions of the package.
#'
#' An informative error is raised if cycles are detected in the dependency
#' graph. The error condition has the class `toposort/cyclic_depenencies_error` and
#' the element `cycles` of the condition will contain the list of detected cycles
#'
#' @examples
#' # the following examples show the different ways to encode the
#' # dependency structure of four items, where item 1 precedes items 2 and 3,
#' # item 2 precedes item 4, and item 3 precedes item 2
#'
#' # list with items encoded by their precedence (i precedes all x[[i]])
#' x <- list(c(2L, 3L), 3L, 4L, integer())
#' topological_sort(x, dependency_type = "precedes")
#' stable_topological_sort(x, dependency_type = "precedes")
#'
#' # list with items encoded by their antecedence (i follows all x[[i]]))
#' x <- list(integer(), c(1L, 3L), 1L, 2L)
#' topological_sort(x, dependency_type = "follows")
#' stable_topological_sort(x, dependency_type = "follows")
#'
#' # matrix with items encoded by their precedence
#' x <- matrix(FALSE, ncol = 4, nrow = 4)
#' x[1L, c(2L, 3L)] <- TRUE
#' x[2L, 4L] <- TRUE
#' x[3L, 2L] <- TRUE
#' topological_sort(x, dependency_type = "precedes")
#' stable_topological_sort(x, dependency_type = "precedes")
#'
#' # matrix with items encoded by their antecedence
#' x <- matrix(FALSE, ncol = 4, nrow = 4)
#' x[2L, c(1L, 3L)] <- TRUE
#' x[3L, 1L] <- TRUE
#' x[4L, 2L] <- TRUE
#' topological_sort(x, dependency_type = "follows")
#' stable_topological_sort(x, dependency_type = "follows")
#'
#' @export
topological_sort <- NULL


#' @rdname topological_sort
#' @export
stable_topological_sort <- function(
  x,
  ...,
  dependency_type,
  labels = vec_names(x)
) {
  # check the arguments and setup the sort parameters
  precedence_graph <- NULL
  n_dependencies <- NULL

  if(...length() != 0L) abort("unknown unnamed parameters passed to function")
  sort_setup(x, dependency_type, labels)

  # sorted items
  sorted <- integer()

  repeat {
    i <- match(0L, n_dependencies, nomatch = 0L)
    if (i == 0L) break

    # add the item to the sorted list
    sorted <- c(sorted, i)

    # reduce the dependency count for all dependent items
    ii <- precedence_graph[[i]]
    n_dependencies[ii] <- n_dependencies[ii] - 1L

    # unmark the item
    n_dependencies[i] <- -1L
  }

  # check if we got cycles
  if (length(sorted) != length(precedence_graph)) {
    call <- call2(sys.call()[[1]], x = x, dependency_type = dependency_type, labels = labels)
    cycles <- detect_cycles(precedence_graph, sorted)
    cyclic_graph_error(cycles, call = call)
  }

  # return sorted items
  if (!is_null(labels)) labels[sorted] else sorted
}

topological_sort <- stable_topological_sort
