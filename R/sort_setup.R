#' Setup topological sort parameters
#'
#' Auxiliary function for `topological_sort()`, takes the arguments
#' of the main function and prepares a precedence based graph structure
#' used in the sorting algorithm
#'
#' @importFrom vctrs vec_size vec_split list_sizes vec_names vec_rep_each
#' @keywords internal
#' @noRd
sort_setup <- function(x, dependency_type, labels = NULL) {
  env <- parent.frame()
  .__error_call__. <- env

  # x must be provided
  if (missing(x)) {
    abort(fmt("missing required argument {.arg x}"))
  }
  if (!is_bare_list(x) && !is.matrix(x)) {
    abort(fmt("{.arg x} must be a list or a matrix"))
  }
  # size of the graph
  n <- vec_size(x)

  # check dependency_type
  if(missing(dependency_type) && is_null(dependency_type <- attr(x, "dependency_type", TRUE))) {
    abort(fmt("missing required named argument {.arg dependency_type}"))
  }
  if(!is_string(dependency_type)) {
    abort(fmt("{.arg dependency_type} must be a character string"))
  }
  env$dependency_type <- dependency_type
  precedes <- switch(dependency_type,
    precedes = 1L,
    follows  = 0L,
    abort(fmt("invalid {.arg dependency_type} value '{dependency_type}'"))
  )

  # check the labels
  if(!is_null(labels) && !(is_bare_character(labels) && length(labels) == vec_size(x))) {
    abort(fmt("{.arg labels} must be a character vector of the same size as {.arg x}"))
  }


  # if the graph is empty, we exit early
  if(n == 0) {
    env$precedence_graph <- list()
    env$n_dependencies <- integer()

    return()
  }

  if(is.list(x)) {
    q <- seq_len(n)
    items <- c(x, recursive = TRUE)
    # check type of items
    items <- switch(typeof(items),
      integer = {
        if(anyNA(items) || any(items <= 0 | items > n)) {
          abort(fmt("{.arg x} contains invalid indices"))
        }

        items
      },
      # labels will need to be recoded into integer
      character = {
        items <- match(items, labels)
        if(anyNA(items)) {
          msg <- if(is_null(labels)) {
            fmt("{.arg labels} must be provided if values of {.arg x} are character vectors")
          } else {
            fmt("character values in {.arg x} do not match the provided {.arg labels}")
          }

          abort(msg)
        }

        items
      },
      # nothign else is accepted
      {
        abort(fmt("elements of {.arg x} must be integer or character vectors"))
      }
    )
    # graph encodes precedence (i comes before x[[i]]), which is what we
    # want to work with, we only need the number of incoming edges
    if(precedes) {
      precedence_graph <- if(typeof(x[[1]]) == "character") {
        # we need to recode the values to integer indexes...
        groups <- vec_split(items, vec_rep_each(q, list_sizes(x)))
        groups$val[match(q, groups$key)]
      } else {
        x
      }
      n_dependencies <- tabulate(items, length(x))
    } else
    # graph encodes dependency (i comes after x[[i]]), we want to recode it
    # to precedece, i.e. we have a -> b, a -> c but we want to have  b -> a, c -> a
    {
      # number of incoming edges is just the size of the vector
      n_dependencies <- list_sizes(x)
      # split the vector encoding dependent items according to the items they depend on
      groups <- vec_split(vec_rep_each(q, n_dependencies), items)
      precedence_graph <- groups$val[match(q, groups$key)]
    }
  }
  # it is a matrix
  else if(is.matrix(x)) {
    if(ncol(x) != nrow(x)) {
      abort(fmt("matrix {.arg x} must be square"))
    }
    # check x values
    x <- switch(typeof(x),
      logical = {
        if(anyNA(x)) {
          abort(fmt("matrix {.arg x} contains NAs"))
        }

        x
      },
      integer = {
        if(anyNA(x)) {
          abort(fmt("matrix {.arg x} contains NAs"))
        }
        if(!all(x == 0L | x == 1L)) {
         abort(fmt("matrix {.arg x} contains invaid values"))
        }

        x == 1
      },
      abort(fmt("elements of matrix {.arg x} must be of type integer or logical"))
    )

    q <- seq_len(n)
    n_dependencies <- integer(n)
    precedence_graph <- vector("list", n)

    # x[i, j] means that i comes before j
    if(precedes) {
      # number of dependencies is the count of non-zero entries per colums
      # precedence graph encodes the rows
      for(i in q) {
        precedence_graph[[i]] <- which(x[i, ])
        n_dependencies[[i]] <- sum(x[, i])
      }
    }
    # x[i, j] means that i comes after j
    else {
      # number of dependencies is the count of non-zero entries per colums
      # precedence graph encodes the columns
      for(i in q) {
        precedence_graph[[i]] <- which(x[, i])
        n_dependencies[[i]] <- sum(x[i, ])
      }
    }
  }

  env$precedence_graph <- precedence_graph
  env$n_dependencies <- n_dependencies
}
