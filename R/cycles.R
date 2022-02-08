pcombine <- function(x, y) {
  out <- vector("list", length(x))
  for(i in seq_along((x))) out[[i]] <- c(x[[i]], y[[i]])
  out
}

is_substring <- function(x, of) {
  any(vapply(which(of == x[1L]), function(i) {
    identical(of[i:(i+ length(x) - 1L)], x)
  }, TRUE))
}

#' @importFrom vctrs vec_duplicate_any vec_rep_each list_sizes
detect_cycles <- function(precedence_graph, to_exclude = integer()) {
  # remove all the sorted items from the precedence graph
  # this will reduce the amount of work we need to do for cycle detection
  precedence_graph <- lapply(precedence_graph, setdiff, to_exclude)

  # initial set of traces
  traces <- pcombine(
    # outgoign node
    vec_rep_each(seq_along(precedence_graph), list_sizes(precedence_graph)),
    # incoming node
    c(precedence_graph, recursive = TRUE)
  )

  cycles <- list()

  # we only take each edge once!
  i <- 1
  while(length(traces) > 0) {
    # extract traces with cycles
    is_cyclic <- vapply(traces, vec_duplicate_any, FALSE)
    cycles <- c(cycles, traces[is_cyclic])
    traces <- traces[!is_cyclic]

    # find all continuations
    from <- vapply(traces, function(x) x[length(x)], 0L)
    to <- precedence_graph[from]

    # number of continuations for each trace
    n <- list_sizes(to)

    # extend all continuations
    traces <- vec_rep_each(traces, n)
    traces <- pcombine(traces, c(to, recursive = TRUE))

    i <- i + 1L
  }

  # clean up overlapping cycles
  # a cycle y includes cycle x if
  # y includes the sequence x[-length(x)] or x[-1L]
  i <- 1L
  while(i < length(cycles)) {
    # substrings to check
    x <- cycles[[i]]
    substrings <- list(x[-1L], x[-length(x)])

    j <- i + 1L
    while(j <= length(cycles)) {
      if(any(vapply(substrings, is_substring, FALSE, cycles[[j]]))) {
        cycles <- cycles[-j]
      } else {
        j <- j + 1L
      }
    }

    i <- i + 1L
  }

  cycles
}



#' @importFrom vctrs data_frame list_sizes
cyclic_graph_error <- function(cycles, call) {
  abort(
    "cyclic dependencies detected",
    cycles = cycles,
    call = call,
    class = "toposort/cyclic_depenencies_error"
  )
}

#' @importFrom utils head
#' @export
`cnd_body.toposort/cyclic_depenencies_error` <- function(cnd, ...) {
  cycles <- cnd$cycles

  # add the labels if provided
  if(!is_null(labels <- cnd$call$labels)) {
    cycles <- lapply(cycles, function(x) labels[x])
  }

  # format the cycles
  out <- vapply(head(cycles), glue::glue_collapse, "", " \u2192 ")
  if(length(out) < length(cycles)) out <- c(out, "...")

  c(" ", paste0("  ", out), " ")
}
