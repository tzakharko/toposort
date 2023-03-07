
# toposort

<!-- badges: start -->
[![R-CMD-check](https://github.com/tzakharko/toposort/workflows/R-CMD-check/badge.svg)](https://github.com/tzakharko/toposort/actions)
[![CRAN status](https://www.r-pkg.org/badges/version/toposort)](https://CRAN.R-project.org/package=toposort)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

A one stop shop for all your topological sorting needs

## Features

toposort is designed to be a fast, flexible and user-friendly interface to topological sorting. 
Here are some reasons to try this package out: 

- Flexible user interface
  - Pass the input data as a list of dependencies or as an adjacency matrix
  - Specify the dependency type (`i` must precede `j` vs. `i` must follow `j`)
  - Use item indices or unique character labels 
- Stable and unstable topological sort algorithm variants
- Robust error handling and cycle detection
  - Input data validation
  - Detects and reports cycles in the input data
- Good asymptotic performance
  - Reasonably fast on small inputs 
  - Faster than `Rfast::topological_sort()` on large inputs (1000+ items)

## Topological sorting

Topological sorting of a directed graph is an act of producing a linear sequence of its vertices 
such that each vertex occurs before any other vertex it dominates. In simpler terms, it is about
sorting a set of items in the order of their dependencies on each other. This problem often occurs
in practice when we want to process a set of items that are mutually dependent.

Let's say we want to process three items `a`,  `b` and `c` such that `b` is a prerequisite for `a`
and `c` and `c` is prerequisite for `a`.

``` r
library(toposort)
# build the graph
g <- list(
  # a is not a prerequisite for any other item
  a = character(),
  # b is a prerequisite for a and c
  b = c("a", "c"),
  # c is a prerequisite for a
  c = c("a")
)
# the graph encodes precedence (i must precede all g[[i]])
topological_sort(g, dependency_type = "precedes")
```

toposort offers multiple options to encode the data. For example, instead of a graph that encodes 
precedence, one can use a graph that encodes dependencies.

``` r
library(toposort)
# build the graph
g <- list(
  # a dependes on b and c
  a = c("b", "c"),
  # b does not depend on any other item
  b = character(),
  # c dependes on b
  c = c("b")
)
# the graph encodes dependency (i must follow all g[[i]])
topological_sort(g, dependency_type = "follows")
```

Or, you can use an adjacency matrix

``` r
library(toposort)
# build the graph
g <- rbind(
  # a dependes on b and c
  c(F, T, T),
  # b does not depend on any other item
  c(F, F, F),
  # c dependes on b
  c(F, T, F)
)
# the graph encodes dependency (i must follow all g[[i]])
topological_sort(g, dependency_type = "follows", labels = c("a", "b", "c"))
```

Check the documentation `?topological_sort` for more examples

## Cyclic dependencies

Topological sorting is only possible if there are no cyclic dependencies. Figuring out such 
dependencies can be time-consuming, especially on larger inputs, which is why toposort will detect 
and report them for you.

``` r
library(toposort)
# build the graph
g <- list(
  # a dependes on c
  a = c("c"),
  # b depends on a (there is a cyclic dependency here!)
  b = c("a"),
  # c dependes on b
  c = c("b")
)
# the graph encodes dependency (i must follow all g[[i]])
topological_sort(g, dependency_type = "follows")
```

## Installation

You can install the development version of toposort from github:

``` r
# install.packages("pak")
pak::pkg_install("tzakharko/toposort")
```


