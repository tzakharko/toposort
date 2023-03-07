test_that("Topological sorting works for randomly generated graphs", {
  skip_on_cran()

  replicate(500, {
    # generate a random graph
    graph <- make_random_acyclic_graph(as.integer(runif(1L, 5, 500 + 1)), FALSE)
    graph_encodings <- build_graph_encodings(graph)

    # test sort variants for different graph encodings
    expect_topo_sorted(
      topological_sort(graph_encodings$list_precedence, dependency_type = "precedes"),
      graph_encodings$list_precedence
    )

    expect_topo_sorted(
      topological_sort(graph_encodings$list_dependency, dependency_type = "follows"),
      graph_encodings$list_precedence
    )

    expect_topo_sorted(
      topological_sort(graph_encodings$matrix_precedence, dependency_type = "precedes"),
      graph_encodings$list_precedence
    )

    expect_topo_sorted(
      topological_sort(graph_encodings$matrix_dependency, dependency_type = "follows"),
      graph_encodings$list_precedence
    )

    # test stable sort variants for different graph encodings
    expect_topo_sorted(
      stable_topological_sort(graph_encodings$list_precedence, dependency_type = "precedes"),
      graph_encodings$list_precedence
    )

    expect_topo_sorted(
      stable_topological_sort(graph_encodings$list_dependency, dependency_type = "follows"),
      graph_encodings$list_precedence
    )

    expect_topo_sorted(
      stable_topological_sort(graph_encodings$matrix_precedence, dependency_type = "precedes"),
      graph_encodings$list_precedence
    )

    expect_topo_sorted(
      stable_topological_sort(graph_encodings$matrix_dependency, dependency_type = "follows"),
      graph_encodings$list_precedence
    )
  })
})
