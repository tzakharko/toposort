describe("topological_sort()", {
  g <- list(c(2L, 3L), 3L, integer())
  labels <- c("a", "b", "c")

  it("argument `dependency_type` must be specified", {
    expect_error(topological_sort(g), regexp = "missing required named argument.*dependency_type")
    expect_error(topological_sort(g, dependency_type = "follows"), regexp = NA)
    expect_error(topological_sort(g, dependency_type = "precedes"), regexp = NA)
    expect_error(topological_sort(g, dependency_type = "other"), regexp = "invalid.*value")
  })

  it("argument `dependency_type` can be specified as an attribute", {
    attr(g, "dependency_type") <- "follows"
    expect_error(topological_sort(g), regexp = NA)
    attr(g, "dependency_type") <- "precedes"
    expect_error(topological_sort(g), regexp = NA)
  })

  it("can accept labeled data", {
    expect_type(topological_sort(g, dependency_type = "precedes", labels  = labels), "character")
    names(g) <- labels
    expect_type(topological_sort(g, dependency_type = "precedes"), "character")
    g <- lapply(g, function(i) labels[i])
    expect_type(topological_sort(g, dependency_type = "precedes"), "character")
  })
})



# Check that topological_sort() works as expected for all the test cases
for(i in seq_along(test_data)) {
  g <- test_data[[i]]

  test_that(fmt("[Test graph #{i}] list with precedence encoding is correctly sorted"), {
    x <- topological_sort(g$list_precedence, dependency_type = "precedes")

    # should be sorted
    expect_topo_sorted(x, g$list_precedence)
  })

  test_that(fmt("[Test graph #{i}] label list with precedence encoding is correctly sorted"), {
    x <- topological_sort(g$list_precedence, dependency_type = "precedes")
    labels <- topological_sort(g$list_precedence_labeled, dependency_type = "precedes")

    # should be identical
    expect_identical(as.integer(labels), x)
  })


  test_that(fmt("[Test graph #{i}] list with dependency encoding is correctly sorted"), {
    x <- topological_sort(g$list_dependency, dependency_type = "follows")

    # should be sorted
    expect_topo_sorted(x, g$list_precedence)
  })

  test_that(fmt("[Test graph #{i}] label list with dependency encoding is correctly sorted"), {
    x <- topological_sort(g$list_dependency, dependency_type = "follows")
    labels <- topological_sort(g$list_dependency_labeled, dependency_type = "follows")

    # should be identical
    expect_identical(as.integer(labels), x)
  })


  test_that(fmt("[Test graph #{i}] matrix with precedence encoding is correctly sorted"), {
    x <- topological_sort(g$matrix_precedence, dependency_type = "precedes")

    # should be sorted
    expect_topo_sorted(x, g$list_precedence)
  })

  test_that(fmt("[Test graph #{i}] label matrix with precedence encoding is correctly sorted"), {
    x <- topological_sort(g$matrix_precedence, dependency_type = "precedes")
    labels <- topological_sort(g$matrix_precedence_labeled, dependency_type = "precedes")

    # should be identical
    expect_identical(as.integer(labels), x)
  })

  test_that(fmt("[Test graph #{i}] matrix with dependency encoding is correctly sorted"), {
    x <- topological_sort(g$matrix_dependency, dependency_type = "follows")

    # should be sorted
    expect_topo_sorted(x, g$list_precedence)
  })

  test_that(fmt("[Test graph #{i}] label matrix with dependency encoding is correctly sorted"), {
    x <- topological_sort(g$matrix_dependency, dependency_type = "follows")
    labels <- topological_sort(g$matrix_dependency_labeled, dependency_type = "follows")

    # should be identical
    expect_identical(as.integer(labels), x)
  })
}
