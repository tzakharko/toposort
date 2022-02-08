# Check that sort_setup() works as expected for all the test cases
for(i in seq_along(test_data)) {
  test_that(fmt("[Test graph #{i}] list precedence encoding is processed correctly"), {
    sort_setup(test_data[[i]]$list_precedence, dependency_type = "precedes")

    # should generate the expected precedence graph
    expect_identical(fixup_graph(precedence_graph), test_data[[i]]$list_precedence)
    # should generate the expected number of dependencies
    expect_identical(n_dependencies, test_data[[i]]$n_dependencies)
  })

  test_that(fmt("[Test graph #{i}] list dependency encoding is processed correctly"), {
    sort_setup(test_data[[i]]$list_dependency, dependency_type = "follows")

    # should generate the expected precedence graph
    expect_identical(fixup_graph(precedence_graph), test_data[[i]]$list_precedence)
    # should generate the expected number of dependencies
    expect_identical(n_dependencies, test_data[[i]]$n_dependencies)
  })

  test_that(fmt("[Test graph #{i}] matrix precedence encoding is processed correctly"), {
    sort_setup(test_data[[i]]$matrix_precedence, dependency_type = "precedes")

    # should generate the expected precedence graph
    expect_identical(fixup_graph(precedence_graph), test_data[[i]]$list_precedence)
    # should generate the expected number of dependencies
    expect_identical(n_dependencies, test_data[[i]]$n_dependencies)
  })

  test_that(fmt("[Test graph #{i}] matrix dependency encoding is processed correctly"), {
    sort_setup(test_data[[i]]$matrix_dependency, dependency_type = "follows")

    # should generate the expected precedence graph
    expect_identical(fixup_graph(precedence_graph), test_data[[i]]$list_precedence)
    # should generate the expected number of dependencies
    expect_identical(n_dependencies, test_data[[i]]$n_dependencies)
  })
}
