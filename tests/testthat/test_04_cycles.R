test_that("cyclic graphs are detected", {
  g <- structure(list(c(2L, 3L), 3L, 1L), dependency_type = "precedes")

  expect_error(topological_sort(g), class = "toposort/cyclic_depenencies_error")
  expect_error(stable_topological_sort(g), class = "toposort/cyclic_depenencies_error")
})

test_that("cycles are correctly reported", {
  g <- structure(list(c(2L, 3L), 3L, 1L), dependency_type = "precedes")

  cnd <- catch_cnd(topological_sort(g), "toposort/cyclic_depenencies_error")
  expect_identical(cnd$cycles, list(c(1L, 3L, 1L)))

  cnd <- catch_cnd(stable_topological_sort(g), "toposort/cyclic_depenencies_error")
  expect_identical(cnd$cycles, list(c(1L, 3L, 1L)))
})
