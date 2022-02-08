# # sorting failures will be collected here
# sort_failures_env <- new_environment(list(failures = list()))

# # # Run after all tests
# withr::defer({
#   print("hello friends")
#   print(sort_failures_env$failures)

#   if(length(sort_failures_env$failures) > 0) {
#     cli::cli_alert_warning("Encountered {length(sort_failures_env$failures)} sort failure{?s}")

#     file <- tempfile(tmpdir = getwd())
#     saveRDS(sort_failures_env$failures, file)

#     cli::cli_alert_info("Failure info written to {.uri {file}}")
#   }
# }, teardown_env())
