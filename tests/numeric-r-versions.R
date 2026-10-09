library(actionsDashboard)

# All-numeric R versions must work without resolving a "release" alias.
# Supply API results locally so the regression check needs no GitHub token.
collect <- get_action_data_as_tib
fixture_env <- new.env(parent = environment(collect))
fixture_env$get_action_results <- function(...) NULL
fixture_env$get_jobs_results <- function(...) list(list(
  date = as.POSIXct("2026-10-07", tz = "UTC"),
  names = c(
    "unit-tests / ubuntu-latest (R 4.5.2 - lockfile packages)",
    "unit-tests / windows-latest (R 4.5.2 - latest packages)"
  ),
  success = c(TRUE, FALSE),
  urls = c("https://github.com/example/jobs/1", "https://github.com/example/jobs/2")
))
environment(collect) <- fixture_env

results <- collect("jaspAnova", enable_cache = FALSE)
stopifnot(
  nrow(results) == 2L,
  !anyNA(results$name_clean),
  identical(results$result, c(TRUE, FALSE))
)
collapsed <- get_collapsed_tib(results)
stopifnot(collapsed$sum_result == 1L, collapsed$n_njobs == 2L)
