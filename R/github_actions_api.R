# TODO:

# - [ ] create webpage with github action
# - [ ] do not use cache on github actions!

# DONE:
# - [x] better formatting of p_individual_runs
# - [x] add some tries so things are allowed to fail
# - [x] urls don't add up, they are correct in the df though - looks like the y-axis labels are messed up


get_action_results <- function(repo, workflow_id = "unittests.yml", owner = "jasp-stats", branch = "master", event = c("schedule"),
                               created = paste(">=", Sys.Date() - 14)) {

  results <- gh::gh(
    "/repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs?branch={branch}&event={event}&created={created}",
    owner       = owner,
    repo        = repo,
    workflow_id = workflow_id,
    branch      = branch,
    event       = event,
    created     = created,
    per_page    = 100L
  )
  return(results)
}

get_jobs_results <- function(action_results) {

  workflow_runs <- action_results[["workflow_runs"]]
  results <- vector("list", length(workflow_runs))
  for (i in seq_along(workflow_runs)) {
    jobs <- gh::gh(workflow_runs[[i]]$jobs_url)
    jobs <- Filter(\(x) !grepl("scheduled-run-check", x[["name"]], fixed = TRUE), jobs$jobs)

    results[[i]]$jobs    <- jobs
    results[[i]]$date    <- as.POSIXct(workflow_runs[[i]]$created_at, format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")
    results[[i]]$names   <- vapply(jobs, `[[`, "name", FUN.VALUE = "")
    results[[i]]$success <- vapply(jobs, \(x) identical(x[["conclusion"]],  "success"), FUN.VALUE = TRUE)
    results[[i]]$urls    <- vapply(jobs, `[[`, "html_url", FUN.VALUE = "")
  }
  return(results)
}

#' @export
get_jasp_repos <- function() {

  repos_results <- gh::gh("/orgs/{org}/repos?per_page={per_page}", org = "jasp-stats", per_page = 100)
  repos <- vapply(repos_results, `[[`, "name", FUN.VALUE = "")
  # exclude non jasp modules
  repos <- repos[grepl("jasp[A-Z]", repos)]

  # some custom exclusions
  repos <- setdiff(repos, c("jaspTools", "jaspColumnEncoder", "jaspResults",
                            # jaspVisualModeling has no unit tests
                            "jaspBase", "jaspGraphs", "jaspVisualModeling", "jaspTestModule"))

  # sort: first the common modules, then the rest alphabetically
  common_modules <- c("jaspDescriptives", "jaspTTests", "jaspAnova", "jaspMixedModels", "jaspRegression", "jaspFrequencies", "jaspFactor")

  # this way it doesn't break if we ever change the common modules
  repos <- c(common_modules[common_modules %in% repos], sort(setdiff(repos, common_modules)))

  return(repos)
}

#' @export
get_action_data_as_tib <- function(repos, force = FALSE, enable_cache = identical(.Platform$OS.type, "unix")) {

  tib_results <- tibble::tibble()

  if (identical(getOption("CI", "false"), "true")) {
    cat("Running on CI, disabling cache")
    enable_cache <- FALSE
  }

  cache_root <- file.path("~", ".cache", "R", "jaspActionsDashboard-cache")
  if (enable_cache) {
    if (!dir.exists(cache_root))
      dir.create(cache_root, recursive = TRUE)
    cache_root <- normalizePath(cache_root)
  }

  for (i in seq_along(repos)) {

    repo <- repos[i]
    cat(sprintf("[%02d/%02d] Making API calls for module %s...\n", i, length(repos), repo))

    cache_file <- file.path(cache_root, paste0(repo, "-", Sys.Date(), ".rds"))
    reloaded_from_cache <- FALSE
    if (enable_cache && !force && file.exists(cache_file)) {

      cat(sprintf("[%02d/%02d] Loaded API calls for module %s from the cache at %s\n", i, length(repos), repo, cache_file))
      temp <- readRDS(cache_file)
      action_results <- temp[["action_results"]]
      job_results    <- temp[["job_results"]]
      reloaded_from_cache <- TRUE

    } else {

      e <- try({
        action_results <- get_action_results(repo)
        job_results    <- get_jobs_results(action_results)
      }, silent = TRUE)

      if (inherits(e, "try-error")) {
        cat(sprintf("[%02d/%02d] Module %s failed with message:\n%s\n", i, length(repos), repo, e))
        next
      }
    }

    if (enable_cache && !reloaded_from_cache) {
      cat(sprintf("[%02d/%02d] Saved API calls for module %s to the cache at %s\n", i, length(repos), repo, cache_file))
      saveRDS(list(action_results = action_results, job_results = job_results), file = cache_file)
    }

    no_results <- Reduce(\(y, x) y + length(x[["names"]]), job_results, init = 0L)
    repo_result_df <- tibble::tibble(
      repo     = rep(repo, no_results),
      name     = unlist(lapply(job_results, `[[`, "names")),
      datetime = Reduce(c, lapply(job_results, \(x) rep(x[["date"]], length(x[["names"]])))),
      result   = unlist(lapply(job_results, `[[`, "success")),
      url      = unlist(lapply(job_results, `[[`, "urls"))
    )
    tib_results <- rbind(tib_results, repo_result_df)
  }

  tib_results <- tib_results |>
    dplyr::mutate(
      # determines the order for the plots
      repo          = factor(repo, levels = rev(repos)),
      name_clean    = gsub("unit-tests / (.*)-latest \\(R (.*)\\)", "\\1 R-\\2", name),
      date          = as.Date(datetime),
      repo_name_int = interaction(repo, name_clean, sep = " ", lex.order = TRUE)
    )

  oses      <- unique(gsub("unit-tests / (.*)-latest \\(R (.*)\\)", "\\1", tib_results$name))
  rversions <- unique(gsub("unit-tests / (.*)-latest \\(R (.*)\\)", "\\2", tib_results$name))
  oses <- c("windows", "macOS", "ubuntu")

  all_combinations <- expand.grid(oses, sort(package_version(rversions), decreasing = TRUE))
  level_order <- paste0(all_combinations[[1]], " R-", as.character(all_combinations[[2]]))

  level_order <- intersect(level_order, tib_results$name_clean)
  tib_results$name_clean <- factor(tib_results$name_clean, levels = rev(level_order))

  return(tib_results)

}

#' @export
plot_individual_runs <- function(repos, tib) {

  js <- "
    function(el, x) {
      el.on('plotly_click', function(d) {
        console.log(d)
        var point = d.points[0];
        console.log(point)
        var url = point.data.customdata[point.pointIndex];
        window.open(url);
      });
    }"

  colors_individual_runs <- setNames(c("red", "green"), c(FALSE, TRUE))

  abs_sizes <- tib |> dplyr::group_by(repo) |> dplyr::summarise(n = dplyr::n(), .groups = "drop")
  abs_sizes <- abs_sizes[["n"]]
  rel_sizes <- abs_sizes / sum(abs_sizes)

  p_individual_runs <- lapply(repos, \(repository) {
    d <- dplyr::filter(tib, repo == repository)
    plotly::plot_ly(
      d,
      x = ~date,
      y = ~name_clean,
      symbol = ~result,
      symbols = c("x-dot", "circle-open"),
      color = ~result,
      colors = colors_individual_runs,
      size = 5,
      customdata = d$url,
      type = "scatter",
      mode = "markers",
      height = vctrs::vec_unique_count(d$name_clean) * length(repos) * 450 / 9
    ) |>
      plotly::layout(
        xaxis = list(
          title      = "",
          type       = "date",
          tickformat = "%a\n%d/%m",
          tickvals   = unique(tib$date),
          ticklabelmode = "period"
        ),
        yaxis = list(
          title    = repository
        ),
        showlegend = FALSE
      )
  }) |>
    plotly::subplot(nrows = length(repos), margin = 0.002, shareX = TRUE, titleY = TRUE) |>
    htmlwidgets::onRender(jsCode = js)

  return(p_individual_runs)
}

#' @export
plot_collapsed_runs <- function(repos, tib) {

  # TODO: should be colorblind friendly!
  cols <- scales::div_gradient_pal(
    low   = "red",
    mid   = "orange",
    high  = "green",
    space = "Lab"
  )

  max_jobs <- max(tib$n_njobs)
  col_values <- cols(0:max_jobs / max_jobs)
  # scales::show_col(col_values)
  col_fact <- stats::setNames(col_values, 0:max_jobs)

  # TODO: onHover should show which runs failed!
  p_collapsed_runs <- plotly::plot_ly(tib, height = 30 * vctrs::vec_unique_count(tib$repo)) |>
    plotly::add_markers(
      x = ~date,
      y = ~repo,
      color = ~factor(sum_result),
      colors = col_fact,
      size = 5
    ) |>
    plotly::layout(
      xaxis = list(
        title      = "",
        type       = 'date',
        tickformat = "%a\n%d/%m",
        tickvals   = unique(tib$date)
      ),
      yaxis = list(
        title    = ""
      )
    )
  return(p_collapsed_runs)
}

#' @export
get_collapsed_tib <- function(tib) {
  tib |>
  dplyr::group_by(repo, date) |>
  dplyr::summarise(
    sum_result = sum(result),
    n_njobs    = length(result),
    url        = url[1],
    .groups = "keep"
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(date = as.Date(date))
}
