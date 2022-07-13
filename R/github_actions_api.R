# TODO:

# - [ ] create webpage with github action
# - [ ] do not use cache on github actions!
# - [ ] better formatting of p_individual_runs
# - [ ] add some tries so things are allowed to fail

# DONE:
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
  # raw_results <- setNames(vector("list", length(repos)), repos)
  tib_results <- tibble::tibble()

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

    # raw_results[[repo]] <- list(action_results = action_results, job_results = job_results)
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
  col_fact <- setNames(col_values, 0:max_jobs)

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


# # for testing
# repos <- repos[1:5] #c("jaspDescriptives", "jaspTTests", )
# raw_results <- setNames(vector("list", length(repos)), repos)
# df_results <- data.frame()
# for (i in seq_along(repos)) {
#
#   repo <- repos[i]
#   cat(sprintf("[%02d/%02d] Making API calls for module %s...\n", i, length(repos), repo))
#   action_results <- get_action_results(repo)
#   job_results   <- get_jobs_results(action_results)
#
#   raw_results[[repo]] <- list(action_results = action_results, job_results = job_results)
#   no_results <- Reduce(\(y, x) y + length(x[["names"]]), job_results, init = 0L)
#   repo_result_df <- data.frame(
#     repo   = rep(repo, no_results),
#     name   = unlist(lapply(job_results, `[[`, "names")),
#     date   = Reduce(c, lapply(job_results, \(x) rep(x[["date"]], length(x[["names"]])))),
#     result = unlist(lapply(job_results, `[[`, "success")),
#     url    = unlist(lapply(job_results, `[[`, "urls"))
#   )
#   df_results <- rbind(df_results, repo_result_df)
# }
#
# # df_results$repo <- factor(df_results$repo, levels = rev(repos))
#
# df_results2 <- df_results |>
#   mutate(
#     # determines the order for the plots
#     repo          = factor(repo, levels = rev(repos)),
#     name_clean    = gsub("unit-tests / (.*)-latest \\(R (.*)\\)", "\\1 R-\\2", name),
#     date          = as.Date(date),
#     repo_name_int = interaction(repo, name_clean, sep = " ", lex.order = TRUE)
#   )
#
# js <- "
# function(el, x) {
#   el.on('plotly_click', function(d) {
#     console.log(d)
#     var point = d.points[0];
#     console.log(point)
#     var url = point.data.customdata[point.pointIndex];
#     window.open(url);
#   });
# }"
#
# colors_individual_runs <- setNames(c("red", "green"), c(FALSE, TRUE))
#
# p_individual_runs <- lapply(repos, \(repository) {
#   d <- df_results2 |> filter(repo == repository)
#   d$repo_name_int <- droplevels(d$repo_name_int)
#   plot_ly(
#     d,
#     x = ~date,
#     y = ~repo_name_int,
#     symbol = ~result,
#     symbols = c("x-dot", "circle-open"),
#     color = ~result,
#     colors = colors_individual_runs,
#     size = 5,
#     customdata = d$url,
#     type = "scatter",
#     mode = "markers",
#     # autosize = TRUE,
#     height = vctrs::vec_unique_count(d$name_clean) * length(repos) * 200 / 9
#     # height = vctrs::vec_unique_count(d$name_clean) * 200 / 9
#   ) |>
#     # add_markers(
#     #   # x = ~as.integer(date),
#     #   x = ~date,
#     #   y = ~repo_name_int,
#     #   symbol = ~result,
#     #   symbols = c("x-dot", "circle-open"),
#     #   color = ~result,
#     #   colors = colors_individual_runs,
#     #   size = 5
#     # ) |>
#     layout(
#       xaxis = list(
#         title      = "",
#         # type       = "category",
#         # ticktext   = strftime(unique(df_results2$date), "%a\n%d/%m"),
#         # tickvals   = unique(as.integer(df_results2$date))
#         type       = "date",
#         tickformat = "%a\n%d/%m",
#         tickvals   = unique(df_results2$date),
#         ticklabelmode = "period"
#       ),
#       yaxis = list(
#         title    = repository,
#         ticktext = unique(d$name_clean),
#         tickvals = unique(as.integer(d$repo_name_int)) - 1L,
#         tickmode = "array"
#       ),
#       showlegend = FALSE
#     )# |> htmlwidgets::onRender(jsCode = js)
# }) |>
#   subplot(nrows = length(repos), shareX = TRUE, titleY = TRUE) |>
#   htmlwidgets::onRender(jsCode = js)
#
# p_individual_runs
# # https://plotly.com/r/time-series/ the buttons are pretty cool
#
# # p_individual_runs$x$layout$xaxis
# # for (i in 2:length(repos)) {
# #   xaxis <- p_individual_runs$x$layout$xaxis
# #   xaxis$anchor <- paste0("y", i - 1)
# #   p_individual_runs$x$layout[[paste0("xaxis", i)]] <- xaxis
# # }
#
# df_results_collaped <- df_results2 |>
#   group_by(repo, date) |>
#   summarise(
#     sum_result = sum(result),
#     n_njobs    = length(result),
#     url        = url[1]
#   ) |>
#   ungroup() |>
#   mutate(date = as.Date(date))
#
# cols <- scales::div_gradient_pal(
#   low = "red",
#   mid = "orange",
#   high = "green",
#   space = "Lab"
# )
#
# max_jobs <- max(df_results_collaped$n_njobs)
# col_values <- cols(0:max_jobs / max_jobs)
# # scales::show_col(col_values)
# col_fact <- setNames(col_values, 0:max_jobs)
#
# p_collapsed_runs <- plot_ly(df_results_collaped) |>
#   add_markers(
#     x = ~date,
#     y = ~repo,
#     color = ~factor(sum_result),
#     colors = col_fact,
#     size = 5
#   ) |>
#   layout(
#     xaxis = list(
#       title      = "",
#       type       = 'date',
#       tickformat = "%a\n%d/%m",
#       tickvals   = unique(df_results_collaped$date)
#     ),
#     yaxis = list(
#       title    = ""
#     )
#   )
#
# f <- tempfile(fileext = ".html")
# save_html(tagList(
#   div(p_collapsed_runs),
#   div(p_individual_runs)
# ), file = f)
# system2("xdg-open", f)


#
# scales::show_col(scales::dichromat_pal("GreentoMagenta.16")(16))
#
# g00 <- ggplot(data = df_results_collaped, aes(x = date, y = repo, fill = factor(sum_result))) +
#   ggplot2::geom_raster() +
#   scale_x_date(name = NULL, breaks = unique(df_results_collaped$date), date_labels = "%a\n%d/%m") +
#   scale_fill_manual(name = "No passed jobs", values = col_fact) +
#   labs(y = NULL) +
#   theme_bw()
#
# ggplotly(g00)
#
# df_results$name_clean <- df_results$name
# df_results$name_clean <- gsub(
#   "unit-tests / (.*)-latest \\(R (.*)\\)",
#   "\\1 R-\\2",
#   df_results$name_clean
# )
#
# df_results2 <- df_results |>
#   mutate(
#     name_clean = gsub("unit-tests / (.*)-latest \\(R (.*)\\)", "\\1 R-\\2", name),
#     date = as.Date(date),
#     repo_name_int = interaction(df_results$repo, df_results$name_clean, sep = " ", lex.order = TRUE)
#   )
#
# sort(levels(interaction(df_results$repo, df_results$name_clean, sep = " ", lex.order = TRUE)))
# df_results2$name_clean
#
#
# g11 <- ggplot(data = df_results2, aes(x = date, y = repo_name_int, fill = factor(result))) +
#   ggplot2::geom_raster(show.legend = FALSE) +
#   scale_x_date(name = NULL, breaks = unique(df_results2$date), date_labels = "%a\n%d/%m") +
#   scale_fill_manual(name = NULL, values = cols2) +
#   scale_y_discrete(labels = df_results2$name_clean) +
#   labs(y = NULL) +
#   facet_wrap(~repo, ncol = 1, strip.position = "left", scales = "free_y") +
#   theme(panel.spacing = unit(0, "lines"),
#         strip.background = element_blank(),
#         strip.placement = "outside")# +
#   # theme_bw()
# obj <- ggplotly(g11)
# obj$x$layout$annotations[[1]]$text <- ""
# obj$x$layout$annotations[[2]]$textangle <- 270
# obj$x$layout$annotations[[2]]$annotationType <- "axis"
# obj$x$layout$annotations[[2]]$x <- 0
# obj$x$layout$annotations[[2]] <- 0
#
#
# df_results2 |>
#   group_by()
#
# plot_ly(df_results2 |> subset(repo == repos[[1]])) |>
#   add_markers(
#     x = ~date,
#     y = ~repo_name_int,
#     symbol = ~result,
#     symbols = c("x-dot", "circle-open"),
#     color = ~result,
#     colors = cols2,
#     size = 5
#   ) |>
#   # add_histogram2d(x = ~date, y = ~repo_name_int, z = ~result) |>
#   # add_area(x = ~date, y = ~repo_name_int, z = ~result) |>
#   layout(
#     xaxis = list(
#       type = 'date',
#       tickformat = "%a\n%d/%m",
#       tickvals = unique(df_results2$date)
#     ),
#     yaxis = list(
#       title = repos[[1]]
#     )
#   )
#
#
#
#
#
# p1 <- plot_ly(economics, x = ~date, y = ~uempmed)
# p2 <- plot_ly(economics, x = ~date, y = ~unemploy)
# subplot(p1, p2, p1, p2, nrows = 2, margin = 0.05)
#
# p <- plot_ly(mpg, x = ~cty, y = ~hwy, alpha = 0.3)
# subplot(
#   add_markers(p, symbol = ~cyl, name = "A single trace"),
#   add_markers(p, symbol = ~factor(cyl), color = I("black"))
# )
#
#
#
# data <- read.table(text = "Group Category Value
#     S1 A   73
#     S2 A   57
#     S1 B   7
#     S2 B   23
#     S1 C   51
#     S2 C   87", header = TRUE)
# obj <- ggplotly(ggplot(data = data, aes(x = Group, y = Value, fill = Group)) +
#   geom_bar(stat = "identity", width = 1) +
#   geom_text(aes(label = paste(Value, "%")), vjust = -0.25) +
#   facet_wrap(~Category, strip.position = "bottom", scales = "free_x") +
#   theme(panel.spacing = unit(0, "lines"),
#         strip.background = element_blank(),
#         strip.placement = "outside") + coord_flip())
#
# obj$x$layout$annotations[[1]]$y
# for (i in 3:5) {
#   anno1 <- obj$x$layout$annotations[[1]]
#   anno1$text <- obj$x$layout$annotations[[i]]$text
#   anno1$x    <- obj$x$layout$annotations[[i]]$x
#   obj$x$layout$annotations[[i]] <- anno1
# }
# obj$x$layout$annotations <- obj$x$layout$annotations[-1]

#
# rep(job_results[[1]][["date"]], length(job_results[[1]][["names"]]))
#
# get_action_url <- function(owner, repo, branch = "master", event = c("schedule"),
#                            created = paste(">=", Sys.Date() - 30)) {
#   return(sprintf(
#     "https://api.github.com/repos/%s/%s/actions/runs?branch=%s&event=%s&created=%s",
#     owner, repo, branch, event, created
#   ))
# }
#
# get_json_from_url <- jsonlite::fromJSON
#
# get_action_url(
#   "jasp-stats",
#   "jaspDescriptives"
# )
#
# res <- get_action_url(
#   "jasp-stats",
#   "jaspDescriptives"
# ) |> jsonlite::fromJSON()
#
# resp <- httr::GET(
#   "https://api.github.com/repos/jasp-stats/jaspDescriptives/actions/runs?branch=master&event=schedule&created=>= 2022-06-12",
#   httr::add_headers(.headers = c(
#     "Accept: application/vnd.github+json"),
#     paste0("Authorization: token", Sys.getenv("GITHUB_PAT"))
#   )
#   # httr::add_headers()
#   # httr::authenticate("token", Sys.getenv("GITHUB_PAT"))
# )
# resp$status_code
#
# get_action_url(
#   "jasp-stats",
#   "jaspDescriptives"
# ) |> jsonlite::fromJSON()
#
# #get_json_from_url()
