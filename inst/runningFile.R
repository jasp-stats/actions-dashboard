library(actionsDashboard)
repos <- get_jasp_repos()
tib_individual    <- get_action_data_as_tib(repos)
tib_collapsed     <- get_collapsed_tib(tib_individual)
repos_successful  <- repos[repos %in% tib_individual$repo]
p_collapsed_runs  <- plot_collapsed_runs(repos_successful, tib_collapsed)
debugonce(plot_individual_runs)
p_individual_runs <- plot_individual_runs(repos_successful, tib_individual)

f <- tempfile(fileext = ".html")
htmltools::save_html(htmltools::tagList(
  htmltools::div(p_collapsed_runs),
  htmltools::div(p_individual_runs)
), file = f)

if (interactive())
  system2("xdg-open", f)

gh::gh_rate_limit()

