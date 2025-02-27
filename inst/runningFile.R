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

library(actionsDashboard)
repos <- get_jasp_repos()
debugonce(get_action_data_as_tib)
get_action_data_as_tib(repos, enable_cache = FALSE)
messages <- capture.output({
  tib_individual  <- try(get_action_data_as_tib(repos, enable_cache = FALSE))
})

# works
rr <- gh::gh(
  "/repos/jasp-stats/jaspDescriptives/actions/workflows/unittests.yml/runs?branch=master&event=schedule&created>=2025-02-13"
  )
# does not work
gh::gh(
  "/repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs?branch={branch}&event={event}&created={created}",
  owner       = "jasp-stats",
  repo        = "jaspDescriptives",
  workflow_id = "unittests.yml",
  branch      = "master",
  event       = "schedule",
  created     = paste0(">=", Sys.Date() - 14),
  per_page    = 100L
)

# works
gh::gh(
  "/repos/{owner}/{repo}/actions/workflows/{workflow_id}/runs?branch={branch}&event={event}&created={created}",
  owner       = "jasp-stats",
  repo        = "jaspDescriptives",
  workflow_id = "unittests.yml",
  branch      = "master",
  event       = "schedule",
  created     = paste0(">", Sys.Date() - 15),
  per_page    = 100L
)

# if you do
# debugonce(gh::gh)
# then gh_build_request
# changes the created parameter into
# created=%3E=2025-02-13
# which fails
