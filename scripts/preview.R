# Run from the repository root after restoring dependencies and installing ".".
if (!nzchar(Sys.getenv("GITHUB_PAT")) && !nzchar(Sys.getenv("GITHUB_TOKEN"))) {
  if (!nzchar(Sys.which("gh")))
    stop("Set GITHUB_PAT or authenticate with the GitHub CLI (gh auth login).")

  token <- system2("gh", c("auth", "token"), stdout = TRUE)
  if (!is.null(attr(token, "status")) || length(token) != 1L || !nzchar(token))
    stop("Authenticate with the GitHub CLI (gh auth login), then retry.")

  Sys.setenv(GITHUB_PAT = token)
  rm(token)
}

dir.create("docs", showWarnings = FALSE)
rmarkdown::render(
  "vignettes/actions-dashboard.Rmd",
  rmarkdown::html_vignette(),
  output_file = "index.html",
  output_dir = "docs"
)
