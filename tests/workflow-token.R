# GitHub's long installation tokens must pass local validation before API calls.
local({
  vars <- c("GITHUB_PAT", "GITHUB_PAT_GITHUB_COM")
  previous <- Sys.getenv(vars, unset = NA_character_)
  on.exit({
    for (name in vars) {
      if (is.na(previous[[name]]))
        Sys.unsetenv(name)
      else
        do.call(Sys.setenv, setNames(list(previous[[name]]), name))
    }
  })

  token <- paste0("ghs_", strrep("A", 300L))
  Sys.unsetenv("GITHUB_PAT_GITHUB_COM")
  Sys.setenv(GITHUB_PAT = token)
  stopifnot(identical(as.character(gh::gh_token()), token))
})
