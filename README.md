# actions-dashboard

Overview of scheduled unit tests for JASP modules.

The dashboard requires `gh` 1.6.1 or newer to support GitHub Actions'
long installation tokens, and `rlang` 1.2.0 or newer for the type-checking
functions used by `gh`. `renv.lock` pins compatible versions. CI checks token
discovery against the restored dependencies before rendering the dashboard.

## Local preview

Restore dependencies and install the local package from the repository root:

```sh
Rscript -e 'renv::restore(prompt = FALSE); renv::install(".")'
```

Authenticate with `gh auth login`, or supply a token through `GITHUB_PAT`.
Then render the dashboard using live GitHub data:

```sh
Rscript scripts/preview.R
python3 -m http.server 8000 --bind 127.0.0.1 --directory docs
```

Open <http://127.0.0.1:8000>. Click an individual result to open its job log.
Generated files are stored in the ignored `docs/` directory.

## Refresh dependencies

Use the R version recorded in `renv.lock`, then update and validate locally:

```sh
Rscript -e 'renv::update(prompt = FALSE); renv::snapshot(prompt = FALSE); renv::install(".")'
Rscript -e 'source("tests/workflow-token.R"); source("tests/numeric-r-versions.R")'
Rscript scripts/preview.R
```

Commit the lockfile and any regenerated `renv/activate.R` together after the
checks pass. CI uses the lockfile's R version and repeats the regression checks
before rendering and publishing.
