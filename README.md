# actions-dashboard

Overview of scheduled unit tests for JASP modules.

The dashboard requires `gh` 1.6.1 or newer to support GitHub Actions'
long installation tokens. `renv.lock` pins a compatible version.

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
