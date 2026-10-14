# dbreg - AI Assistant Context

## Package Overview

**dbreg** is an R package for fast regressions on database backends, designed for datasets too large for R's memory. Uses DuckDB as the default backend with support for other DBI-compatible databases. Zero non-essential dependencies in Imports.

- Main function: `dbreg()` — regression with automatic strategy selection
- IV regression: `dbivreg()` — 2SLS via the same strategies (`moments`, `demean`, `compress` without FE)
- Binscatter: `dbbinsreg()` — binned scatter plots on database backends
- SQL design matrix: `sql_model_matrix()` — factor/interaction expansion to SQL

## Quick Reference

Always use `pkgload::load_all()` for development — never `library(dbreg)`.

```r
pkgload::load_all()

# Then test interactively, e.g.
dbreg(weight ~ Time | Diet, data = ChickWeight)
dbreg(weight ~ Time | Diet, data = ChickWeight, vcov = "hc1")
dbreg(weight ~ Time | Diet + Chick, data = ChickWeight, strategy = "mundlak")
```

## Repository Structure

- `R/` — Package source:
  - `dbreg.R` — public API, input processing, strategy selection, alternating projections, finalization
  - `dbivreg.R` — IV public API, IV formula parsing, IV strategies, first-stage/overid diagnostics
  - `strategies.R` — strategy execution functions (called from `dbreg.R`)
  - `frames.R` — estimation frames: base, demeaned (incl. AP) CTEs and the moments query over them
  - `fit.R` — estimators on sufficient statistics (`cross_moments()`, `fit_ols()`, `fit_2sls()`, `fit_meat()`)
  - `vcov.R` — variance-covariance and meat matrix computation (shared across strategies)
  - `utils.R` — shared helpers: SQL dialect, formula parsing, connection setup, `env2env`
  - `dbbinsreg.R` — binscatter on database backends
  - `sql_model_matrix.R` — factor/interaction expansion to SQL
  - `stats-methods.R`, `tidiers.R`, `print.R`, `gof.R`, `plot.r` — S3 methods and output
- `inst/tinytest/` — Test suite (tinytest framework).
- `man/` — roxygen2-generated `.Rd` files.
- `vignettes/` — Package vignette (`intro.qmd`).
- `_benchmarks/` — Performance comparisons with plots.
- `nyc-taxi/` — Large test dataset (180M rows, Hive-partitioned parquet). Not in repo; see download instructions below.
- `SCRATCH/` — Developer scratch files and experiments (not part of the package).

## Code Style & Conventions

### Assignment & Syntax
```r
# Use = not <-
x = 5

# Prefer explicit function() (not \() for broader compatibility)
fn = function(x) x^2

# Prefer [[ over $ for element access (no partial matching, works with variables)
inputs[["yvar"]]
result[["coeftable"]]
# NOT: inputs$yvar, result$coeftable
```

### Dependency Policy
Minimize Imports — avoid adding new dependencies unless truly necessary. Non-essential packages go in Suggests/Enhances. All heavy computation should happen in SQL, not R.

### SQL Design
Build modular, reusable SQL-generating helpers (e.g., `sql_weighted_mean()`, `sql_weighted_sum()`, `sql_count()`, `build_weighted_moment_terms()`) that compose into larger queries. Strategy functions (`execute_*_strategy()`) assemble these building blocks into full CTEs rather than writing bespoke SQL inline. This keeps the code DRY across strategies that share common patterns (e.g., weighted vs unweighted moment computation).

### Line Length
No strict limit, but keep lines readable. Break long SQL strings with `paste0()` or `glue()`.

## Architecture

### Execution Flow (`dbreg()`)
1. `dbreg()` normalises its arguments and copies them into a new `inputs` **environment** (`new.env()` + `env2env()`), then `process_dbreg_inputs(inputs)` validates args, sets up the DB connection, parses the formula, filters missings and validates weights, writing derived fields back into `inputs` in place.
2. `choose_strategy(inputs)` — auto-selection logic. Mutates `inputs[["is_balanced"]]` and `inputs[["compression_ratio_est"]]` in place (reference semantics).
3. `execute_*_strategy(inputs)` — one of: `moments`, `demean`, `mundlak`, `compress`
4. `finalize_dbreg_result(result, inputs, chosen_strategy)` — set class, attach metadata

### The `inputs` Environment
`inputs` is an environment (created up front in `dbreg()`/`dbivreg()`) that flows through the pipeline. Setup functions record the connection in `inputs` as soon as it exists, so the caller's `on.exit(release_db_setup(...))` also cleans up when validation fails. Using an environment rather than a list gives reference semantics — functions like `choose_strategy()` can store computed metadata (e.g., `is_balanced`, `compression_ratio_est`) that downstream functions read without needing return-value plumbing.

Functions that read several fields pull them into local scope once with `env2env()` (as in tinyplot's `settings`), then use bare names:

```r
execute_moments_strategy = function(inputs) {
  env2env(inputs, environment(), c("conn", "yvar", "weights", "sql_only"))
  ...
}
```

Lower-level helpers take `inputs` too (e.g. `dbreg_alternating_projections(inputs, x_sql, x_names)`, `nested_fe_df(inputs)`), keeping explicit arguments only for values that vary per call (residuals, score columns, a non-default `from_statement`). Keep `inputs[["field"]]` for one-off reads, fields a function reassigns locally, and writes back to `inputs` (e.g. `is_balanced`). Never use `<<-`.

`dbreg()`, `dbivreg()` and `dbbinsreg()` all follow this pattern.

### Estimation Layers
Moments-based strategies (and `dbivreg()`) compose five shared layers. Reuse them rather than writing strategy-specific SQL or linear algebra:

1. **Frame** (`frames.R`) — `build_base_frame()` / `build_demean_frame()` return the CTE both passes read from, plus a name→column map (`cols`), `weights_expr`, `intercept`, `cast`, count `prefix_terms`, and `cleanup()` for AP temp tables.
2. **Moments** — `frame_moments_sql()` aggregates via `build_weighted_moment_terms()`; `cross_moments()` reads the result into one symmetric matrix `S` over `(Intercept)`, regressors and `y`. Any sub-regression is indexing: `S[x, x]`, `S[x, y]`.
3. **Estimator** (`fit.R`) — `fit_ols()` / `fit_2sls()` work only on `S` and return a common shape: `coef`, `bread`, `rss`, `x`, `scores`, `project_meat()`. A new estimator (e.g. GMM) is another `fit_*()`.
4. **Score pass** — `fit_meat()` → `compute_score_meat()` pushes coefficients back to SQL (`sql_linear_predictor()`) to form residuals and aggregate scores for HC1/cluster. Residual regressors and score columns can differ (IV: X vs Z).
5. **Vcov** — `compute_vcov(XtX_inv = fit$bread, meat = fit$project_meat(meat))`.

The compress strategies build `S` and the meat in R from the compressed cells instead (exact, since regressors are constant within cells).

### Acceleration Strategies
1. **compress** — GROUP BY compression → frequency-weighted least squares (Wong et al. 2021). Best when regressors are discrete.
2. **moments** — Direct sufficient statistics (X'X, X'y) via SQL aggregation. No FE. Single-row result.
3. **demean/within** — Demeaning for 1+ FE. Analytic for 1 FE or balanced 2-way panels; alternating projections (AP) for weighted, unbalanced, or 3+ FE cases.
4. **mundlak** — True Mundlak/CRE estimator: Y ~ X + group means of X. Any number of FE, any panel structure. Single-pass (no iteration).

### Auto Strategy Logic
- No FE + (continuous vars OR poor compression) → `"moments"`
- 1 FE + poor compression → `"demean"`
- 2 FE + poor compression + balanced → `"demean"` (analytic double demeaning)
- 2 FE + poor compression + unbalanced → `"demean"` (alternating projections)
- 3+ FE + poor compression → `"demean"` (alternating projections)
- Otherwise → `"compress"`

### Variance-Covariance Computation
- `compute_vcov()` — dispatches on vcov_type (iid/hc1/cluster) × strategy
- `compute_score_meat()` — HC1/cluster meat via a SQL score pass (residuals from `sql_linear_predictor()`; score columns can differ from regressors, e.g. instruments for IV)
- `compute_meat_cluster_compress()` — special cluster meat for compress strategy

### Alternating Projections (AP)
Iterative demeaning for exact FE when analytic demeaning isn't available. Uses temp tables with a join-based approach (GROUP BY to compute means, then JOIN to subtract). Controlled by:
- `options(dbreg.ap_max_iter = 100)` — max iterations
- `options(dbreg.ap_tol = 1e-10)` — convergence tolerance

Note: AP has a convergence floor around ~1e-8 on larger datasets due to floating-point noise from repeated temp-table serialization. Use `tol = 1e-7` for large data, or consider `strategy = "mundlak"` as a single-pass alternative.

### SQL Backend Support
Backend detection via `detect_backend()` in `R/utils.R`. Supported dialects:
- DuckDB (default, with reservoir sampling)
- SQL Server (COUNT_BIG, TOP N, #temp tables, NEWID)
- PostgreSQL, SQLite, MySQL/MariaDB
- Spark, Athena/Presto/Trino (REAL instead of FLOAT for Athena)

## Testing

Tests use the **tinytest** framework in `inst/tinytest/`.

**Philosophy:** test *values* not forms. Always benchmark against known results from `fixest::feols()` (or `lm()`) where possible. Tolerances are explicit (e.g., `tol_iid = 1e-6`, `tol_robust = 1e-5`). Use `fixef.rm = "none"` in fixest comparisons to avoid singleton-removal discrepancies.

NYC taxi tests are gated behind `DBREG_TEST_NYC=TRUE` envvar.

### Running Tests
```r
pkgload::load_all()
tinytest::test_all(".")                                    # all tests
tinytest::run_test_file("inst/tinytest/test_trade.R")      # single file
```

### NYC Taxi Data (180M rows, 8.5GB)
```bash
mkdir -p nyc-taxi/year=2012
aws s3 cp s3://arrow-datasets/nyc-taxi/year=2012 nyc-taxi/year=2012 --recursive --no-sign-request
```

## Development Workflow

### Makefile Commands
```bash
make test        # Run tinytest::test_all()
make check       # Full R CMD check (--no-manual)
make document    # Regenerate man pages (devtools::document)
make install     # Install package locally
make website     # Build docs website (altdoc::render_docs)
```

### Documentation
- Man pages: roxygen2 (`devtools::document()`)
- Website: `altdoc::render_docs(freeze = TRUE)` — CI rebuilds on merge to main
- Vignette: `vignettes/intro.qmd`

## Contributing

- All code should be contributed via pull requests — don't push directly to main.
- PRs automatically trigger CI (`R CMD check` on Ubuntu, macOS, and Windows). Ensure CI passes before merging.
- Use of automatic GitHub code reviewers (e.g., tagging `@copilot`) is encouraged but not required.

## Common Pitfalls

- **AP convergence floor**: On large datasets, AP stalls around ~1e-8 due to temp-table float rounding. Use `options(dbreg.ap_tol = 1e-7)` or recommend `strategy = "mundlak"`.
- **Athena FLOAT gotcha**: AWS Athena doesn't support `CAST(... AS FLOAT)` — use `REAL` instead. Handled by `gsub("FLOAT", "REAL", ...)` checks throughout.
- **fixest singleton removal**: When comparing dbreg results to fixest, use `fixef.rm = "none"` in `feols()` — otherwise fixest drops FE singletons and nobs won't match.
- **Column names with dots**: DuckDB can struggle with column names like `Solar.R` in certain SQL contexts (WHERE clauses). Prefer clean column names in test data.
- **`is_balanced` detection**: Balance check must verify both equal cell counts AND no missing cells (n_observed == n_fe1 × n_fe2).

## Links

- Docs: https://grantmcdermott.com/dbreg/
- Issues: https://github.com/grantmcdermott/dbreg/issues
- TODO: https://github.com/grantmcdermott/dbreg/issues/5
