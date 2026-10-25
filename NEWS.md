# News

_This NEWS file is best viewed on our [website](https://grantmcdermott.com/dbreg/NEWS.html)._

## Development

New features

- Alternating projections on Spark/Databricks now use sequential SQL window
  functions (both unweighted and weighted) and batch three cycles per temporary
  table write, controlled by the new `dbreg.ap_method` and
  `dbreg.ap_batch_size` options.
- New `?dbreg-options` help page documenting all global options.
- Added `dbivreg()` for instrumental-variables (2SLS) regression on database
  backends. Uses `fixest`-style IV formulas, e.g. `y ~ x | d ~ z` or
  `y ~ x | fe1 + fe2 | d ~ z`, and supports multiple endogenous regressors,
  factors/interactions, weights, and iid, HC1, or clustered SEs (incl.
  `ssc`). Estimation is exact via the `"moments"`, `"demean"` (any number of
  FE), and `"compress"` (no FE) strategies. (#66)
- Added support for weighted regression via the new `weights` argument. Accepts
  a column name string and is compatible with all strategies and SE types (iid,
  HC1, clustered). (#12)
- The `"demean"` strategy now supports three or more fixed effects via
  alternating projections (AP). Previously this was limited to at most two FE.
  The `"auto"` heuristic will select `"demean"` (AP) for 3+ FE when compression
  is not viable. (#12)
- For unbalanced two-way panels, `strategy = "auto"` now routes to `"demean"`
  via alternating projections instead of erroring. Users no longer need to
  explicitly choose between `"compress"` and `"mundlak"` for these cases.
- Formulas now support `fixest`-style `i()` terms, e.g.
  `dbreg(y ~ i(period, treat, ref = 5) | id + period, ...)` for event studies,
  in `dbreg()` and `dbivreg()` and across all strategies. Supports `i(f)`,
  `i(f, x)` with numeric or logical `x`, and the `ref` and `keep` arguments.
  Coefficients are named as in `fixest` (e.g. `period::1:treat`). (#82)

Bug fixes

- Alternating projections no longer fail with an SQL error when a fixed-effect
  or cluster variable also appears as a regressor. Invalid `dbreg.ap_max_iter`
  and `dbreg.ap_tol` options now produce a clear error.
- Fixed a typo in `<dbreg.object>$coeftable` column names that was obscuring
  some downstream functions, e.g. stars in `modelsummary::modelsummary`. (#62) 
- Fixed `dbreg_is_balanced_panel()` to correctly detect unbalanced panels with
  missing cells (previously only checked for unequal cell counts).
- The CR1 small-sample correction for clustered SEs now uses
  `(G/(G-1)) * ((N-1)/(N-K))`, matching `fixest`, instead of
  `(G/(G-1)) * (N/(N-K))`. Similarly, `ssc = "nested"` now excludes `G-1` (not
  `G`) parameters for each fixed effect nested within the cluster variable. In
  practice, clustered SEs shift by a relative factor of roughly `1/(2N)` under
  the default `ssc = "full"` (and are essentially unchanged under `"nested"`),
  which is negligible for large datasets. But it does mean that `dbreg` now
  matches `fixest` to numerical precision in small samples too. (#66)
- With `drop_missings = TRUE` (the default), rows with a missing cluster
  variable are now dropped, as in `fixest`. Previously they were kept and
  treated as their own cluster. (#66)
- Clustered SEs for the `"compress"` strategy no longer stall when there are
  many grouping columns. (#66)
- Transformed formula terms, e.g. `log(y) ~ x` or `y ~ log(x) + I(x^2)`, now
  throw an informative error. Previously these were silently reduced to their
  underlying variables (e.g., regressing on `x` instead of `log(x)`). Please
  create such variables in the data (or a database view) first. (#66)
- Requests to omit the intercept (`y ~ 0 + x` or `y ~ x - 1`) are now honoured
  for models without fixed effects, in both `dbreg()` and `dbivreg()`.
  Previously an intercept was always added. As in `lm()`, the first factor
  keeps all of its levels and R2 is uncentred. (#68, #73)
- `predict()` now rebuilds the design matrix from the model formula, fixing
  predictions for models with interactions (e.g., `"mundlak"`), factors, or no
  intercept. (#67, #73)
- Variable names that aren't valid SQL identifiers (e.g., `Sepal.Length`,
  names with spaces, or reserved words) and factor levels with spaces or other
  special characters now work in `dbreg()`, `dbivreg()` and `dbbinsreg()`.
  Results use the original names. (#58)
- Plain factor regressors (without interactions) now work for the
  `"moments"`, `"demean"` and `"mundlak"` strategies, and factor levels are
  read from all rows rather than the first 100,000. (#76)
- `predict()` now errors for `"demean"` models with more than two fixed
  effects. Previously it silently ignored the third and later fixed effects,
  returning incorrect predictions. (#66)
- `sql_model_matrix()` (and thus interaction/factor terms) now generates
  backend-portable schema and factor-level queries, fixing failures on SQL
  Server and other non-DuckDB backends. (#66)
- Sufficient statistics and HC1/clustered scores are now computed in double
  precision on all backends. Previously, they were cast to `FLOAT`, which is
  single precision on DuckDB, MySQL, Spark and Athena, limiting the
  `"demean"` and `"mundlak"` strategies (and robust SEs) to ~1e-8 accuracy. (#78)
- The `"mundlak"` strategy now includes group means of every regressor column,
  incl. factor dummies and interactions. Previously only raw numeric variables
  were averaged, so models with factors or interactions didn't match the
  equivalent fixed effects estimates. Time-invariant regressors now keep their
  coefficient (dropping their collinear group mean) instead of erroring. (#77)
- Clustered SEs for the `"compress"` strategy no longer error for models with
  interaction terms.

Internal

- Update maintainer email.
- Added a `CLAUDE.md` context file for sharper AI-assisted development, as well
  as a `Makefile` for automating dev workflows. (#64)
- Refactored and modularized the codebase to make it more efficient and safer.
  (#65)
- Much faster cluster-robust SEs for the `"compress"` strategy with many
  clusters. (#72)
- Internal arguments now flow through a single `inputs` environment (filled in
  place via `env2env()`), simplifying function signatures. (#74)
- Estimation internals now share a common set of building blocks (estimation
  frames, a cross-moment matrix, estimator fits, and a SQL score pass for
  robust/clustered meat matrices), which `dbreg()` and `dbivreg()` both use.
  This removes a lot of duplicated code and should make it easier to add new
  estimators. (#66)

## dbreg 0.0.3

_**dbreg** v0.0.3 is a big release, containing many new features and some
important breaking changes. We have closed out most of items on our
[TODO](https://github.com/grantmcdermott/dbreg/issues/5) list and this will
probably be our final "beta" release before submitting to CRAN._

Website

- We finally have a package website:
[grantmcdermott.com/dbreg](https://grantmcdermott.com/dbreg/index.html) 🎉

Breaking changes

- The behaviour of the `"mundlak"` strategy has been changed. (#24)
  - The old `"mundlak"` strategy is remapped to the new `"demean"` (alias
    `"within"`) strategy, to better reflect the fact that this strategy invokes
    a (double) demeaning transformation. Users who want the old behaviour should
    thus use `"demean"` instead of `"mundlak"`.
  - Simultaneously, we now provide a revised `"mundlak"` strategy that
    implements a "true" Mundlak/CRE estimator; see New features below.
- For estimations with two fixed effects on _unbalanced_ panels,
  `strategy="auto"` now errors when the compression limits are exceeded. It does
  this to avoid silently selecting a different estimand (i.e., Mundlak/CRE
  instead of TWFE). For these ambiguous cases, users will now be prompted to
  explicitly choose `"compress"` (with higher limits) or `"mundlak"` (different
  model and thus potentially different coefficients). (#24)
- The default `verbose` behaviour is changed to `FALSE`. Users can revert to
  the old behaviour for a single call (i.e., `dbreg(..., verbose = TRUE)`), or
  set it globally (i.e., `options(dbreg.verbose = TRUE)`). (#33)
- Technically not a breaking change, since we currently support backwards
  compatibility, but several minor arguments have been renamed/superseded. (#34)
  - `query_only` -> `sql_only` (in `dbreg`)
  - `fes` -> `fe` (in `print.dbreg`, `coef.dbreg`, `confint.dbreg`, etc.)

Major new features

- We have added new/revised acceleration strategies. (#24)
  - The `"demean"` (alias `"within"`) strategy implements a (double) demeaning
    transformation and is particularly suited to (balanced) panels with one or
    two fixed effects. Note the underlying query is the same as the _old_
    `"mundlak"` strategy, which was somewhat erroneously named. Speaking of
    which...
  - The revised `"mundlak"` strategy now implements a "true" Mundlak/CRE
    (correlated random effects) estimator by regressing Y on X plus group
    means of X. Unlike the `"demean"` strategy (above), this revised `"mundlak"`
    model obtains consistent coefficients regardless of panel structure (incl.
    unbalanced panels) and supports any number of fixed effects. However, users
    should note that Mundlak/CRE is a different model from "vanilla" fixed
    effects---albeit asymptotically equivalent under certain assumptions---and
    may obtain different coefficients as a result.
  - Please consult the expanded `Acceleration Strategies` section in the
    `?dbreg` helpfile for technical details.
- Add support for clustered standard errors. Follows the `fixest` API:

  ```r
  dbreg(..., vcov = ~cluster_var)
  ```

  Please note that these clustered SEs are computed _analytically_ (not
  bootstrapped) and should thus add minimal overhead to your regressions. See
  the updated README for examples. (#29)
- Added support for binscatter regressions on database backends via the
  `dbbinsreg()` function. This function attempts to mimic the main API of the
  **binsreg** package (modulo some simplifications), but should be much faster
  on big datasets. (#32)
- Added support for interaction terms in formulas (e.g., `y ~ x1 * x2 | fe`).
  Interactions are expanded to SQL via the new (user-facing)
  `sql_model_matrix()` function and work with all acceleration strategies.
  Collinear terms (e.g., treatment main effects absorbed by unit FEs in DiD
  models) are automatically detected and dropped. (#48)

  - Note that interactions support also enables basic event-study estimations,
    although the reference period needs to be specified manually, e.g.:

    ```r
    data("base_did", package = "fixest")
    did = transform(base_did, period = factor(period))
    did$period = relevel(did$period, ref = "5")
    dbreg(y ~ x1 + treat:period | id + period, data = did)
    ```

Other new features

- The `"auto"` strategy logic now considers a `compress_nmax` threshold, which
  governs the maximum allowable size of the compressed data object (default
  threshold = 1 million rows). This additional guardrail is intended to avoid
  cases where the `"compress"` strategy satisfies the `compress_ratio`
  threshold, but could still return a prohibitively large dataset. The most
  common example would be querying a massive dataset on a remote database, where
  network latency makes data I/O transfer expensive, even though we've achieved
  good compression relative to the original data size. (#10)
  - Aside: Improved documentation and messaging (when `verbose = TRUE`) should
    also help users understand the `"auto"` strategy decision tree.
- Enabled weights for double demean (within) specification. (#13)
- Estimations now report some goodness-of-fit statistics like R2 and RMSE,
  powered by the (user-facing) `gof()` function. (#21) 
- Added support for various `*.dbreg` methods (#21, #30):
  - From **stats**: `coef()`, `confint()`, `predict()`, and `vcov()`.
  - From **broom**/**generics**: `tidy()` and `glance()`. These also enable
  post-processing operations like exporting results to coefficient tables via
  `modelsummary::msummary()`. Thanks to @HariharanJayashankar for the request in
  #20.
- Better documentation. (#28, #36, #43, and various other PRs)

Bug fixes

- Added QR decomposition fallback for regression calculations, for cases where
  the default Cholesky solver fails. (#7)
- Improved integration for running regressions on AWS Athena datasets via the
  **noctua** package/driver. (#8)
- Automatically drop incomplete cases (i.e., missing values) prior to any
  aggregation steps, avoiding mismatched matrices during estimation. (#19)
- User-specified `compress_ratio` values should now bind in all cases.
  Previously, these could sometimes be silently ignored due to internal
  overrides. Also, clarify in the argument documentation that the default
  (automatic) `compress_ratio` threshold can vary based on heuristics related to
  model structure. (#25)
- Correctly estimate HC1 standard errors for the `"moments"`, `"demean"`, and
  `"mundlak"` strategies. While the old analytic HC1 approach worked (and still
  does) for the `"compress"` case, it led to misleading SEs for these other
  strategies. The fix does impose some additional computational overhead, since
  it requires a second pass over the data to calculate the individual errors
  and "meat" of the sandwich matrix. But testing suggests that this leads to a
  <2x increase in total estimation time, which seems a reasonable tradeoff for
  heteroskedastic-robust SEs. (#27)
- Fixed a bug for in-memory `dbreg(..., data = <data>)` cases, where factor
  variables in X could cause the `"compress"` strategy to fail. (#41) 

Internals

- Added unit testing framework using **tinytest**. (#16)
- Added GitHub Actions CI. (#18)

## dbreg 0.0.2

**IMPORTANT BREAKING CHANGE:**

The package has been renamed to **dbreg** to better reflect the fact that it
supports multiple database backends. (#4)

Other breaking changes

- The default `vcov` is now "iid". (#2 @grantmcdermott)

New features 

- The new `dbreg(..., strategy = <strategy>)` argument allows users to choose
  between different acceleration strategies for efficient computation of the
  regression coefficients and standard errors. This includes `"compress"` (the
  old default), as well as `"mundlak"` or `"moments"`. The latter two strategies 
  are newly introduced in **dbreg** v0.0.2 and offer important advantages in the 
  case of true panel data. If an explicit strategy is not provided by the user, 
  then `dbreg()` will invoke some internal heuristics to determine the optimal
  strategy based on the size and structure of the data. (#2 @jamesbrandecon
  and @grantmcdermott)

Project

- @jamesbrandecon has joined the project as a core contributor.

## duckreg 0.0.1

* Initial GitHub release.
