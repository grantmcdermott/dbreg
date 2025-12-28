# News

## dev version

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
  set it globally (i.e., `options(dbreb.verbose = TRUE)`). (#33)
- Technically not a breaking change, since we currently support backwards
  compatibility, but several minor arguments have been renamed/superseded. (#34)
  - `query_only` -> `sql_only` (in `dbreg`)
  - `fes` -> `fe` (in `print.dbreg`, `coef.dbreg`, `confint.dbreg`, etc.)

New features

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
- Esimations now report some goodness-of-fit statistics like R2 and RMSE,
  powered by the (user-facing) `gof()` function. (#21) 
- Added support for various `*.dbreg` methods (#21, #30):
  - From **stats**: `coef()`, `confint()`, `predict()`, and `vcov()`.
  - From **broom**/**generics**: `tidy()` and `glance()`. These also enable
  post-processing operations like exporting results to coefficient tables via
  `modelsummary::msummary()`. Thanks to @HariharanJayashankar for the request in
  #20.

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
  <2 increase in total estimation time, which seems a reasonable tradeoff for
  heteroskedastic-robust SEs. (#27)

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
