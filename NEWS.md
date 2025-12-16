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
  this to avoid silently selecting a different estimand (e.g, TWFE vs
  Mundlak/CRE). For these ambiguous cases, users will now be prompted to
  explicitly choose `"compress"` (with higher limits) or `"mundlak"`
  (CRE, different model). (#24)

New features

- We have added new/revised acceleration strategies. (#24)
  - The `"demean"` (alias `"within"`) strategy implements a (double) demeaning
    transformation and is particularly suited to (balanced) panels with one or
    two fixed effects. Note the underlying query is the same as the _old_
    `"mundlak"` strategy, which was somewhat erroneously named. Speaking of
    which...
  - The revised `"mundlak"` strategy now implements a "true" Mundlak/CRE
    (correlated random effects) estimator by regressesing Y on X plus group
    means of X. Unlike the `"demean"` strategy (above), this revised `"mundlak"`
    model obtains consistent coefficients regardless of panel structure (incl.
    unbalanced panels) and supports any number of fixed effects. However, users
    should note that Mundlak/CRE is a different model from "vanila" fixed
    effects---albeit asymptotically equivalent under certain assumptions---and
    may obtain different coefficients as a result.
  - Please consult the expanded `Acceleration Strategies` section in the
    `?dbreg` helpfile for technical details. (#24)
- Added QR decomposition fallback for regression calculations, for cases where
  the default Cholesky solver fails. (#7)
- Improved integration for running regressions on AWS Athena datasets via the
  **noctua** package/driver. (#8)
- The automatic strategy logic now considers the maximum size of the compressed 
  data, mostly to avoid cases where the `"compress"` strategy would return a
  (still) prohibitively large dataset. (#10)
- Enabled weights for double demean (within) specification. (#13)
- Esimations now report some goodness-of-fit statistics like R2 and RMSE,
  powered by the (user-facing) `gof()` function. (#21) 
- Added support for various `*.dbreg` methods (#21):
  - From **stats**: `confint()` and `vcov()`.
  - From **broom**/**generics**: `tidy()` and `glance()`. These also enable
  post-processing operations like exporting results to coefficient tables via
  `modelsummary::msummary()`. Thanks to @HariharanJayashankar for the request in
  #20.

Bug fixes

- Automatically drop incomplete cases (i.e., missing values) prior to any
  aggregation steps, avoiding mismatched matrices during estimation. (#19)

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
