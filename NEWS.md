# News

## dev version

New features

- Added QR decomposition fallback for regression calculations, for cases where
  the default Cholesky solver fails. (#7)
- Improved integration for running regressions on AWS Athena datasets via the
  **noctua** package/driver. (#8)
- The automatic strategy logic now considers the maximum size of the compressed 
  data, mostly to avoid cases where the `"compress"` strategy would return a
  (still) prohibitively large dataset. (#10)
- Enabled weights for Mundlak specification. (#13)

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
