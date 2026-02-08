library(dbreg)

set.seed(123)
g = 20
n_per_g = 10
n = g * n_per_g

group = rep(seq_len(g), each = n_per_g)
d1 = rnorm(n)
d2 = rnorm(n)
alpha_g = rnorm(g)

y = 1 + 2 * d1 - 1.5 * d2 + alpha_g[group] + rnorm(n, sd = 0.5)

df = data.frame(
  y = y,
  d1 = d1,
  d2 = d2,
  g = factor(group)
)

# Explicit strata path with two treatments
mod = dbdml(
  y ~ d1 + d2 | g,
  data = df,
  treat = c("d1", "d2"),
  method = "strata",
  vcov = "none"
)
lm_mod = lm(y ~ d1 + d2 + g, data = df)

expect_equal(
  mod$coeftable[c("d1", "d2"), "estimate"],
  coef(lm_mod)[c("d1", "d2")],
  tolerance = 1e-6,
  info = "dbdml strata coefficients match FE regression"
)

# S3 methods for dbdml
expect_equal(
  coef(mod),
  mod$coeftable[, "estimate"],
  info = "coef.dbdml returns coeftable estimates"
)
expect_equal(
  dim(vcov(mod)),
  c(length(mod$treatments), length(mod$treatments)),
  info = "vcov.dbdml has expected dimensions"
)
ci_mod = confint(mod)
expect_equal(
  dim(ci_mod),
  c(length(mod$treatments), 2),
  info = "confint.dbdml returns 2-column intervals"
)

print_strata_txt = capture.output(print(mod))
expect_true(any(grepl("Strata LOO", print_strata_txt)), info = "print.dbdml strata header is present")

mod_auto_strata = dbdml(
  y ~ d1 + d2 | g,
  data = df,
  treat = c("d1", "d2"),
  method = "auto",
  vcov = "hc1"
)
expect_true(
  identical(mod_auto_strata$method, "strata"),
  info = "dbdml auto dispatch chooses strata for discrete controls"
)
expect_true(all(is.finite(mod_auto_strata$coeftable[, "std.error"])))

# Bootstrap path for strata
mod_boot = dbdml(
  y ~ d1 | g,
  data = df,
  treat = "d1",
  method = "strata",
  vcov = "bootstrap",
  n_bootstraps = 30,
  seed = 1
)
expect_equal(
  attr(mod_boot$vcov, "type"),
  "bootstrap",
  info = "dbdml strata bootstrap stores vcov type"
)
expect_true(
  is.finite(mod_boot$coeftable["d1", "std.error"]),
  info = "dbdml strata bootstrap returns finite SE"
)

# n_bootstraps forces bootstrap vcov in strata mode
mod_boot_forced = suppressWarnings(
  dbdml(
    y ~ d1 | g,
    data = df,
    treat = "d1",
    method = "strata",
    vcov = "hc1",
    n_bootstraps = 10,
    seed = 2
  )
)
expect_equal(
  mod_boot_forced$vcov_type,
  "bootstrap",
  info = "n_bootstraps > 0 forces bootstrap vcov"
)

# sql_only / data_only paths for strata
sql_txt = dbdml(
  y ~ d1 + d2 | g,
  data = df,
  treat = c("d1", "d2"),
  method = "strata",
  sql_only = TRUE
)
expect_true(
  is.character(sql_txt) && length(sql_txt) == 1,
  info = "sql_only returns a SQL string"
)
expect_true(
  grepl("GROUP BY", sql_txt),
  info = "strata sql_only includes GROUP BY"
)

compressed = dbdml(
  y ~ d1 + d2 | g,
  data = df,
  treat = c("d1", "d2"),
  method = "strata",
  data_only = TRUE
)
expect_true(
  is.data.frame(compressed),
  info = "data_only returns compressed data frame"
)
expect_true(
  all(c("n_g", "sum_y", "sum_d1", "sum_d2") %in% names(compressed)),
  info = "compressed data includes key sufficient statistics"
)

# Clustered inference is not available for strata
expect_error(
  dbdml(
    y ~ d1 + d2 | g,
    data = df,
    treat = "d1",
    method = "strata",
    cluster = ~g
  ),
  "Clustered inference",
  info = "strata method rejects clustered inference"
)

# PLM path with continuous controls
set.seed(456)
n2 = 1200
x = rnorm(n2)
d = 0.6 * x + rnorm(n2)
y2 = 2 * d + sin(x) + rnorm(n2, sd = 0.5)
df2 = data.frame(y = y2, d = d, x = x)

mod_plm = dbdml(
  y ~ d + x,
  data = df2,
  treat = "d",
  method = "plm",
  folds = 5,
  ridge = 1,
  vcov = "hc1"
)

expect_true(
  is.finite(mod_plm$coeftable["d", "estimate"]),
  info = "dbdml plm estimate is finite"
)
expect_equal(
  mod_plm$coeftable["d", "estimate"],
  2,
  tolerance = 0.25,
  info = "dbdml plm recovers treatment effect in a smooth-control DGP"
)
expect_equal(
  mod_plm$folds,
  5L,
  info = "dbdml plm stores requested number of folds"
)

print_plm_txt = capture.output(print(mod_plm))
expect_true(any(grepl("Cross-fitted partially linear model", print_plm_txt)), info = "print.dbdml plm header is present")
expect_true(any(grepl("Nuisance strategy: moments", print_plm_txt)), info = "print.dbdml reports nuisance strategy for PLM")

# Cross-fitting reproducibility for fixed data and fold assignments
mod_plm_repeat = dbdml(
  y ~ d + x,
  data = df2,
  treat = "d",
  method = "plm",
  folds = 5,
  ridge = 1,
  vcov = "hc1"
)
expect_equal(
  mod_plm_repeat$coeftable["d", "estimate"],
  mod_plm$coeftable["d", "estimate"],
  tolerance = 1e-10,
  info = "PLM cross-fitting is deterministic for fixed data and folds"
)

# PLM cluster path and ridge list parsing
set.seed(789)
df2$cluster_id = sample(1:50, n2, replace = TRUE)
mod_plm_cluster = dbdml(
  y ~ d + x,
  data = df2,
  treat = "d",
  method = "plm",
  cluster = ~cluster_id,
  ridge = list(y = 0.5, d = 2),
  folds = 4
)
expect_equal(
  mod_plm_cluster$vcov_type,
  "cluster",
  info = "dbdml plm uses cluster vcov when requested"
)
expect_true(
  is.finite(mod_plm_cluster$coeftable["d", "std.error"]),
  info = "dbdml plm cluster SE is finite"
)

# Auto with cluster should choose PLM
mod_auto_cluster = dbdml(
  y ~ d + x,
  data = df2,
  treat = "d",
  method = "auto",
  cluster = ~cluster_id,
  folds = 3
)
expect_equal(
  mod_auto_cluster$method,
  "plm",
  info = "auto chooses PLM when clustered inference is requested"
)

# PLM without controls falls back to second-stage only and no folds
mod_plm_nocontrols = dbdml(
  y ~ d,
  data = df2,
  treat = "d",
  method = "plm",
  folds = 3,
  ridge = 1,
  vcov = "hc1"
)
expect_true(
  is.na(mod_plm_nocontrols$folds),
  info = "plm without controls stores folds as NA"
)

mod_auto_plm = dbdml(
  y ~ d + x,
  data = df2,
  treat = "d",
  method = "auto",
  folds = 5,
  ridge = 1,
  vcov = "hc1"
)
expect_true(
  identical(mod_auto_plm$method, "plm"),
  info = "dbdml auto dispatch chooses plm for continuous controls"
)

# PLM with mixed controls (numeric + factor) uses compress nuisance fits
set.seed(246)
n3 = 1600
x3 = rnorm(n3)
g3 = factor(sample(letters[1:6], n3, replace = TRUE))
d3 = 0.7 * x3 + as.numeric(g3) / 4 + rnorm(n3)
y3 = 1.8 * d3 + 0.9 * x3 + as.numeric(g3) / 5 + rnorm(n3, sd = 0.7)
df3 = data.frame(y = y3, d = d3, x = x3, g = g3)

mod_plm_mixed = dbdml(
  y ~ d + x + g,
  data = df3,
  treat = "d",
  method = "plm",
  folds = 4,
  ridge = 1,
  vcov = "hc1"
)
expect_true(
  is.finite(mod_plm_mixed$coeftable["d", "estimate"]),
  info = "PLM mixed-controls estimate is finite"
)
expect_equal(
  mod_plm_mixed$coeftable["d", "estimate"],
  1.8,
  tolerance = 0.3,
  info = "PLM mixed-controls estimate is near truth"
)
expect_equal(
  mod_plm_mixed$nuisance_strategy,
  "compress",
  info = "PLM switches nuisance fits to compress with factor controls"
)
mixed_print_txt = capture.output(print(mod_plm_mixed))
expect_true(
  any(grepl("Nuisance strategy: compress", mixed_print_txt)),
  info = "print.dbdml reports compress nuisance strategy for mixed controls"
)

mod_auto_mixed = dbdml(
  y ~ d + x + g,
  data = df3,
  treat = "d",
  method = "auto",
  folds = 4,
  ridge = 1,
  vcov = "hc1"
)
expect_equal(
  mod_auto_mixed$method,
  "plm",
  info = "auto chooses PLM for mixed controls"
)
expect_equal(
  mod_auto_mixed$nuisance_strategy,
  "compress",
  info = "auto-PLM with mixed controls uses compress nuisance strategy"
)

# -----------------------------------------------------------------------------
# NYC Taxi tests (large data smoke tests)
# -----------------------------------------------------------------------------

if (!tolower(Sys.getenv("DBREG_TEST_NYC")) %in% c("true", "1")) {
  exit_file("Run `Sys.setenv(DBREG_TEST_NYC = 'TRUE')` to enable NYC taxi tests")
}

nyc_path = here::here("nyc-taxi/year=2012")
if (!dir.exists(nyc_path)) {
  exit_file("NYC taxi data not found at nyc-taxi/year=2012")
}

# Create a DuckDB connection with a subset of NYC data
con = DBI::dbConnect(duckdb::duckdb(), shutdown = TRUE)

DBI::dbExecute(con, sprintf("
  CREATE VIEW nyc_dml AS
  SELECT
    tip_amount,
    fare_amount,
    trip_distance,
    passenger_count,
    vendor_name,
    month
  FROM read_parquet('%s/../**/*.parquet')
  WHERE year = 2012 AND CAST(month AS INTEGER) <= 2
", nyc_path))

# Test dbdml strata method with FE on NYC data
nyc_strata = dbdml(
  tip_amount ~ fare_amount + trip_distance | month + vendor_name,
  conn = con,
  table = "nyc_dml",
  treat = "fare_amount",
  method = "strata",
  vcov = "hc1"
)

expect_true(
  inherits(nyc_strata, "dbdml"),
  info = "dbdml strata returns dbdml object on NYC data"
)
expect_true(
  is.finite(nyc_strata$coeftable["fare_amount", "estimate"]),
  info = "dbdml strata estimate is finite on NYC data"
)
expect_true(
  nyc_strata$coeftable["fare_amount", "estimate"] > 0,
  info = "dbdml strata finds positive fare->tip effect"
)

# Test dbdml PLM method on NYC data (continuous controls)
nyc_plm = dbdml(
  tip_amount ~ fare_amount + trip_distance,
  conn = con,
  table = "nyc_dml",
  treat = "fare_amount",
  method = "plm",
  folds = 3,
  ridge = 1,
  vcov = "hc1"
)

expect_true(
  inherits(nyc_plm, "dbdml"),
  info = "dbdml plm returns dbdml object on NYC data"
)
expect_true(
  is.finite(nyc_plm$coeftable["fare_amount", "estimate"]),
  info = "dbdml plm estimate is finite on NYC data"
)

# Test auto dispatch on NYC data
nyc_auto = dbdml(
  tip_amount ~ fare_amount + trip_distance | vendor_name,
  conn = con,
  table = "nyc_dml",
  treat = "fare_amount",
  method = "auto",
  vcov = "hc1"
)

expect_true(
  nyc_auto$method %in% c("strata", "plm"),
  info = "dbdml auto dispatch selects a valid method on NYC data"
)

DBI::dbDisconnect(con)
rm(con)
