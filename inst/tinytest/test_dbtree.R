library(dbreg)

# -----------------------------------------------------------------------------
# Core behavior on toy data
# -----------------------------------------------------------------------------

df = data.frame(
  y = c(1, 1, 1, 10, 10, 10),
  x = c(0, 0, 0, 1, 1, 1)
)

mod0 = dbtree(y ~ x, data = df, max_depth = 0, n_bins = 2)
pred0 = predict(mod0, df)
expect_true(all(abs(pred0 - mean(df$y)) < 1e-8))
expect_equal(sum(mod0$nodes$is_leaf), 1)
expect_true(any(mod0$nodes$reason == "max_depth"), info = "max_depth=0 stops at root")

mod1 = dbtree(y ~ x, data = df, max_depth = 1, n_bins = 2, min_split = 2, min_leaf = 1)
pred1 = predict(mod1, df)
expect_equal(length(unique(pred1)), 2)
expect_equal(length(pred1), nrow(df))
expect_true(sum(!mod1$nodes$is_leaf) >= 1)

# -----------------------------------------------------------------------------
# Stopping rules
# -----------------------------------------------------------------------------

mod_min_split = dbtree(y ~ x, data = df, max_depth = 2, n_bins = 2, min_split = 100, min_leaf = 1)
expect_equal(sum(mod_min_split$nodes$is_leaf), 1)
expect_true(any(mod_min_split$nodes$reason == "min_split"))

mod_min_leaf = dbtree(y ~ x, data = df, max_depth = 2, n_bins = 2, min_split = 2, min_leaf = 4)
expect_equal(sum(mod_min_leaf$nodes$is_leaf), 1)
expect_true(any(mod_min_leaf$nodes$reason == "min_leaf"))

mod_min_gain = dbtree(y ~ x, data = df, max_depth = 2, n_bins = 2, min_split = 2, min_leaf = 1, min_gain = 1e6)
expect_equal(sum(mod_min_gain$nodes$is_leaf), 1)
expect_true(any(mod_min_gain$nodes$reason == "no_split"))

# -----------------------------------------------------------------------------
# Split-construction branches
# -----------------------------------------------------------------------------

set.seed(77)
df_num = data.frame(
  x = rep(seq(0, 1, length.out = 30), each = 2)
)
df_num$y = ifelse(df_num$x > 0.55, 2, -1) + rnorm(nrow(df_num), sd = 0.1)

mod_global_quant = dbtree(
  y ~ x,
  data = df_num,
  max_depth = 1,
  n_bins = 8,
  min_split = 10,
  min_leaf = 3,
  bin_scope = "global",
  bin_method = "quantile"
)
expect_true(inherits(mod_global_quant, "dbtree"))
expect_true(sum(!mod_global_quant$nodes$is_leaf) >= 1)

mod_node_width = dbtree(
  y ~ x,
  data = df_num,
  max_depth = 1,
  n_bins = 8,
  min_split = 10,
  min_leaf = 3,
  bin_scope = "node",
  bin_method = "width"
)
expect_true(inherits(mod_node_width, "dbtree"))
expect_true(sum(!mod_node_width$nodes$is_leaf) >= 1)

# Categorical split
df_cat = data.frame(
  y = c(2, 2, 8, 8),
  g = factor(c("a", "a", "b", "b"))
)
mod_cat = dbtree(y ~ g, data = df_cat, max_depth = 1, n_bins = 2, min_split = 2, min_leaf = 1)
pred_cat = predict(mod_cat, df_cat)
expect_equal(length(unique(pred_cat)), 2)

# -----------------------------------------------------------------------------
# Weights and method utilities
# -----------------------------------------------------------------------------

df_w = data.frame(
  y = c(0, 0, 10),
  x = c(0, 0, 1),
  w = c(1, 1, 20)
)

mod_unw_root = dbtree(y ~ x, data = df_w, max_depth = 0, n_bins = 2)
mod_w_root = dbtree(y ~ x, data = df_w, weights = "w", max_depth = 0, n_bins = 2)

expect_true(
  abs(mod_w_root$nodes$prediction[1] - weighted.mean(df_w$y, df_w$w)) < 1e-8,
  info = "weighted root prediction matches weighted mean"
)
expect_true(
  abs(mod_w_root$nodes$prediction[1] - mod_unw_root$nodes$prediction[1]) > 1,
  info = "weighted and unweighted root predictions differ when weights are informative"
)

print_txt = capture.output(print(mod1))
expect_true(any(grepl("DB-native regression tree", print_txt)))
expect_true(any(grepl("Depth:", print_txt)))
expect_true(any(grepl("Constraints:", print_txt)))
expect_true(any(grepl("Leaf stop reasons:", print_txt)))

expect_error(
  predict(mod1, data.frame(z = df$x)),
  "missing required variables",
  info = "predict.dbtree errors on missing predictors"
)
expect_error(
  predict(mod1, data.frame(x = c(0, NA, 1))),
  "missing values",
  info = "predict.dbtree errors on NA predictors"
)

# Errors for unsupported features
expect_error(dbtree(y ~ x | g, data = df), "fixed effects")
expect_error(dbtree(y ~ x * g, data = df), "interaction")

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

con = DBI::dbConnect(duckdb::duckdb(), shutdown = TRUE)

DBI::dbExecute(con, sprintf("
  CREATE VIEW nyc_tree AS
  SELECT
    tip_amount,
    fare_amount,
    trip_distance,
    vendor_name
  FROM read_parquet('%s/month=1/*.parquet')
  WHERE trip_distance > 0 AND trip_distance < 20
    AND fare_amount > 0 AND fare_amount < 100
", nyc_path))

nyc_mod = dbtree(
  tip_amount ~ fare_amount + trip_distance + vendor_name,
  conn = con,
  table = "nyc_tree",
  max_depth = 2,
  n_bins = 16,
  min_split = 200,
  min_leaf = 100,
  max_cat = 20,
  bin_scope = "global",
  bin_method = "quantile"
)

expect_true(inherits(nyc_mod, "dbtree"), info = "dbtree returns dbtree object on NYC data")
expect_true(is.finite(nyc_mod$metrics$rmse), info = "dbtree RMSE is finite on NYC data")
expect_true(nrow(nyc_mod$nodes) >= 1, info = "dbtree returns node table on NYC data")

nyc_new = DBI::dbGetQuery(con, "SELECT fare_amount, trip_distance, vendor_name FROM nyc_tree LIMIT 200")
nyc_pred = predict(nyc_mod, nyc_new)
expect_equal(length(nyc_pred), nrow(nyc_new), info = "predict.dbtree returns one value per row on NYC data")
expect_true(all(is.finite(nyc_pred)), info = "predict.dbtree returns finite values on NYC data")

DBI::dbDisconnect(con)
rm(con)
