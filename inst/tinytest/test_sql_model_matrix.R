library(dbreg)
library(DBI)
library(duckdb)

# Setup test data
con = dbConnect(duckdb())
test_df = data.frame(
  y = 1:6,
  x1 = c(1, 2, 3, 1, 2, 3),
  x2 = c("a", "a", "b", "b", "c", "c"),
  x3 = c(10, 20, 30, 40, 50, 60)
)
duckdb_register(con, "test", test_df)

#
## Test expand = "all" ----

res = sql_model_matrix(~ x1 + x2, con, "test", expand = "all")

expect_equal(res$col_names[1], "x1", info = "numeric passes through")
expect_equal(res$col_names[2:3], c("x2b", "x2c"), info = "factor one-hot encoded (drops ref)")
expect_equal(res$select_exprs[1], "x1", info = "numeric SQL is column name")
expect_true(grepl("CASE WHEN", res$select_exprs[2]), info = "factor SQL uses CASE WHEN")

#
## Test interaction ----

res = sql_model_matrix(~ x1:x2, con, "test", expand = "all")

# No main effect for x1, so all factor levels of x2 retained
expect_equal(res$col_names, c("x1_x_x2a", "x1_x_x2b", "x1_x_x2c"), info = "interaction names")
expect_true(all(grepl("\\*", res$select_exprs)), info = "interaction SQL uses multiplication")

#
## Test expand = "interactions" ----

res = sql_model_matrix(~ x1 + x2 + x1:x2, con, "test", expand = "interactions")

# Main effects: x1 numeric, x2 factor kept as-is
expect_equal(res$col_names[1], "x1", info = "numeric main effect")
expect_equal(res$col_names[2], "x2", info = "factor main effect NOT expanded")
expect_equal(res$select_exprs[2], "x2", info = "factor SQL is column name when not expanded")

# Interaction: factor IS expanded
expect_equal(res$col_names[3:4], c("x1_x_x2b", "x1_x_x2c"), info = "interaction expands factor")

#
## Test factor levels stored ----

expect_equal(res$factor_levels$x2, c("a", "b", "c"), info = "factor levels captured")

#
## Test numeric × numeric interaction ----
  
res = sql_model_matrix(~ x1:x3, con, "test", expand = "all")

expect_equal(res$col_names, "x1_x_x3", info = "numeric × numeric interaction name")
expect_equal(res$select_exprs, "(x1) * (x3)", info = "numeric × numeric SQL")

#
## Test SQL-safe aliases for factor levels with spaces ----

duckdb_register(con, "test_space", data.frame(x1 = 1:4, g = c("a", "b c", "a", "b c")))
mm_space = sql_model_matrix(~ x1 + g, con, "test_space")
expect_equal(mm_space$col_names, c("x1", "gb c"), info = "col_names keep the level text")
expect_equal(mm_space$aliases, c("x1", "gb_c"), info = "aliases are SQL-safe")

# Generated aliases don't collide with existing variable names
duckdb_register(con, "test_clash", data.frame(gb_c = 1:4, g = c("a", "b c", "a", "b c")))
mm_clash = sql_model_matrix(~ gb_c + g, con, "test_clash")
expect_false(anyDuplicated(mm_clash$aliases) > 0, info = "factor aliases avoid variable names")

## Cleanup ----

dbDisconnect(con)

