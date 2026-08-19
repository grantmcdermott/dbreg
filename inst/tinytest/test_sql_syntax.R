library(dbreg)

# Setup test data
test_df = data.frame(
  y = c(1, 2, 3, 4),
  x = c(1, 1, 2, 2),
  fe = c("a", "a", "b", "b")
)

#
## Test compress SQL syntax ----

invisible(capture.output({
  compress_sql = dbreg(
    y ~ x | fe,
    data = test_df,
    strategy = "compress",
    sql_only = TRUE
  )
}))

expect_false(
  grepl(",\\s*FROM\\b", compress_sql, ignore.case = TRUE, perl = TRUE),
  info = "compress SQL has no trailing comma before FROM"
)
