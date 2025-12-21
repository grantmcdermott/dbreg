library(dbreg)

#
## Test db_bins: focused, minimal coverage ----
#

# Use mtcars for quick smoke tests
data(mtcars)

#
## Basic sanity: p=0 (bin means) ----
#

bins0 = db_bins(mtcars, mpg, wt, B = 5, degree = 0, verbose = FALSE)

# Check structure
expect_true(inherits(bins0, "data.frame"))
expect_equal(nrow(bins0), 5)
expect_true(all(c("bin", "x_left", "x_right", "x_mid", "n", "y") %in% names(bins0)))

# Verify bin means match direct SQL
library(DBI)
library(duckdb)
con = dbConnect(duckdb())
dbWriteTable(con, "mtcars_test", mtcars)

manual_means = dbGetQuery(con, "
  WITH binned AS (
    SELECT 
      ntile(5) OVER (ORDER BY wt) AS bin,
      mpg
    FROM mtcars_test
  )
  SELECT 
    bin,
    AVG(mpg) AS mean_mpg
  FROM binned
  GROUP BY bin
  ORDER BY bin
")

expect_equal(bins0$y, manual_means$mean_mpg, tolerance = 1e-6)
dbDisconnect(con, shutdown = TRUE)


#
## Piecewise linear: p=1 ----
#

bins1 = db_bins(mtcars, mpg, wt, B = 5, degree = 1, verbose = FALSE)

expect_true(all(c("y_left", "y_right") %in% names(bins1)))
expect_equal(nrow(bins1), 5)
expect_false("y" %in% names(bins1))  # Should have segments, not points

# Segments should be defined (no NAs in well-populated bins)
expect_true(all(!is.na(bins1$y_left)))
expect_true(all(!is.na(bins1$y_right)))


#
## Piecewise quadratic: p=2 ----
#

bins2 = db_bins(mtcars, mpg, wt, B = 5, degree = 2, verbose = FALSE)

expect_true(all(c("y_left", "y_right") %in% names(bins2)))
expect_equal(nrow(bins2), 5)


#
## With controls ----
#

bins_ctrl = db_bins(
  mtcars, mpg, wt, 
  controls = ~ hp + cyl, 
  B = 5, 
  degree = 1, 
  verbose = FALSE
)

expect_equal(nrow(bins_ctrl), 5)
expect_true(all(c("y_left", "y_right") %in% names(bins_ctrl)))


#
## Edge cases ----
#

# Validate smooth > degree error
expect_error(
  db_bins(mtcars, mpg, wt, B = 5, degree = 1, smooth = 2),
  "smooth must be <= degree"
)

# Validate smooth > 0 requires moments_kkt
expect_error(
  db_bins(mtcars, mpg, wt, B = 5, degree = 1, smooth = 1, engine = "design_ols"),
  "smooth > 0 requires engine = 'moments_kkt'"
)

# Invalid degree
expect_error(
  db_bins(mtcars, mpg, wt, degree = 3),
  "degree must be 0, 1, or 2"
)


#
## Test with database connection ----
#

con2 = dbConnect(duckdb())
dbWriteTable(con2, "cars", mtcars)

bins_db = db_bins(
  data = "cars",
  y = mpg,
  x = wt,
  B = 5,
  degree = 1,
  conn = con2,
  verbose = FALSE
)

expect_equal(nrow(bins_db), 5)
expect_true(all(c("y_left", "y_right") %in% names(bins_db)))

dbDisconnect(con2, shutdown = TRUE)
