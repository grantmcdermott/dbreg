# dbbinsreg: unconstrained (s = 0) estimation in the database
#
# Covers non-DuckDB backends, integer running variables, missing and
# single-valued bins, and the "auto" strategy.

library(dbreg)
library(DBI)

con = dbConnect(duckdb::duckdb())

# Per-bin OLS of y on u = (x - x_left) / h, evaluated at the bin mean of x.
# This is what points = c(1, 0) estimates (bins are fitted independently).
ref_deg1 = function(dat, bin) {
  vapply(split(dat, bin), function(s) {
    x_left = min(s$x)
    h = max(s$x) - x_left
    b = coef(lm(y ~ I((x - x_left) / h), data = s))
    unname(b[1] + b[2] * (mean(s$x) - x_left) / h)
  }, numeric(1))
}

#
## Missing and single-valued bins (degree 1) ----

set.seed(2)
dat = data.frame(x = c(5, rep(15, 300), runif(3000, 20, 40)))
dat$y = ifelse(dat$x == 15, 3, 0.2 * dat$x) + rnorm(nrow(dat), sd = 0.1)
brk = c(0, 10, 20, 30, 40)
dat$bin = findInterval(dat$x, brk, rightmost.closed = TRUE)
dbWriteTable(con, "dat", dat)

# Bin 1 has one row (dropped: degree 1 needs >= 2); bin 2 has a single x value
p = dbbinsreg(y ~ x, conn = con, table = "dat", binspos = brk,
              points = c(1, 0), ci = FALSE, plot = FALSE)[["points"]]
expect_equal(as.integer(as.character(p$bin)), 2:4)
expect_equal(p$fit[1], mean(dat$y[dat$bin == 2]), tolerance = 1e-8,
             info = "single-valued bin returns its mean, not NaN")
keep = dat[dat$bin %in% 3:4, ]
expect_equal(p$fit[2:3], unname(ref_deg1(keep, keep$bin)), tolerance = 1e-8,
             info = "slopes are matched to the right bins after a dropped bin")

# Bin 1 has no rows at all
dbWriteTable(con, "dat_gap", dat[dat$x >= 20, ])
p2 = dbbinsreg(y ~ x, conn = con, table = "dat_gap", binspos = brk,
               points = c(1, 0), ci = FALSE, plot = FALSE)[["points"]]
expect_equal(p2$fit, unname(ref_deg1(keep, keep$bin)), tolerance = 1e-8,
             info = "slopes are matched to the right bins when a bin is empty")

#
## "auto" strategy: moments without FE when compression is poor ----

set.seed(3)
cont = data.frame(x = runif(20000), z = rnorm(20000))
cont$y = cont$x + cont$z + rnorm(20000)
dbWriteTable(con, "cont", cont)
for (spec in list(list(y ~ x, c(1, 0)), list(y ~ x + z, c(0, 0)))) {
  auto = dbbinsreg(spec[[1]], conn = con, table = "cont", nbins = 10,
                   points = spec[[2]], plot = FALSE)
  comp = suppressWarnings(
    dbbinsreg(spec[[1]], conn = con, table = "cont", nbins = 10,
              points = spec[[2]], strategy = "compress", plot = FALSE)
  )
  expect_equal(auto$model$strategy, "moments", info = deparse(spec[[1]]))
  expect_equal(auto$points, comp$points, tolerance = 1e-8,
               info = paste("auto (moments) matches compress:", deparse(spec[[1]])))
}
# Bin means compress well, so auto keeps compress
expect_equal(
  dbbinsreg(y ~ x, conn = con, table = "cont", nbins = 10, plot = FALSE)$model$strategy,
  "compress"
)

dbDisconnect(con, shutdown = TRUE)

#
## Non-DuckDB backend (SQLite) ----

if (!requireNamespace("RSQLite", quietly = TRUE)) {
  exit_file("RSQLite not installed")
}

lite = dbConnect(RSQLite::SQLite(), ":memory:")
duck = dbConnect(duckdb::duckdb())

# Integer running variable: no integer division in the basis or geometry
set.seed(1)
int_dat = data.frame(x = sample(0:40, 5000, TRUE))
int_dat$y = 0.5 * int_dat$x + rnorm(5000)
dbWriteTable(lite, "int_dat", int_dat)
dbWriteTable(duck, "int_dat", int_dat)
for (pts in list(c(0, 0), c(1, 0))) {
  a = dbbinsreg(y ~ x, conn = lite, table = "int_dat", binspos = brk,
                points = pts, plot = FALSE)[["points"]]
  b = dbbinsreg(y ~ x, conn = duck, table = "int_dat", binspos = brk,
                points = pts, plot = FALSE)[["points"]]
  expect_equal(a, b, tolerance = 1e-8,
               info = paste("SQLite matches DuckDB, points =", deparse(pts)))
}

dbDisconnect(lite)
dbDisconnect(duck, shutdown = TRUE)
