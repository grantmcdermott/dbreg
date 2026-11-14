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

#
## Quantile bins are defined by value ----

# Reference: binsreg's definition. Breaks are quantile(type = 2), repeated
# breaks are dropped, and a value belongs to bin i if it is in (b_{i-1}, b_i]
# (bin 1 also includes b_0).
ref_qbins = function(x, y, B) {
  br = quantile(x, seq(0, 1, 1 / B), type = 2, names = FALSE)
  br = c(br[1], unique(br[-1]))
  bin = findInterval(x, br, rightmost.closed = TRUE, left.open = TRUE)
  list(
    breaks = br,
    bin = sort(unique(bin)),
    fit = as.numeric(tapply(y, bin, mean)),
    x = as.numeric(tapply(x, bin, mean))
  )
}

# Tied x (12 distinct values): exact quantile breaks, tied values share a bin
cw = as.data.frame(ChickWeight)
cw$Time = as.numeric(cw$Time)
dbWriteTable(con, "cw", cw)
ref = ref_qbins(cw$Time, cw$weight, 5)
bs = dbbinsreg(weight ~ Time, conn = con, table = "cw", nbins = 5, ci = FALSE,
               plot = FALSE)
expect_equal(bs$points$fit, ref$fit, tolerance = 1e-8)
expect_equal(bs$points$x, ref$x, tolerance = 1e-8)
expect_equal(as.integer(as.character(bs$points$bin)), ref$bin)
# No x value is split across bins
expect_equal(bs$bins$right, ref$breaks[-1], tolerance = 1e-8)

# Heavily tied x: repeated breaks are dropped, with a message, and the bins
# that remain are numbered 1, 2, ...
set.seed(8)
tied = data.frame(x = rpois(2e4, 0.4), z = rnorm(2e4))
tied$y = 0.3 * tied$x + tied$z + rnorm(2e4)
dbWriteTable(con, "tied", tied)
ref = ref_qbins(tied$x, tied$y, 10)
expect_message(
  bs <- suppressWarnings(
    dbbinsreg(y ~ x, conn = con, table = "tied", nbins = 10, ci = FALSE, plot = FALSE)
  ),
  "Ties in `x` reduced the number of bins from 10 to"
)
expect_warning(
  suppressMessages(
    dbbinsreg(y ~ x, conn = con, table = "tied", nbins = 10, ci = FALSE, plot = FALSE)
  ),
  "exceeds unique values"
)
expect_true(nrow(bs$points) < 10)
expect_equal(as.integer(as.character(bs$points$bin)), seq_len(nrow(bs$points)))
expect_equal(as.integer(as.character(bs$points$bin)), ref$bin)
expect_equal(bs$points$fit, ref$fit, tolerance = 1e-8)

# Same data with a control: quantile bins stay value-based
res = suppressMessages(suppressWarnings(
  dbbinsreg(y ~ x + z, conn = con, table = "tied", nbins = 5, plot = FALSE)
))
expect_equal(as.integer(as.character(res$points$bin)), seq_len(nrow(res$points)))

# Sampled breaks (randcut) also drop repeats and keep bins contiguous
sampled = suppressMessages(suppressWarnings(
  dbbinsreg(y ~ x, conn = con, table = "tied", nbins = 10, randcut = 0.5,
            ci = FALSE, plot = FALSE)
))
expect_equal(as.integer(as.character(sampled$points$bin)), seq_len(nrow(sampled$points)))
expect_equal(sum(sampled$bins$right >= sampled$bins$left), nrow(sampled$bins))
# ... and no rows are lost: the outer breaks are the true min and max
expect_equal(range(c(sampled$bins$left, sampled$bins$right)), range(tied$x))
expect_equal(sampled$opt$N, nrow(tied))

# The constrained (s > 0) path uses the same bins
sp = suppressMessages(
  dbbinsreg(weight ~ Time, conn = con, table = "cw", nbins = 5, points = c(1, 1),
            ci = FALSE, plot = FALSE)
)
bs0 = dbbinsreg(weight ~ Time, conn = con, table = "cw", nbins = 5, ci = FALSE,
                plot = FALSE)
expect_equal(sp$bins$right, bs0$bins$right)
expect_equal(sp$bins$left, bs0$bins$left)

# User-supplied breaks keep the left-closed convention [b_{i-1}, b_i), with the
# last bin closed on both ends, so a value on a break belongs to the bin above
dbWriteTable(con, "on_breaks", data.frame(x = rep(1:4, each = 5), y = rep(c(10, 20, 30, 40), each = 5)))
for (pts in list(c(0, 0), c(1, 1))) {
  mb = dbbinsreg(y ~ x, conn = con, table = "on_breaks", binspos = c(1, 2, 3, 4),
                 points = pts, ci = FALSE, plot = FALSE)
  expect_equal(mb$points$fit[1:2], c(10, 20), info = paste("manual breaks, points =", deparse(pts)))
}
mb = dbbinsreg(y ~ x, conn = con, table = "on_breaks", binspos = c(1, 2, 3, 4),
               ci = FALSE, plot = FALSE)
expect_equal(mb$points$fit, c(10, 20, 35))
# Evenly spaced bins too: breaks at 1, 2.5, 4 on x in {1, 2, 3, 4}, and with
# breaks that land on data values (x in 0:10, 5 bins: breaks 0, 2, ..., 10)
es = dbbinsreg(y ~ x, conn = con, table = "on_breaks", nbins = 3, binspos = "es",
               ci = FALSE, plot = FALSE)
expect_equal(es$points$fit, c(10, 20, 35), info = "es bins, no value on an interior break")
dbWriteTable(con, "zero_ten", data.frame(x = rep(0:10, each = 3), y = rep(0:10, each = 3)))
es2 = dbbinsreg(y ~ x, conn = con, table = "zero_ten", nbins = 5, binspos = "es",
                ci = FALSE, plot = FALSE)
expect_equal(es2$points$fit, c(0.5, 2.5, 4.5, 6.5, 9), info = "es bins: values on a break go to the upper bin (as before)")

# Non-finite x fails clearly instead of producing broken SQL
dbWriteTable(con, "inf_x", data.frame(x = c(rnorm(50), Inf), y = rnorm(51)))
expect_error(
  dbbinsreg(y ~ x, conn = con, table = "inf_x", nbins = 5, plot = FALSE),
  "non-finite"
)

#
## Sampling draws random rows from the filtered data ----

half_null = data.frame(id = 1:20000, x = c(rep(NA, 10000), runif(10000)))
dbWriteTable(con, "half_null", half_null)
smp = dbGetQuery(con, dbreg:::sql_sample("id", "half_null", "x IS NOT NULL", 1000, "duckdb"))
expect_equal(nrow(smp), 1000L, info = "filter runs before sampling")
expect_true(all(smp$id > 10000))
# Spark's TABLESAMPLE (n ROWS) is a LIMIT, so Spark uses a random filter instead
spark_sql = dbreg:::sql_sample("x", "t", "x IS NOT NULL", 1000, "spark", n_rows = 50000)
expect_false(grepl("TABLESAMPLE", spark_sql))
expect_true(grepl("rand() < ", spark_sql, fixed = TRUE))
expect_true(grepl("LIMIT 1000", spark_sql, fixed = TRUE))
expect_false(grepl("rand() <", dbreg:::sql_sample("x", "t", "x IS NOT NULL", 1000, "spark", n_rows = 900),
                   fixed = TRUE), info = "no thinning when n >= rows")

#
## No estimable bins gives a clear error ----

dbWriteTable(con, "one_row", data.frame(x = 1, y = 2))
expect_error(
  dbbinsreg(y ~ x, conn = con, table = "one_row", nbins = 1, points = c(1, 0),
            plot = FALSE),
  "No bins have enough observations"
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

# Tied x: the same bins and fits on both backends (ties no longer depend on
# the backend's row order), and SQLite gets exact quantile breaks
for (db in list(lite, duck)) {
  dbWriteTable(db, "cw", cw)
  dbWriteTable(db, "tied", tied)
}
for (bp in list("qs", "es")) {
  a = dbbinsreg(weight ~ Time, conn = lite, table = "cw", nbins = 5, binspos = bp,
                plot = FALSE)
  b = dbbinsreg(weight ~ Time, conn = duck, table = "cw", nbins = 5, binspos = bp,
                plot = FALSE)
  expect_equal(a$points, b$points, tolerance = 1e-8,
               info = paste("SQLite matches DuckDB, tied x, binspos =", bp))
  expect_equal(a$bins, b$bins, tolerance = 1e-8)
}
ref = ref_qbins(cw$Time, cw$weight, 5)
a = dbbinsreg(weight ~ Time, conn = lite, table = "cw", nbins = 5, ci = FALSE, plot = FALSE)
expect_equal(a$points$fit, ref$fit, tolerance = 1e-8, info = "SQLite matches binsreg's bins")
# Separate points / line models, and the heavily tied case
a = suppressMessages(suppressWarnings(
  dbbinsreg(y ~ x, conn = lite, table = "tied", nbins = 10, points = c(0, 0),
            line = c(1, 0), plot = FALSE)
))
b = suppressMessages(suppressWarnings(
  dbbinsreg(y ~ x, conn = duck, table = "tied", nbins = 10, points = c(0, 0),
            line = c(1, 0), plot = FALSE)
))
expect_equal(a$points, b$points, tolerance = 1e-8)
expect_equal(a$line, b$line, tolerance = 1e-8)

dbDisconnect(lite)
dbDisconnect(duck, shutdown = TRUE)
