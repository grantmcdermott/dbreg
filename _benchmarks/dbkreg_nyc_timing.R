# Quick timing benchmark for dbkreg on NYC Taxi January data (DuckDB)
# Produces a ggplot timing figure and CSV summary.

library(dbreg)
# pkgload::load_all()
library(DBI)
library(duckdb)
library(data.table)
library(here)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Please install ggplot2 to run this benchmark.")
}
if (!requireNamespace("scales", quietly = TRUE)) {
  stop("Please install scales to run this benchmark.")
}

set.seed(123)

nyc_path = here::here("nyc-taxi/year=2012")
if (!dir.exists(nyc_path)) {
  stop("NYC taxi data not found at nyc-taxi/year=2012")
}

con = dbConnect(duckdb::duckdb(), shutdown = TRUE)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

nyc_jan = sprintf("%s/month=1/*.parquet", nyc_path)

# Total rows (month=1)
total_n = dbGetQuery(
  con,
  sprintf("SELECT COUNT(*) AS n FROM read_parquet('%s')", nyc_jan)
)$n

sizes = c(1e5, 3e5, 1e6, 3e6, 1e7, 3e7, total_n)
sizes = unique(pmin(sizes, total_n))
sizes = sizes[sizes > 0]
sizes = sort(unique(sizes))

# Bandwidth based on SD (fast enough and stable)
sd_x = dbGetQuery(
  con,
  sprintf(
    "SELECT STDDEV_SAMP(trip_distance) AS sd
     FROM read_parquet('%s')
     WHERE trip_distance IS NOT NULL AND fare_amount IS NOT NULL",
    nyc_jan
  )
)$sd

bandwidth = 0.5 * sd_x
grid_n = 200L
n_iter = 2L
degrees = c(0L, 1L)

time_one = function(n, degree) {
  view_name = sprintf("nyc_jan_%s", n)
  if (n >= total_n) {
    sql = sprintf(
      "CREATE OR REPLACE TEMP VIEW %s AS
       SELECT * FROM read_parquet('%s')",
      view_name, nyc_jan
    )
  } else {
    sql = sprintf(
      "CREATE OR REPLACE TEMP VIEW %s AS
       SELECT * FROM read_parquet('%s') USING SAMPLE %d ROWS",
      view_name, nyc_jan, n
    )
  }
  dbExecute(con, sql)
  
  tt = system.time({
    dbkreg(
      fare_amount ~ trip_distance,
      conn = con,
      table = view_name,
      eval = "grid",
      n_eval = grid_n,
      grid_method = "quantile",
      bandwidth = bandwidth,
      kernel = "epanechnikov",
      degree = degree,
      verbose = FALSE
    )
  })[["elapsed"]]
  
  data.table(
    n = n,
    degree = degree,
    elapsed = tt
  )
}

res = rbindlist(lapply(
  sizes,
  function(n) {
    cat(sprintf("n = %s\n", format(n, big.mark = ",")))
    rbindlist(lapply(
      degrees,
      function(d) {
        rbindlist(lapply(seq_len(n_iter), function(i) {
          cat(sprintf("  degree = %d, iter %d/%d\n", d, i, n_iter))
          gc()
          time_one(n, d)
        }))
      }
    ))
  }
))

summ = res[, .(
  median_elapsed = median(elapsed, na.rm = TRUE),
  mean_elapsed = mean(elapsed, na.rm = TRUE)
), by = .(n, degree)]

plot_df = copy(summ)
plot_df[, degree := factor(degree, levels = c(0, 1), labels = c("local constant", "local linear"))]

p = ggplot2::ggplot(plot_df, ggplot2::aes(x = n, y = median_elapsed, color = degree)) +
  ggplot2::geom_line() +
  ggplot2::geom_point(size = 2) +
  ggplot2::scale_x_log10(labels = scales::comma) +
  ggplot2::scale_y_log10() +
  ggplot2::labs(
    title = "dbkreg timing on NYC taxi (January)",
    subtitle = sprintf("grid_n = %d | bandwidth = %.4g | kernel = epanechnikov", grid_n, bandwidth),
    x = "Rows (sample size)",
    y = "Elapsed time (seconds, median of runs)",
    color = NULL
  ) +
  ggplot2::theme_minimal()

fig_path = here::here("_benchmarks/dbkreg_nyc_timing.png")
csv_path = here::here("_benchmarks/dbkreg_nyc_timing.csv")
ggplot2::ggsave(fig_path, p, width = 8, height = 5, dpi = 150)
data.table::fwrite(plot_df, csv_path)

cat("Saved figure to:", fig_path, "\n")
cat("Saved summary to:", csv_path, "\n")
