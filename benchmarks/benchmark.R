# Concise benchmark comparing demean/sufficient statistics vs group-by approach
# Uses data.table for efficient data manipulation

#
## libs and globals ----

library(dbreg)
# pkgload::load_all()
library(fixest)
library(data.table)
library(tinyplot)
tinytheme("clean")

# True coefficients
beta_true = c(x1 = 0.5, x2 = -0.3)

#
## Benchmark functions ----

# Function for generating panel data (with discrete regressors)
gen_dat = function(
  N_units = 1e3,
  T_periods = 5L,
  beta = beta_true,
  K1 = 4L,
  K2 = 4L,
  sd_fe_unit = 1.0,
  sd_fe_time = 0.5,
  sd_err = 0.8
) {
  unit_fe = rnorm(N_units, sd = sd_fe_unit)
  time_fe = rnorm(T_periods, sd = sd_fe_time)

  dat = CJ(unit = 1:N_units, time = 1:T_periods)

  # Create latent variables and discretize. Then add outcome variable.
  dat[, let(
    latent_x1 = 0.6 * unit_fe[unit] + 0.3 * time_fe[time] + rnorm(.N, sd = 0.7),
    latent_x2 = -0.4 * unit_fe[unit] + 0.5 * time_fe[time] + rnorm(.N, sd = 0.7)
  )]

  # Compute quantile cuts outside data.table
  cuts1 = quantile(dat$latent_x1, probs = seq(0, 1, length.out = K1 + 1))
  cuts2 = quantile(dat$latent_x2, probs = seq(0, 1, length.out = K2 + 1))

  # Make unique breakpoints (jitter if ties) - matching original script
  cuts1 = unique(cuts1)
  while (length(cuts1) < K1 + 1) {
    cuts1 = sort(unique(c(cuts1, jitter(cuts1[length(cuts1)], 1e-6))))
  }
  cuts2 = unique(cuts2)
  while (length(cuts2) < K2 + 1) {
    cuts2 = sort(unique(c(cuts2, jitter(cuts2[length(cuts2)], 1e-6))))
  }

  dat[, let(
    x1 = as.integer(cut(
      latent_x1,
      breaks = cuts1,
      include.lowest = TRUE,
      labels = FALSE
    )) -
      1L,
    x2 = as.integer(cut(
      latent_x2,
      breaks = cuts2,
      include.lowest = TRUE,
      labels = FALSE
    )) -
      1L
  )][,
    y := unit_fe[unit] +
      time_fe[time] +
      beta["x1"] * x1 +
      beta["x2"] * x2 +
      rnorm(.N, sd = sd_err)
  ]

  dat[, c("latent_x1", "latent_x2") := NULL]
  setnames(dat, c("unit", "time"), c("unit_fe", "time_fe"))

  return(dat)
}

# Helper function to time and fit models
time_fit = function(
  data,
  fml,
  strategy,
  size_limit = NULL,
  compressible = FALSE
) {
  # Manual override for some compress runs (which take too long)
  # Match original script logic: compress strategy has different limits for compressible vs non-compressible
  if (is.null(size_limit)) {
    size_limit = if (strategy == "compress") {
      if (isTRUE(compressible)) 5e5 else 1e4
    } else {
      Inf
    }
  }

  if (nrow(data) > size_limit) {
    cat(sprintf(
      "Skipping: nrow(data) = %d > size_limit = %d\n",
      nrow(data),
      size_limit
    ))
    return(data.table(
      strategy = strategy,
      compressible = compressible,
      tt = NA,
      x1_coef = NA,
      x1_se = NA,
      x2_coef = NA,
      x2_se = NA
    ))
  }

  if (strategy != "feols") {
    tt = system.time({
      fit = dbreg(fml = fml, data = data, vcov = "iid", strategy = strategy)
    })["elapsed"]
  } else {
    tt = system.time({
      fit = feols(fml = fml, data = data, vcov = "iid", lean = TRUE)
    })["elapsed"]
  }
  x1_coef = fit$coeftable["x1", 1]
  x1_se = fit$coeftable["x1", 2]
  x2_coef = fit$coeftable["x2", 1]
  x2_se = fit$coeftable["x2", 2]

  data.table(
    strategy = strategy,
    compressible = compressible,
    tt = tt,
    x1_coef = x1_coef,
    x1_se = x1_se,
    x2_coef = x2_coef,
    x2_se = x2_se
  )
}

#
## Run benchmark -----

# Benchmark parameters
N_grid = 10^(3:8)
n_iters = 3
fml = y ~ x1 + x2 | unit_fe + time_fe

res = rbindlist(lapply(
  N_grid,
  function(N) {
    cat(sprintf("Data size (total rows) = %d\n", N))
    # Fix the no. of units based on total rows and the (fixed) no. of periods
    T_periods = 5L
    N_units = N / T_periods
    ret2 = rbindlist(lapply(
      1:n_iters,
      function(i, ...) {
        cat(sprintf("Iteration %d/%d\n", i, n_iters))
        # Non-compressible first
        set.seed(123)
        dat = gen_dat(N_units = N_units, T_periods = T_periods)
        ret = rbindlist(lapply(
          c("feols", "compress", "demean"),
          function(s) time_fit(dat, fml, strategy = s, compressible = FALSE)
        ))[,
          strategy := factor(
            strategy,
            levels = c("feols", "compress", "demean")
          )
        ]
        ret$compressible = FALSE

        # For compressible data: Repeat the data 100 times, stacking vertically
        # (we add a small bit of noise to y, so it's not a literal duplication)
        comp_reps = 100L
        set.seed(123)
        dat = gen_dat(N_units = N_units / comp_reps, T_periods = T_periods)
        dat = dat[
          rep(seq_len(nrow(dat)), times = comp_reps)
        ][,
          y := y + rnorm(.N, sd = 1e-4)
        ]
        retc = rbindlist(lapply(
          c("feols", "compress", "demean"),
          function(s) time_fit(dat, fml, strategy = s, compressible = TRUE)
        ))[,
          strategy := factor(
            strategy,
            levels = c("feols", "compress", "demean")
          )
        ]
        retc$compressible = TRUE
        ret = rbind(ret, retc)

        ret$i = i
        return(ret)
      }
    ))
    ret2$N = N
    ret2[,
      compressible := fifelse(compressible, "Compressible", "Non-compressible")
    ]
    gc()
    return(ret2)
  }
))
res

res_summ = res[,
  c(list(tt = median(tt)), lapply(.SD, first)),
  by = .(strategy, compressible, N),
  .SDcols = x1_coef:x2_se
]
res_summ

#
## Plots ----

#
## benchmark times

plt(
  tt ~ N | strategy,
  facet = ~ as.factor(compressible),
  facet.args = list(ncol = 1),
  data = res_summ,
  type = 'bar',
  beside = TRUE,
  fill = 0.5,
  xaxl = \(x) tinylabel(as.numeric(x), 'l'),
  main = "In-memory benchmarks: Compressible vs Non-compressible",
  sub = "Estimation form: y ~ x1 + x2 | unit_fe + time_fe",
  xlab = "Number of Rows",
  ylab = "Median Time (seconds)",
  file = 'benchmarks/benchmark_results.png',
  width = 8,
  height = 5
)

#
## compare coefs and vcov

res_long = res_summ |>
  melt(
    id = c('compressible', 'strategy', 'N'),
    measure.vars = measure(coef, part, sep = "_")
  ) |>
  dcast(compressible + N + coef + part ~ strategy) |>
  melt(
    measure = c('compress', 'demean'),
    value = 'dbreg',
    variable = 'strategy'
  )

plt(
  feols ~ dbreg | strategy,
  facet = coef ~ compressible,
  # facet.args = list(free = TRUE),
  pch = 'by',
  cex = 1.5,
  data = res_long[part == 'coef'],
  main = 'Comparison of Coefficient Estimates',
  draw = abline(0, 1, lty = 2),
  file = 'benchmarks/benchmark_coef_comp.png',
  width = 8,
  height = 5
)

plt(
  feols ~ dbreg | strategy,
  facet = coef ~ compressible,
  # facet.args = list(free = TRUE),
  pch = 'by',
  cex = 1.5,
  data = res_long[part == 'se'],
  main = 'Comparison of Standard Errors',
  draw = abline(0, 1, lty = 2),
  file = 'benchmarks/benchmark_vcov_comp.png',
  width = 8,
  height = 5
)
