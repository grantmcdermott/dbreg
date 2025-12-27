library(dbreg)
library(fixest)

# Load test data
data("trade", package = "fixest")

# Fit models
mod_db = dbreg(
  Euros ~ dist_km | Destination + Origin,
  data = trade,
  vcov = 'hc1'
)

mod_fe = feols(
  Euros ~ dist_km | Destination + Origin,
  data = trade,
  vcov = 'hc1'
)

# Test vcov method
vcov_db = vcov(mod_db)
vcov_fe = vcov(mod_fe)
expect_equal(vcov_db["dist_km", "dist_km"], vcov_fe["dist_km", "dist_km"], tolerance = 1e-6)

# Test confint method
ci_db = confint(mod_db)
ci_fe = confint(mod_fe, "dist_km")
expect_equal(ci_db["dist_km", 1], ci_fe[, 1], tolerance = 1e-4)
expect_equal(ci_db["dist_km", 2], ci_fe[, 2], tolerance = 1e-4)

# Test confint with different level
ci90_db = confint(mod_db, level = 0.90)
ci90_fe = confint(mod_fe, "dist_km", level = 0.90)
expect_equal(ci90_db["dist_km", 1], ci90_fe[, 1], tolerance = 1e-4)
expect_equal(ci90_db["dist_km", 2], ci90_fe[, 2], tolerance = 1e-4)

# Test gof function
gof_vals = dbreg:::gof(mod_db)
fe_stats = fitstat(mod_fe, ~ r2 + ar2 + rmse, simplify = TRUE, verbose = FALSE)
expect_equal(as.numeric(gof_vals["r2"]), as.numeric(fe_stats$r2), tolerance = 1e-6)
expect_equal(as.numeric(gof_vals["adj_r2"]), as.numeric(fe_stats$ar2), tolerance = 1e-6)
expect_equal(as.numeric(gof_vals["rmse"]), as.numeric(fe_stats$rmse), tolerance = 1e-6)

# Test fes argument
mod_nofes = dbreg(
  Temp ~ Wind | Month,
  data = airquality
)

ci_nofes = confint(mod_nofes, fes = FALSE)
expect_equal(nrow(ci_nofes), 1)
expect_equal(rownames(ci_nofes), "Wind")

ci_fes = confint(mod_nofes, fes = TRUE)
expect_true(nrow(ci_fes) > 1)
expect_true("Wind" %in% rownames(ci_fes))
