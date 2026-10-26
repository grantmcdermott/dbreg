if (!requireNamespace("etwfe", quietly = TRUE)) exit_file("etwfe not installed")
library(dbreg)
library(fixest)
library(etwfe)

data("base_stagg", package = "fixest")
# etwfe >= 0.6.2.99 reports unconditional SEs by default; older versions
# condition on the cells' shares
has_uncond = packageVersion("etwfe") >= "0.6.2.99"

# emfx() results keyed like dbetwfe's coefficient names
emfx_ref = function(m, type, conditional, ...) {
  e = as.data.frame(if (conditional) emfx(m, type = type, vcov = TRUE, ...) else emfx(m, type = type, ...))
  a = attr(m, "etwfe")
  key = switch(
    type,
    simple = "ATT",
    event = paste0("event::", e[["event"]]),
    group = paste0(a[["gvar"]], "::", e[[a[["gvar"]]]]),
    calendar = paste0(a[["tvar"]], "::", e[[a[["tvar"]]]])
  )
  out = data.frame(est = e[["estimate"]], se = e[["std.error"]], row.names = key)
  out[order(key), ]
}
expect_aggregates = function(m, db, label) {
  for (type in c("simple", "group", "calendar", "event")) {
    for (conditional in c(TRUE, if (has_uncond) FALSE)) {
      ref = emfx_ref(m, type, conditional)
      ct = aggregate(db, type, unconditional = !conditional)[["coeftable"]]
      info = paste(label, type, if (conditional) "conditional" else "unconditional")
      expect_equal(sort(rownames(ct)), rownames(ref), info = paste(info, "names"))
      expect_equal(unname(ct[rownames(ref), "estimate"]), ref[["est"]], tolerance = 1e-8, info = paste(info, "estimates"))
      # emfx() derivatives are numerical in some designs, hence the tolerance
      expect_equal(unname(ct[rownames(ref), "std.error"]), ref[["se"]], tolerance = 1e-5, info = paste(info, "SEs"))
    }
  }
}

#
## Matches etwfe() + emfx() ----

for (cgroup in c("notyet", "never")) {
  for (vc in c("iid", "hc1", "cluster")) {
    label = paste(cgroup, vc)
    m = etwfe(
      y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, cgroup = cgroup,
      vcov = switch(vc, iid = "iid", hc1 = "hetero", cluster = ~id)
    )
    db = dbetwfe(
      y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, cgroup = cgroup,
      vcov = if (vc == "cluster") ~id else vc, aggregate = "none"
    )
    b = coef(m)
    expect_equal(coef(db)[names(b)], b, tolerance = 1e-10, info = paste(label, "cohort-by-period effects"))
    expect_equal(as.numeric(db$coeftable[names(b), "std.error"]), as.numeric(se(m)), tolerance = 1e-10, info = paste(label, "cohort-by-period SEs"))
    expect_aggregates(m, db, label)
  }
}

# Clusters that span cohorts (not nested in them) take a different code path
base_stagg$cl = base_stagg$id %% 10
m = etwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, vcov = ~cl)
expect_aggregates(m, dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, vcov = ~cl), "clusters spanning cohorts")

# Event-time options
m = etwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, vcov = ~id)
db = dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, vcov = ~id)
ref = emfx_ref(m, "event", TRUE, post_only = FALSE)
expect_equal(
  unname(aggregate(db, "event", post_only = FALSE, unconditional = FALSE)$coeftable[rownames(ref), "estimate"]),
  ref[["est"]],
  tolerance = 1e-8,
  info = "post_only = FALSE (zero pre-treatment effects)"
)
ref = emfx_ref(m, "event", TRUE, window = 2)
expect_equal(
  unname(aggregate(db, "event", window = 2, unconditional = FALSE)$coeftable[rownames(ref), "estimate"]),
  ref[["est"]],
  tolerance = 1e-8,
  info = "event window"
)

# Weighted, and unbalanced panels
base_stagg$w = 0.5 + (base_stagg$id %% 7) / 4
m = etwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, weights = ~w, vcov = ~id)
expect_aggregates(m, dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, weights = "w", vcov = ~id), "weighted")
unbalanced = base_stagg[base_stagg$id %% 9 != 0 | base_stagg$year > 3, ]
m = etwfe(y ~ 0, tvar = year, gvar = year_treated, data = unbalanced, vcov = ~id)
expect_aggregates(m, dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = unbalanced, vcov = ~id), "unbalanced")

# Clustering by the cohort (etwfe's default): nested cohort FE don't count
# towards the small-sample K, as in fixest
m = etwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, vcov = ~year_treated)
expect_aggregates(m, dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, vcov = ~year_treated), "cluster by cohort")

# A cohort treated in the first period: its reference-period cells count as
# zero effects, as in emfx()
first_period = base_stagg
first_period$year_treated[first_period$year_treated == 2] = 1
for (cgroup in c("notyet", "never")) {
  m = suppressWarnings(etwfe(y ~ 0, tvar = year, gvar = year_treated, data = first_period, cgroup = cgroup, vcov = ~id))
  expect_aggregates(m, dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = first_period, cgroup = cgroup, vcov = ~id), paste("first-period cohort", cgroup))
}

# Several never-treated codes outside the panel: the farthest is the reference
# (rule of etwfe >= 0.6.2.99)
if (has_uncond) {
  two_codes = base_stagg
  two_codes$year_treated[two_codes$year_treated == 10] = 11
  m = suppressWarnings(etwfe(y ~ 0, tvar = year, gvar = year_treated, data = two_codes, cgroup = "never", vcov = ~id))
  expect_warning(
    {db = dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = two_codes, cgroup = "never", vcov = ~id)},
    "farthest"
  )
  expect_aggregates(m, db, "farthest never-treated code")
}

#
## Interface ----

# Never-treated units can have a missing cohort; columns can be strings
never_na = base_stagg
never_na$year_treated[never_na$year_treated == 10000] = NA
expect_equal(
  coef(dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = never_na)),
  coef(dbetwfe(y ~ 0, tvar = "year", gvar = "year_treated", data = base_stagg)),
  info = "missing cohort means never treated"
)
expect_equal(
  dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = never_na, vcov = ~id)$coeftable,
  dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, vcov = ~id)$coeftable,
  info = "missing cohort with clustered SEs"
)
expect_equal(
  dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = never_na, vcov = ~year_treated)$coeftable,
  dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, vcov = ~year_treated)$coeftable,
  info = "missing cohort clustered by the cohort keeps never-treated units"
)

# Re-aggregating gives the same result as aggregating at fit time
db = dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg)
expect_equal(
  coef(aggregate(db, "group")),
  coef(dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, aggregate = "group")),
  info = "aggregate() matches aggregate at fit time"
)

expect_error(dbetwfe(y ~ x1, tvar = year, gvar = year_treated, data = base_stagg), "not supported")
expect_error(dbetwfe(y ~ 0, tvar = year, data = base_stagg), "required")
expect_error(dbetwfe(y ~ 0, tvar = year, gvar = year_treated, data = base_stagg, gref = 999), "not found")
expect_error(plot(aggregate(db, "simple")), "Plotting needs")
expect_error(predict(db), "not supported")
