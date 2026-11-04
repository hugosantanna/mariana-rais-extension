#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# Project: Mariana — RAIS extension (post-2018)
# Script: 04_longrun_event_study.R
# Summary: Extend the paper's event studies past 2018 (referee 1, comment 2).
#          Outcomes: log hourly wage and job separation (conditional on formal
#          MG employment, as in the paper) and formal employment on the
#          zero-filled 2014-cohort panel (script 05), plus employment anywhere
#          in Brazil when FILE_SCOPE = "national".
#          Sample variants handle the two post-2018 confounders:
#            B/C  Brumadinho (Jan 2019) hit the Paraopeba, inside the control group
#            D    Vale's statewide dam shutdowns after Brumadinho hit mining
#                 towns in both groups
#          2020 onward overlaps COVID-19; period averages keep 2019 separate.
# Outputs: 04_event_study.csv, 04_period_averages.csv, 04_event_study_*.pdf
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

source("R/utils.R")
suppressPackageStartupMessages({
  library(fixest)
  library(ggplot2)
})
setFixest_nthreads(N_THREADS)

log_msg("=== 04_longrun_event_study ===")
yrs <- available_years()
T_MAX <- max(yrs)
if (T_MAX <= 2018) stop("No post-2018 years in ", WORK_DIR, " — nothing to extend")
log_msg("Years in panel: ", min(yrs), "-", T_MAX)

d <- build_analysis_panel(yrs)
base <- load_baseline()

COVARS <- "age + age2 + male + tenure + tenure2"
FE <- "wid + municipality6 + year"
es_fml <- function(y, bartik = TRUE) {
  as.formula(paste0(y, " ~ i(year, treat, ref = ", BASE_YEAR, ")",
                    if (bartik) " + bartik" else "", " + ", COVARS, " | ", FE))
}

# Zero-filled employment panel, 2014 onward (script 05): absent from the MG
# panel in year t -> formal_employed = 0; municipality carried from 2014.
emp <- CJ(wid = base$wid, year = BASE_YEAR:T_MAX)
emp <- merge(emp, d[year >= BASE_YEAR, .(wid, year, municipality6)], by = c("wid", "year"), all.x = TRUE)
emp[, formal_employed := as.integer(!is.na(municipality6))]
emp <- merge(emp, base[, .(wid, muni_2014 = municipality6, treat, paraopeba_10km,
                           alto_sao_francisco, mining_muni)], by = "wid")
emp[is.na(municipality6), municipality6 := muni_2014]
if (FILE_SCOPE == "national") {
  anyjob <- rbindlist(lapply(BASE_YEAR:T_MAX, function(y) {
    read_fst(file.path(WORK_DIR, sprintf("earn_%d.fst", y)), columns = c("wid", "year"), as.data.table = TRUE)
  }))
  anyjob[, employed_anywhere := 1L]
  emp <- merge(emp, anyjob, by = c("wid", "year"), all.x = TRUE)
  emp[is.na(employed_anywhere), employed_anywhere := 0L]
}

################################################################################
# Run list: variant A with and without Bartik; B-D with Bartik (spec 4)
################################################################################

runs <- rbind(
  data.table(variant = "A_paper", bartik = c(TRUE, FALSE)),
  data.table(variant = c("B_no_paraopeba", "C_no_alto_sf", "D_no_mining_muni"), bartik = TRUE)
)

es_out <- list()
period_out <- list()
models_for_plot <- list()

PERIODS <- list(pre_2010_2013 = 2010:2013, post_2016_2018 = 2016:2018, y2019 = 2019L,
                covid_2020_2021 = 2020:2021, post_2022_plus = seq(2022L, max(2022L, T_MAX)))

# Equal-weight average of event-study coefficients within each period, with
# delta-method standard errors from the clustered covariance matrix.
period_avgs <- function(m) {
  b <- coef(m); V <- vcov(m)
  rbindlist(lapply(names(PERIODS), function(p) {
    nm <- intersect(paste0("year::", PERIODS[[p]], ":treat"), names(b))
    if (!length(nm)) return(NULL)
    w <- rep(1 / length(nm), length(nm))
    yy <- as.integer(sub("year::(\\d{4}).*", "\\1", nm))
    data.table(period = p, years = paste(range(yy), collapse = "-"),
               coef = sum(w * b[nm]), se = sqrt(drop(t(w) %*% V[nm, nm] %*% w)))
  }))
}

run_es <- function(dat, fml, outcome, variant, spec) {
  log_msg(sprintf("  %-20s %-18s %s (%s obs)", outcome, variant, spec, format(nrow(dat), big.mark = ",")))
  m <- feols(fml, data = dat, cluster = ~municipality6)
  es_out[[length(es_out) + 1]] <<- tidy_coefs(m, "^year::", dat)[
    , `:=`(year = as.integer(sub("year::(\\d{4}).*", "\\1", term)),
           outcome = outcome, variant = variant, spec = spec)]
  period_out[[length(period_out) + 1]] <<- period_avgs(m)[
    , `:=`(outcome = outcome, variant = variant, spec = spec, n_obs = nobs(m))]
  m
}

for (i in seq_len(nrow(runs))) {
  v <- runs$variant[i]; bk <- runs$bartik[i]
  spec <- if (bk) "spec4_bartik" else "spec3_no_bartik"
  dv <- VARIANTS[[v]](d)
  run_es(dv[!is.na(log_hourly_wage)], es_fml("log_hourly_wage", bk), "log_hourly_wage", v, spec)
  run_es(dv[!is.na(displaced) & !is.na(log_hourly_wage)], es_fml("displaced", bk), "displaced", v, spec)
  if (bk) {
    ev <- VARIANTS[[v]](emp)
    efml <- as.formula(paste0("formal_employed ~ i(year, treat, ref = ", BASE_YEAR, ") | ", FE))
    run_es(ev, efml, "formal_employed_mg", v, "worker_muni_year_fe")
    if (FILE_SCOPE == "national") {
      efml2 <- as.formula(paste0("employed_anywhere ~ i(year, treat, ref = ", BASE_YEAR, ") | ", FE))
      run_es(ev, efml2, "employed_anywhere", v, "worker_muni_year_fe")
    }
  }
}

# December pay as an alternative wage measure (referee 2, comment 5), variant A.
if (mean(!is.na(d$log_hourly_dec)) > 0.5) {
  run_es(d[!is.na(log_hourly_dec)], es_fml("log_hourly_dec", TRUE), "log_hourly_dec", "A_paper", "spec4_bartik")
} else {
  log_msg("  December pay mostly missing; skipping log_hourly_dec")
}

es_all <- rbindlist(es_out)
write_out(es_all[, .(outcome, variant, spec, year, coef, se, pval, n_obs, n_clusters)], "04_event_study.csv")
write_out(rbindlist(period_out), "04_period_averages.csv")

################################################################################
# Plots (no titles; captions go in the paper)
################################################################################

Y_LABELS <- c(log_hourly_wage = "Log hourly wage", displaced = "Job separation",
              formal_employed_mg = "Formal employment in MG", employed_anywhere = "Formal employment anywhere",
              log_hourly_dec = "Log hourly December pay")
ref <- unique(es_all[, .(outcome, variant, spec)])[, `:=`(year = BASE_YEAR, coef = 0, se = 0)]
pl <- rbind(es_all[, .(outcome, variant, spec, year, coef, se)], ref)
for (oc in unique(pl$outcome)) {
  g <- ggplot(pl[outcome == oc & spec != "spec3_no_bartik"],
              aes(year, coef, colour = variant, shape = variant)) +
    annotate("rect", xmin = 2018.5, xmax = 2019.5, ymin = -Inf, ymax = Inf, alpha = 0.08) +
    annotate("rect", xmin = 2019.5, xmax = 2021.5, ymin = -Inf, ymax = Inf, alpha = 0.15) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_vline(xintercept = 2015.5, linetype = "dotted") +
    geom_pointrange(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se),
                    position = position_dodge(width = 0.5), size = 0.3) +
    scale_x_continuous(breaks = min(pl$year):T_MAX) +
    labs(x = "Year", y = paste0(Y_LABELS[[oc]], ", effect relative to 2014"), colour = NULL, shape = NULL) +
    theme_minimal(base_family = "serif") +
    theme(legend.position = "bottom", panel.grid.minor = element_blank())
  ggsave(file.path(OUT_DIR, paste0("04_event_study_", oc, ".pdf")), g, width = 8, height = 5)
}

log_msg("Shaded: 2019 (Brumadinho, light) and 2020-2021 (COVID, darker).")
log_msg("=== 04_longrun_event_study done ===")
