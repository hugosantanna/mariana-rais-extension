#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# Project: Mariana — RAIS extension (post-2018)
# Script: 03_replicate_baseline.R
# Summary: Re-estimate the paper's main table (Table 2) on this build, 2010-2018
#          only, and compare with the published numbers. The post-2018 results
#          are interpretable only if this matches: same cohort size, same
#          coefficients to about the third decimal.
#          Wage: scripts 05 (Panel A). Separation: script 05d (Panel B).
# Outputs: 03_replication.csv, 03_replication_cohort.csv,
#          03_event_study_2010_2018.csv
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

source("R/utils.R")
suppressPackageStartupMessages(library(fixest))
setFixest_nthreads(N_THREADS)

log_msg("=== 03_replicate_baseline ===")
yrs <- intersect(2010:2018, available_years())
if (length(yrs) < 9) log_msg("WARNING: only years ", paste(yrs, collapse = ", "), " available")
if (SAMPLE_SHARE < 1) log_msg("NOTE: SAMPLE_SHARE < 1, so observation counts will not match the paper")

d <- build_analysis_panel(yrs)
dw <- d[!is.na(log_hourly_wage)]
ds <- d[!is.na(displaced) & !is.na(log_hourly_wage)]

# Published Table 2 (paper/tables/script05_twfe_main/did_main_results_combined.tex)
paper <- data.table(
  outcome = rep(c("log_hourly_wage", "displaced"), each = 4),
  spec = rep(1:4, 2),
  paper_coef = c(-0.014, -0.016, -0.018, -0.018, 0.008, 0.007, 0.006, 0.006),
  paper_se = c(0.008, 0.008, 0.008, 0.007, 0.007, 0.008, 0.007, 0.007),
  paper_n = 34817597
)

rhs <- c(
  "treat_post | wid + year",
  "treat_post | wid + municipality6 + year",
  "treat_post + age + age2 + male + tenure + tenure2 | wid + municipality6 + year",
  "treat_post + bartik + age + age2 + male + tenure + tenure2 | wid + municipality6 + year"
)

res <- list()
for (oc in c("log_hourly_wage", "displaced")) {
  dat <- if (oc == "log_hourly_wage") dw else ds
  for (s in 1:4) {
    log_msg("  ", oc, " spec ", s)
    m <- feols(as.formula(paste(oc, "~", rhs[s])), data = dat, cluster = ~municipality6)
    res[[length(res) + 1]] <- tidy_coefs(m, "^treat_post$", dat)[, `:=`(outcome = oc, spec = s)]
  }
}
res <- merge(rbindlist(res), paper, by = c("outcome", "spec"))
res[, diff := round(coef - paper_coef, 4)]
res[, match := abs(diff) <= 0.003 & (SAMPLE_SHARE < 1 | abs(n_obs / paper_n - 1) <= 0.02)]
write_out(res[, .(outcome, spec, coef, se, paper_coef, paper_se, diff, n_obs, paper_n, n_clusters, match)],
          "03_replication.csv")

cohort <- data.table(
  stat = c("treated_workers", "control_workers", "treated_munis_2014", "control_munis_2014"),
  this_build = c(uniqueN(dw[treat == 1L, wid]), uniqueN(dw[treat == 0L, wid]),
                 uniqueN(load_baseline()[treat == 1L, municipality6]),
                 uniqueN(load_baseline()[treat == 0L, municipality6])),
  paper = c(549598, 4826901, 194, 659)
)
write_out(cohort, "03_replication_cohort.csv")

log_msg("  event study, wage, spec 4")
es <- feols(log_hourly_wage ~ i(year, treat, ref = 2014) + bartik + age + age2 + male + tenure + tenure2 |
              wid + municipality6 + year, data = dw, cluster = ~municipality6)
write_out(tidy_coefs(es, "^year::", dw)[, year := as.integer(sub("year::(\\d{4}).*", "\\1", term))],
          "03_event_study_2010_2018.csv")

log_msg(sprintf("Replication: %d of %d coefficients within 0.003 of the paper", sum(res$match), nrow(res)))
log_msg("=== 03_replicate_baseline done ===")
