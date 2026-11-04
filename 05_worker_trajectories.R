#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# Project: Mariana — RAIS extension (post-2018)
# Script: 05_worker_trajectories.R
# Summary: Worker-level outcomes for the 2014 cohort in the style of Autor, Dorn,
#          Hanson and Song (2014), for referee 1, comments 1, 2 and 4.
#   (a) Cumulative formal earnings 2016..H as a percent of 2014 annual pay
#       (12 x 2014 real monthly pay), zeros for years out of RAIS, split by
#       source: initial employer, same sector, other sector, outside the
#       origin region. Years with any formal job, 2016..H.
#       Worker cross-section: y = b * Doce + baseline controls, clustered by
#       2014 municipality. Horizons H = 2018 (paper window), 2019, last year.
#       Placebo: the same cohort's 2011-2013 earnings (should show b ~ 0).
#   (b) Heterogeneity by 2014 wage tercile, age, tenure, establishment size,
#       and ore mining.
#   (c) Where workers are in 2017 and in the last year: still with the 2014
#       employer, same sector, other sector, outside the region, no formal job.
#   (d) Exit from formal employment by year and treatment.
# Outputs: 05_cumulative_earnings.csv, 05_heterogeneity.csv,
#          05_destinations.csv, 05_exit_rates.csv
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

source("R/utils.R")
suppressPackageStartupMessages(library(fixest))
setFixest_nthreads(N_THREADS)

log_msg("=== 05_worker_trajectories ===")
yrs <- available_years()
T_MAX <- max(yrs)
HORIZONS <- sort(unique(c(2018L, if (T_MAX >= 2019) 2019L, T_MAX)))
HORIZONS <- HORIZONS[HORIZONS <= T_MAX]
TOPCODE_P <- 0.995

base <- load_baseline()
base[, annual_base := 12 * monthly_real]
base[, wage_decile := cut(log_hourly_wage, unique(quantile(log_hourly_wage, 0:10 / 10)),
                          include.lowest = TRUE, labels = FALSE)]
base[, age_bin := cut(age, c(-Inf, seq(20, 60, 5), Inf))]
base[, mining := fifelse(sector_2d == "07", "ore_mining", "other")]

earn_years <- intersect(2011:T_MAX, yrs)
earn <- rbindlist(lapply(earn_years, function(y) {
  read_fst(file.path(WORK_DIR, sprintf("earn_%d.fst", y)), as.data.table = TRUE)
}), use.names = TRUE, fill = TRUE)
SOURCES <- c("earn_total", "earn_initial", "earn_same_sector", "earn_other_sector", "earn_outside_region")

################################################################################
# (a) Cumulative earnings and years employed
################################################################################

cum_outcomes <- function(from, to) {
  e <- earn[year >= from & year <= to]
  s <- e[, c(lapply(.SD, sum), list(years_any = uniqueN(year), years_mg = sum(any_job_mg))),
         by = wid, .SDcols = SOURCES]
  s <- merge(base[, .(wid, annual_base)], s, by = "wid", all.x = TRUE)
  for (v in c(SOURCES, "years_any", "years_mg")) set(s, which(is.na(s[[v]])), v, 0)
  for (v in SOURCES) set(s, j = paste0("cum_", sub("earn_", "", v)), value = 100 * s[[v]] / s$annual_base)
  s[, (SOURCES) := NULL]
  s[, annual_base := NULL]
  s
}

topcode <- function(x, p = TOPCODE_P) pmin(x, quantile(x, p, na.rm = TRUE))

CUM_VARS <- c("cum_total", "cum_initial", "cum_same_sector", "cum_other_sector", "cum_outside_region")
YEAR_VARS <- if (FILE_SCOPE == "national") c("years_any", "years_mg") else "years_mg"

CONTROLS_FE <- "sector_2d + age_bin + tenure_group + size_group + wage_decile + educ_group + male + race"

xs_fit <- function(dat, y, horizon, variant, sample_label = "all") {
  m <- feols(as.formula(paste(y, "~ treat |", CONTROLS_FE)), data = dat, cluster = ~municipality6)
  data.table(outcome = y, horizon = horizon, variant = variant, sample = sample_label,
             coef = coef(m)[["treat"]], se = se(m)[["treat"]], pval = pvalue(m)[["treat"]],
             control_mean = dat[treat == 0L, mean(get(y), na.rm = TRUE)],
             n_obs = nobs(m), n_clusters = uniqueN(dat$municipality6[obs(m)]))
}

make_xs <- function(from, to) {
  s <- merge(base, cum_outcomes(from, to), by = "wid")
  s[, cum_total_raw := cum_total]
  for (v in CUM_VARS) set(s, j = v, value = topcode(s[[v]]))
  s
}

cum_res <- list()
for (H in HORIZONS) {
  s <- make_xs(POST_START, H)
  for (v in c("A_paper", "B_no_paraopeba", "D_no_mining_muni")) {
    if (H <= 2018 && v != "A_paper") next
    sv <- VARIANTS[[v]](s)
    for (y in c(CUM_VARS, "cum_total_raw", YEAR_VARS)) {
      log_msg(sprintf("  %-20s H=%d %s", y, H, v))
      cum_res[[length(cum_res) + 1]] <- xs_fit(sv, y, H, v)
    }
  }
}

# Placebo: same cohort, 2011-2013 (three years, like 2016-2018)
sp <- make_xs(2011L, 2013L)
for (y in c("cum_total", YEAR_VARS)) {
  cum_res[[length(cum_res) + 1]] <- xs_fit(sp, y, 2013L, "A_paper", "placebo_2011_2013")
}
write_out(rbindlist(cum_res), "05_cumulative_earnings.csv")

################################################################################
# (b) Heterogeneity: Doce effect within baseline groups
################################################################################

het_res <- list()
GROUPS <- c("wage_tercile", "age_group", "tenure_group", "size_group", "mining")
for (H in unique(c(2018L, T_MAX))) {
  s <- make_xs(POST_START, H)
  for (g in GROUPS) {
    for (y in c("cum_total", YEAR_VARS[1])) {
      m <- feols(as.formula(paste0(y, " ~ i(", g, ", treat) | ", CONTROLS_FE, " + ", g)),
                 data = s, cluster = ~municipality6)
      ct <- tidy_coefs(m, paste0("^", g, "::"), s)
      ct[, `:=`(group_var = g, group = sub(paste0("^", g, "::(.*):treat$"), "\\1", term),
                outcome = y, horizon = H)]
      n_g <- s[, .(n_group = .N), by = .(group = as.character(get(g)))]
      het_res[[length(het_res) + 1]] <- merge(ct, n_g, by = "group", all.x = TRUE)
    }
  }
}
het <- rbindlist(het_res)
write_out(het[, .(outcome, horizon, group_var, group, coef, se, pval, n_group, n_obs, n_clusters)],
          "05_heterogeneity.csv")

################################################################################
# (c) Where the cohort is: main source of formal earnings in a given year
################################################################################

dest_res <- list()
for (Y in unique(c(2017L, T_MAX))) {
  e <- earn[year == Y, .(wid, main_source)]
  s <- merge(base[, .(wid, treat, mining, annual_base)], e, by = "wid", all.x = TRUE)
  s[is.na(main_source), main_source := "no_formal_job"]
  cum <- cum_outcomes(POST_START, Y)[, .(wid, cum_total)]
  s <- merge(s, cum, by = "wid")
  s[, cum_total := topcode(cum_total)]
  tab <- s[, .(n = .N, mean_cum_total = mean(cum_total)), by = .(treat, mining, main_source)]
  tab[, share := n / sum(n), by = .(treat, mining)]
  tab[, year := Y]
  dest_res[[length(dest_res) + 1]] <- suppress_small(tab, "n", c("mean_cum_total", "share"))
}
write_out(rbindlist(dest_res)[order(year, mining, treat, main_source)], "05_destinations.csv")

################################################################################
# (d) Exit: share of the cohort with no formal job, by year and treatment
################################################################################

panel_presence <- rbindlist(lapply(intersect(BASE_YEAR:T_MAX, yrs), function(y) {
  read_fst(file.path(WORK_DIR, sprintf("panel_mg_%d.fst", y)), columns = c("wid", "year"), as.data.table = TRUE)
}))
exit_res <- list()
for (Y in intersect((BASE_YEAR + 1L):T_MAX, yrs)) {
  pm <- panel_presence[year == Y, wid]
  ea <- earn[year == Y]
  s <- base[, .(wid, treat, mining)]
  s[, `:=`(out_fulltime_mg_panel = as.integer(!wid %chin% pm),
           out_any_mg_job = as.integer(!wid %chin% ea[any_job_mg == 1L, wid]),
           out_any_job = as.integer(!wid %chin% ea$wid))]
  tab <- s[, .(n = .N, out_fulltime_mg_panel = mean(out_fulltime_mg_panel),
               out_any_mg_job = mean(out_any_mg_job), out_any_job = mean(out_any_job)),
           by = .(treat, mining)]
  tab <- rbind(tab, s[, .(mining = "all", n = .N, out_fulltime_mg_panel = mean(out_fulltime_mg_panel),
                          out_any_mg_job = mean(out_any_mg_job), out_any_job = mean(out_any_job)), by = treat])
  tab[, year := Y]
  if (FILE_SCOPE != "national") tab[, out_any_job := NA_real_]
  exit_res[[length(exit_res) + 1]] <- suppress_small(
    tab, "n", c("out_fulltime_mg_panel", "out_any_mg_job", "out_any_job"))
}
write_out(rbindlist(exit_res)[order(mining, treat, year)], "05_exit_rates.csv")

log_msg("=== 05_worker_trajectories done ===")
