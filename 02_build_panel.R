#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# Project: Mariana — RAIS extension (post-2018)
# Script: 02_build_panel.R
# Summary: Build the worker-year panel with the paper's definitions (script 01):
#          private-sector formal jobs, positive pay, CNAE 84 out, full-time
#          (contract type 1/2 or >= 30 weekly hours), one primary job per
#          worker-year (highest pay, then longest tenure), real hourly wage in
#          2025 USD trimmed to [0.1, 10000].
#          Treatment follows the paper: the worker's 2014 municipality lies in
#          the Doce basin (origin-based ITT, 2014 cohort).
# Writes to WORK_DIR (identified microdata, stays on this machine):
#   panel_mg_<year>.fst          2014-cohort workers' primary MG job per year
#   earn_<year>.fst              2014-cohort workers' annual earnings by source
#   baseline_2014.fst            2014-cohort baseline characteristics
#   counts_muni_sector_year.fst  all MG workers by municipality x sector x year (Bartik)
# Writes to OUT_DIR: 02_build_summary.csv (aggregates only).
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

source("R/utils.R")

MINING_MUNI_SHARE <- 0.05   # 2014 ore-mining (CNAE 07) employment share defining a mining municipality

log_msg("=== 02_build_panel ===")
if (!BASE_YEAR %in% YEARS) stop("YEARS must include ", BASE_YEAR)
deflator <- load_deflator()
if (length(setdiff(YEARS, deflator$year))) {
  stop("Missing deflator for ", paste(setdiff(YEARS, deflator$year), collapse = ", "))
}
xwalk <- load_crosswalk()
mg_munis <- xwalk$municipality6

################################################################################
# Read and harmonize one year (all files), keeping MG records and, in national
# scope, out-of-state records of cohort workers.
################################################################################

read_year <- function(y, cohort_ids = NULL) {
  files <- rais_files(y)
  if (!length(files)) stop("No RAIS files for ", y)
  parts <- lapply(files, function(f) {
    map <- map_columns(read_header(f))
    miss <- setdiff(c(REQUIRED_FIELDS, WORKER_ID), names(map))
    if (length(miss)) stop(basename(f), ": missing fields ", paste(miss, collapse = ", "),
                           " — see 01_audit output and COLUMN_MAP in config.R")
    j <- harmonize_jobs(read_cols(f, unlist(map, use.names = FALSE)), map, y)
    j[, wid := get(WORKER_ID)]
    j <- j[!is.na(wid)]
    j[, in_mg := municipality6 %chin% mg_munis]
    if (FILE_SCOPE == "national" && !is.null(cohort_ids)) {
      j[in_mg | wid %chin% cohort_ids]
    } else {
      j[in_mg == TRUE]
    }
  })
  j <- rbindlist(parts, use.names = TRUE)
  j <- apply_core_filters(j)
  d <- deflator[year == y, deflator]
  j[, earn_year := wage_avg * months_worked(j) / d]
  j[, deflator := d]
  j
}

# Primary MG job per worker-year, as in script 01.
primary_panel <- function(j) {
  p <- apply_fulltime(j[in_mg == TRUE])
  setorder(p, wid, -wage_avg, -tenure, na.last = TRUE)
  p <- p[!duplicated(wid)]
  p[, hourly_wage := wage_avg / (hours * WEEKS_PER_MONTH) / deflator]
  p <- p[!is.na(hourly_wage) & hourly_wage >= MIN_HOURLY & hourly_wage <= MAX_HOURLY]
  p[, log_hourly_wage := log(hourly_wage)]
  p[, log_hourly_dec := fifelse(!is.na(wage_dec) & wage_dec > 0,
                                log(wage_dec / (hours * WEEKS_PER_MONTH) / deflator), NA_real_)]
  p[, monthly_real := wage_avg / deflator]
  p[, educ_group := educ_bin(education)]
  p
}

PANEL_COLS <- c("wid", "year", "municipality6", "cnpj", "firm_root", "sector_2d", "occupation_3d",
                "log_hourly_wage", "log_hourly_dec", "age", "male", "tenure", "educ_group",
                "race", "sep_cause")

# Annual earnings of cohort workers by source, relative to the 2014 job:
# initial employer (same CNPJ root) > outside origin region (other basin group
# or other state) > same 2-digit sector > other sector.
earnings_by_source <- function(j, base) {
  e <- merge(j[, .(wid, year, in_mg, municipality6, firm_root, sector_2d, earn_year)],
             base[, .(wid, b_root = firm_root, b_sector = sector_2d, b_treat = treat)], by = "wid")
  e <- merge(e, xwalk[, .(municipality6, doce)], by = "municipality6", all.x = TRUE)
  e[, source := fcase(
    firm_root == b_root, "initial_employer",
    !in_mg | is.na(doce) | doce != b_treat, "outside_region",
    sector_2d == b_sector, "same_sector",
    default = "other_sector")]
  w <- dcast(e, wid + year ~ source, value.var = "earn_year", fun.aggregate = sum, fill = 0)
  for (s in c("initial_employer", "outside_region", "same_sector", "other_sector")) {
    if (!s %in% names(w)) w[, (s) := 0]
  }
  setnames(w, c("initial_employer", "outside_region", "same_sector", "other_sector"),
           paste0("earn_", c("initial", "outside_region", "same_sector", "other_sector")))
  w[, earn_total := earn_initial + earn_outside_region + earn_same_sector + earn_other_sector]
  src <- e[, .(earn = sum(earn_year)), by = .(wid, source)][order(wid, -earn)][!duplicated(wid)]
  w <- merge(w, src[, .(wid, main_source = source)], by = "wid", all.x = TRUE)
  mg <- e[in_mg == TRUE, .(any_job_mg = 1L), by = wid]
  w <- merge(w, mg, by = "wid", all.x = TRUE)
  w[is.na(any_job_mg), any_job_mg := 0L]
  w[, any_job := 1L]
  w
}

################################################################################
# 2014 first: defines the cohort and the baseline
################################################################################

counts <- list()
summary_rows <- list()

log_msg("Reading ", BASE_YEAR, " ...")
j14 <- read_year(BASE_YEAR)
p14 <- primary_panel(j14)
counts[[as.character(BASE_YEAR)]] <- p14[, .(n = .N), by = .(municipality6, sector_2d, year)]

base <- merge(p14, xwalk[, .(municipality6, treat = doce, paraopeba_10km, alto_sao_francisco)],
              by = "municipality6")
estab <- p14[, .(estab_n = .N), by = cnpj]
mining <- p14[, .(mining_share = mean(sector_2d == "07")), by = municipality6]
base <- merge(base, estab, by = "cnpj", all.x = TRUE)
base <- merge(base, mining, by = "municipality6", all.x = TRUE)
base[, mining_muni := as.integer(mining_share >= MINING_MUNI_SHARE)]
base[, wage_tercile := cut(log_hourly_wage, quantile(log_hourly_wage, c(0, 1/3, 2/3, 1)),
                           labels = c("low", "mid", "high"), include.lowest = TRUE)]
base[, age_group := cut(age, c(-Inf, 29, 44, Inf), labels = c("under30", "30to44", "45plus"))]
base[, tenure_group := cut(tenure, c(-Inf, 23.99, 59.99, Inf), labels = c("under2y", "2to5y", "over5y"))]
base[, size_group := cut(estab_n, c(0, 19, 99, 499, Inf), labels = c("1to19", "20to99", "100to499", "500plus"))]
log_msg("2014 cohort: ", format(nrow(base), big.mark = ","), " workers; treated ",
        format(sum(base$treat), big.mark = ","), "; mining municipalities ",
        uniqueN(base[mining_muni == 1L, municipality6]))

if (SAMPLE_SHARE < 1) {
  draw <- suppressWarnings(as.numeric(substr(base$wid, 3, 10)))
  if (anyNA(draw)) draw <- frank(base$wid, ties.method = "dense")  # pseudonymized IDs
  keep <- (draw %% 10000) < SAMPLE_SHARE * 10000
  base <- base[keep]
  log_msg("SAMPLE_SHARE = ", SAMPLE_SHARE, ": keeping ", format(nrow(base), big.mark = ","), " cohort workers")
}
cohort_ids <- base$wid

base_out <- base[, .(wid, municipality6, cnpj, firm_root, sector_2d, occupation_3d, treat,
                     paraopeba_10km, alto_sao_francisco, mining_muni, mining_share,
                     log_hourly_wage, monthly_real, age, male, tenure, educ_group, race,
                     estab_n, wage_tercile, age_group, tenure_group, size_group)]
write_fst(base_out, file.path(WORK_DIR, "baseline_2014.fst"), compress = 60)

write_fst(p14[wid %chin% cohort_ids, ..PANEL_COLS], file.path(WORK_DIR, sprintf("panel_mg_%d.fst", BASE_YEAR)), compress = 60)
write_fst(earnings_by_source(j14[wid %chin% cohort_ids], base_out),
          file.path(WORK_DIR, sprintf("earn_%d.fst", BASE_YEAR)), compress = 60)
summary_rows[[1]] <- data.table(year = BASE_YEAR, mg_primary_records = nrow(p14),
                                cohort_in_mg_panel = nrow(base), cohort_any_job = nrow(base),
                                municipalities = uniqueN(p14$municipality6))
rm(j14, p14); gc()

################################################################################
# Remaining years
################################################################################

for (y in setdiff(sort(YEARS), BASE_YEAR)) {
  log_msg("Reading ", y, " ...")
  j <- read_year(y, cohort_ids)
  p <- primary_panel(j)
  counts[[as.character(y)]] <- p[, .(n = .N), by = .(municipality6, sector_2d, year)]
  pc <- p[wid %chin% cohort_ids, ..PANEL_COLS]
  write_fst(pc, file.path(WORK_DIR, sprintf("panel_mg_%d.fst", y)), compress = 60)
  ec <- earnings_by_source(j[wid %chin% cohort_ids], base_out)
  write_fst(ec, file.path(WORK_DIR, sprintf("earn_%d.fst", y)), compress = 60)
  summary_rows[[length(summary_rows) + 1]] <- data.table(
    year = y, mg_primary_records = nrow(p), cohort_in_mg_panel = nrow(pc),
    cohort_any_job = nrow(ec), municipalities = uniqueN(p$municipality6))
  log_msg(sprintf("  %d: %s MG primary records | %s cohort workers in MG panel | %s with any job",
                  y, format(nrow(p), big.mark = ","), format(nrow(pc), big.mark = ","),
                  format(nrow(ec), big.mark = ",")))
  rm(j, p, pc, ec); gc()
}

write_fst(rbindlist(counts), file.path(WORK_DIR, "counts_muni_sector_year.fst"), compress = 60)

summ <- rbindlist(summary_rows)[order(year)]
summ[, `:=`(cohort_size = nrow(base), treated = sum(base$treat), control = sum(base$treat == 0L),
            treated_munis = uniqueN(base[treat == 1L, municipality6]),
            control_munis = uniqueN(base[treat == 0L, municipality6]),
            file_scope = FILE_SCOPE, id_variable = WORKER_ID, sample_share = SAMPLE_SHARE)]
write_out(summ, "02_build_summary.csv")
log_msg("=== 02_build_panel done ===")
