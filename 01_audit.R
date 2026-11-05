#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# Project: Mariana — RAIS extension (post-2018)
# Script: 01_audit.R
# Summary: Inventory the local RAIS files before any heavy run. For each year:
#          which files exist, how each needed field maps to a column, parsing
#          quality on a row sample, code distributions, and how well PIS vs CPF
#          link 2014 Minas Gerais workers into later years.
#          Writes aggregates only. Check the log (README step 5) before running 02.
# Runtime: minutes (header + sample reads), longer if AUDIT_LINKAGE = TRUE.
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

source("R/utils.R")

AUDIT_ROWS <- 200000   # rows sampled per file for parsing checks
AUDIT_LINKAGE <- TRUE  # full read of ID columns for 2014 and later years

log_msg("=== 01_audit: RAIS inventory ===")
log_msg("RAIS_DIR = ", RAIS_DIR, " | YEARS = ", min(YEARS), "-", max(YEARS),
        " | WORKER_ID = ", WORKER_ID, " | FILE_SCOPE = ", FILE_SCOPE)
if (!dir.exists(RAIS_DIR)) stop("RAIS_DIR does not exist: ", RAIS_DIR)

deflator <- load_deflator()
missing_defl <- setdiff(YEARS, deflator$year)
if (length(missing_defl)) {
  log_msg("WARNING: no deflator for ", paste(missing_defl, collapse = ", "),
          " — add rows to inputs/ipca_deflator_2025usd.csv")
}

fields_rows <- list()
stats_rows <- list()
code_rows <- list()
missing_rows <- list()

share_tab <- function(x, var, year, file) {
  if (!length(x)) return(NULL)
  t <- as.data.table(table(code = x, useNA = "ifany"))
  t <- t[N >= MIN_CELL]
  t[, `:=`(year = year, file = file, variable = var, share = N / length(x))]
  t[, .(year, file, variable, code = as.character(code), share)]
}

for (y in YEARS) {
  files <- rais_files(y)
  if (!length(files)) {
    log_msg("Year ", y, ": NO FILES FOUND")
    stats_rows[[length(stats_rows) + 1]] <- data.table(year = y, file = NA_character_,
                                                      stat = "n_files", value = 0)
    next
  }
  log_msg("Year ", y, ": ", length(files), " file(s)")
  for (f in files) {
    fb <- basename(f)
    hdr <- tryCatch(read_header(f), error = function(e) { log_msg("  cannot read ", fb, ": ", conditionMessage(e)); NULL })
    if (is.null(hdr)) next
    map <- map_columns(hdr)
    fields_rows[[length(fields_rows) + 1]] <- data.table(
      year = y, file = fb, field = names(RAIS_FIELDS),
      source_column = enc2utf8(vapply(names(RAIS_FIELDS), function(k) if (is.null(map[[k]])) NA_character_ else map[[k]], "")),
      required = names(RAIS_FIELDS) %in% c(REQUIRED_FIELDS, WORKER_ID)
    )
    miss <- setdiff(c(REQUIRED_FIELDS, WORKER_ID), names(map))
    if (length(miss)) {
      log_msg("  ", fb, ": MISSING REQUIRED FIELDS: ", paste(miss, collapse = ", "))
      log_msg("    columns in this file that matched nothing: ",
              paste(setdiff(hdr, unlist(map, use.names = FALSE)), collapse = ", "))
      missing_rows[[length(missing_rows) + 1]] <- data.table(year = y, file = fb,
                                                            missing = paste(miss, collapse = ", "))
    }
    if (!("age" %in% names(map) || "birth_date" %in% names(map))) log_msg("  ", fb, ": no age or birth date")

    raw <- read_cols(f, unlist(map, use.names = FALSE), n_max = AUDIT_ROWS)
    j <- harmonize_jobs(raw, map, y)
    mg <- j[substr(municipality6, 1, 2) == "31"]
    st <- list(
      n_rows_sampled   = nrow(j),
      share_mg         = mean(substr(j$municipality6, 1, 2) == "31", na.rm = TRUE),
      share_muni_na    = mean(is.na(j$municipality6)),
      share_pis_valid  = mean(!is.na(j$pis)),
      share_cpf_valid  = mean(!is.na(j$cpf)),
      share_cnpj_valid = mean(!is.na(j$cnpj)),
      share_wage_na    = mean(is.na(j$wage_avg)),
      share_wage_pos   = mean(j$wage_avg > 0, na.rm = TRUE),
      median_wage_nom  = median(j$wage_avg[j$wage_avg > 0], na.rm = TRUE),
      share_hours_na   = mean(is.na(j$hours)),
      share_hours_ge30 = mean(j$hours >= 30, na.rm = TRUE),
      share_public     = mean(substr(j$nature, 1, 1) == "1", na.rm = TRUE),
      share_sep_na     = mean(is.na(j$sep_cause)),
      share_sep_nonzero = mean(j$sep_cause != "0", na.rm = TRUE),
      share_male_na    = mean(is.na(j$male)),
      share_age_na     = mean(is.na(j$age)),
      mean_age         = mean(j$age, na.rm = TRUE),
      share_tenure_na  = mean(is.na(j$tenure)),
      share_hire_month = mean(!is.na(j$hire_month)),
      share_sep_month  = mean(!is.na(j$sep_month)),
      share_wage_dec   = mean(!is.na(j$wage_dec)),
      share_wage_contract = mean(!is.na(j$wage_contract)),
      share_sector_na  = mean(is.na(j$sector_2d))
    )
    stats_rows[[length(stats_rows) + 1]] <- data.table(year = y, file = fb, stat = names(st),
                                                      value = round(unlist(st), 4))
    code_rows[[length(code_rows) + 1]] <- rbind(
      share_tab(j$sep_cause, "sep_cause", y, fb),
      share_tab(j$contract_type, "contract_type", y, fb),
      share_tab(substr(j$nature, 1, 1), "nature_first_digit", y, fb),
      share_tab(j$education, "education", y, fb),
      share_tab(j$race, "race", y, fb)
    )
    log_msg(sprintf("  %s: sampled %s rows | MG %.2f | PIS ok %.2f | CPF ok %.2f | wage NA %.3f",
                    fb, format(nrow(j), big.mark = ","), st$share_mg,
                    st$share_pis_valid, st$share_cpf_valid, st$share_wage_na))
  }
}

write_out(rbindlist(fields_rows), "01_audit_fields.csv")
write_out(rbindlist(stats_rows), "01_audit_stats.csv")
write_out(rbindlist(code_rows), "01_audit_codes.csv")

if (length(missing_rows)) {
  log_msg("ACTION NEEDED: required fields not found in these files. ",
          "Map them in COLUMN_MAP in config.R (see the 'matched nothing' lines above).")
  m <- rbindlist(missing_rows)
  for (i in seq_len(nrow(m))) log_msg("  ", m$year[i], " ", m$file[i], ": ", m$missing[i])
}

################################################################################
# Linkage: share of 2014 MG workers (valid ID) found anywhere in later files
################################################################################

if (AUDIT_LINKAGE && BASE_YEAR %in% YEARS) {
  log_msg("=== Linkage check (reads ID columns in full) ===")
  read_ids <- function(y, mg_only) {
    out <- lapply(rais_files(y), function(f) {
      map <- map_columns(read_header(f))
      cols <- unlist(map[intersect(c("pis", "cpf", "muni"), names(map))], use.names = FALSE)
      raw <- read_cols(f, cols)
      d <- data.table(
        pis = if (!is.null(map$pis)) clean_id(raw[[map$pis]], 11) else NA_character_,
        cpf = if (!is.null(map$cpf)) clean_id(raw[[map$cpf]], 11) else NA_character_,
        mg  = substr(muni6(raw[[map$muni]]), 1, 2) == "31"
      )
      if (mg_only) d[mg == TRUE] else d
    })
    rbindlist(out)
  }
  ids14 <- read_ids(BASE_YEAR, mg_only = TRUE)
  pis14 <- unique(na.omit(ids14$pis))
  cpf14 <- unique(na.omit(ids14$cpf))
  log_msg("2014 MG: ", format(length(pis14), big.mark = ","), " valid PIS, ",
          format(length(cpf14), big.mark = ","), " valid CPF")
  link <- list()
  for (y in setdiff(YEARS[YEARS > BASE_YEAR], NA)) {
    if (!length(rais_files(y))) next
    idy <- read_ids(y, mg_only = FALSE)
    link[[length(link) + 1]] <- data.table(
      year = y,
      share_2014_pis_found_any = mean(pis14 %chin% idy$pis),
      share_2014_cpf_found_any = mean(cpf14 %chin% idy$cpf),
      share_2014_pis_found_mg  = mean(pis14 %chin% idy[mg == TRUE, pis]),
      share_2014_cpf_found_mg  = mean(cpf14 %chin% idy[mg == TRUE, cpf]),
      share_rows_pis_valid     = mean(!is.na(idy$pis)),
      share_rows_cpf_valid     = mean(!is.na(idy$cpf))
    )
    log_msg(sprintf("  %d: 2014 PIS found %.3f | 2014 CPF found %.3f", y,
                    link[[length(link)]]$share_2014_pis_found_any,
                    link[[length(link)]]$share_2014_cpf_found_any))
    rm(idy); gc()
  }
  write_out(rbindlist(link), "01_audit_linkage.csv")
  log_msg("If CPF links clearly better than PIS after 2018, set WORKER_ID <- \"cpf\" in config.R.")
}

log_msg("=== 01_audit done. Check this log against step 5 of the README before running run_all.R. ===")
