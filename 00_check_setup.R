#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# Project: Mariana — RAIS extension (post-2018)
# Script: 00_check_setup.R
# Summary: Quick check before anything heavy. Confirms the working folder,
#          R version, packages, the RAIS folder, and that each year in YEARS
#          has files. Reads file names only, so it takes seconds.
# Usage:   open rais_extension.Rproj, then source("00_check_setup.R")
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

if (!file.exists("config.R")) {
  stop("config.R not found. Open rais_extension.Rproj first, so R starts inside the rais_extension folder.")
}
source("config.R")

report <- function(pass, what, fix = "") {
  message(sprintf("  %-4s %s", if (pass) "OK" else "FIX", what))
  if (!pass && nzchar(fix)) message("       -> ", fix)
  pass
}

message("\n1. R version")
r_ok <- report(getRversion() >= "4.2.0", R.version.string, "R 4.2 or newer is needed. Ask CDIA to update it.")

message("\n2. Packages")
pkgs <- c("data.table", "fst", "fixest", "stringi", "ggplot2")
have <- vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)
for (p in pkgs) report(have[[p]], p)
if (!all(have)) {
  message('       -> Install the missing ones by pasting this into the Console:\n          install.packages(c("',
          paste(pkgs[!have], collapse = '", "'), '"))')
}

message("\n3. RAIS folder")
dir_ok <- report(dir.exists(RAIS_DIR), RAIS_DIR,
                 "Fix RAIS_DIR in config.R. Use forward slashes, e.g. \"T:/Projetos/RAIS\".")

years_ok <- FALSE
extra_ok <- TRUE
if (dir_ok) {
  message("\n4. Files for each year in YEARS")
  report(2014 %in% YEARS, "2014 is in YEARS", "Add 2014 to YEARS in config.R. It is the cohort year.")
  per_year <- lapply(YEARS, rais_files)
  n <- lengths(per_year)
  for (i in seq_along(YEARS)) {
    shown <- if (n[i]) head(basename(per_year[[i]]), 2) else "(no files)"
    more <- if (n[i] > 2) sprintf(" (+%d more)", n[i] - 2) else ""
    report(n[i] > 0, sprintf("%d  %s%s", YEARS[i], paste(shown, collapse = ", "), more),
           "No file found. Remove this year from YEARS, or adjust rais_files() in config.R.")
  }
  years_ok <- all(n > 0) && 2014 %in% YEARS

  defl <- read.csv(file.path("inputs", "ipca_deflator_2025usd.csv"))
  no_defl <- setdiff(YEARS, defl$year)
  report(!length(no_defl), "wage deflator covers every year",
         paste0("No deflator for ", paste(no_defl, collapse = ", "), ". Tell Hugo before running."))

  ext <- tolower(unlist(per_year))
  need_extra <- c(arrow = any(grepl("\\.parquet$", ext)), haven = any(grepl("\\.dta$", ext)),
                  dplyr = any(grepl("\\.(parquet|dta)$", ext)), R.utils = any(grepl("\\.gz$", ext)))
  need_extra <- names(need_extra)[need_extra]
  if (length(need_extra)) {
    message("\n5. Extra packages for these file types")
    have_extra <- vapply(need_extra, requireNamespace, logical(1), quietly = TRUE)
    for (p in need_extra) report(have_extra[[p]], p)
    extra_ok <- all(have_extra)
    if (!extra_ok) {
      message('       -> install.packages(c("', paste(need_extra[!have_extra], collapse = '", "'), '"))')
    }
  }
}

here_path <- normalizePath(".", winslash = "/")
if (grepl("^C:", here_path, ignore.case = TRUE)) {
  message("\nNOTE  This folder is on the C: drive (", here_path, ").\n",
          "      On the Insper VM, C: is wiped after some days without logging in.\n",
          "      Move the folder to your Storage folder or the project drive.")
}

if (r_ok && all(have) && dir_ok && years_ok && extra_ok) {
  message("\nAll set. Next step: source(\"01_audit.R\")\n")
} else {
  message("\nFix the lines marked FIX, then run this check again.\n")
}
