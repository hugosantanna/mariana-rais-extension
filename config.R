#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# config.R — the only file you should need to edit.
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

# Folder holding the identified RAIS vínculos files (must include PIS or CPF).
RAIS_DIR <- "/path/to/rais"

# Files for a given year. Default: every data file under RAIS_DIR whose name
# contains the year. Edit if your naming differs, for example
#   rais_files <- function(year) file.path(RAIS_DIR, sprintf("MG%d.txt", year))
#   rais_files <- function(year) Sys.glob(file.path(RAIS_DIR, year, "*.txt"))
# Supported: .txt/.csv (optionally .gz), .fst, .parquet, .dta, .rds.
# Extract .7z/.zip archives first.
rais_files <- function(year) {
  f <- list.files(RAIS_DIR, pattern = as.character(year), full.names = TRUE, recursive = TRUE)
  f <- f[grepl("\\.(txt|csv|gz|fst|parquet|dta|rds)$", f, ignore.case = TRUE)]
  # One file per state (ac2010id.csv, ..., mg2010id.csv): with FILE_SCOPE "mg",
  # read only the Minas Gerais files.
  uf <- tolower(substr(basename(f), 1, 2))
  states <- c("ac", "al", "am", "ap", "ba", "ce", "df", "es", "go", "ma", "mg", "ms", "mt", "pa",
              "pb", "pe", "pi", "pr", "rj", "rn", "ro", "rr", "rs", "sc", "se", "sp", "to")
  if (FILE_SCOPE == "mg" && "mg" %in% uf && any(uf %in% setdiff(states, "mg"))) f <- f[uf == "mg"]
  f
}

# Years to process. 2010-2018 reproduce the paper; later years extend it.
# 2014 is required (cohort year).
YEARS <- 2010:2023

# Worker identifier used to link years: "pis" or "cpf".
# Run 01_audit.R first — it reports which one links 2014 workers better.
WORKER_ID <- "pis"

# "mg":       keep Minas Gerais records only (what the paper does).
# "national": also follow 2014 Minas Gerais workers into jobs in other states.
#             Needs files covering all states; slower, but makes "left formal
#             employment" distinguishable from "moved out of state".
FILE_SCOPE <- "mg"

# Share of the 2014 cohort to keep (selected by worker ID, reproducible).
# Use 0.05 for a first test run; 1 for the real run.
SAMPLE_SHARE <- 1

# Text-file options (the Ministry's .txt files are ';'-separated, Latin-1).
TXT_SEP <- "auto"
TXT_ENCODING <- "Latin-1"

# Only if 01_audit.R reports a field as missing although the column exists
# under an unusual name: map it here, e.g. list(sep_cause = "causa_deslig").
# A name only applies to files that have it, so year-specific names are fine.
# Several candidates also work: list(sep_cause = c("causa_deslig", "motivo_desl")).
COLUMN_MAP <- list()

# Intermediate worker-level files. Contains identified microdata:
# keep it on the secure machine and never share it.
WORK_DIR <- "work"

# Aggregated results to send back (coefficients, rates, counts >= MIN_CELL).
OUT_DIR <- "outputs"
MIN_CELL <- 10

# Threads for data.table / fixest.
N_THREADS <- 4
