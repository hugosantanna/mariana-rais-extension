#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# Project: Mariana — RAIS extension (post-2018)
# Script: R/utils.R
# Summary: Shared helpers. Column harmonization across RAIS layouts, file
#          readers, variable construction (identical to the paper's script 01),
#          analysis-panel assembly, and disclosure checks on outputs.
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

suppressPackageStartupMessages({
  library(data.table)
  library(fst)
  library(stringi)
})

if (!file.exists("config.R")) {
  stop("Run these scripts from inside the rais_extension/ folder (config.R not found).")
}
source("config.R")

setDTthreads(N_THREADS)
dir.create(WORK_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

EVENT_YEAR <- 2015L       # Fundão collapse, 5 Nov 2015
POST_START <- 2016L       # 2015 is a transition year, as in the paper
BASE_YEAR  <- 2014L       # cohort year and event-study reference year
BARTIK_BASE <- 2010L
MIN_HOURLY <- 0.1         # trimming bounds on real hourly wage (2025 USD), as in the paper
MAX_HOURLY <- 10000
WEEKS_PER_MONTH <- 4.33

################################################################################
# Logging
################################################################################

LOG_FILE <- file.path(OUT_DIR, "run_log.txt")

log_msg <- function(...) {
  msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", paste0(...))
  message(msg)
  cat(msg, "\n", file = LOG_FILE, append = TRUE, sep = "")
}

################################################################################
# Column dictionary
# Each canonical field lists the labels it goes by across RAIS layouts:
# the Ministry's raw .txt headers, lower-cased/de-spaced variants, the English
# harmonized names used in the paper's files, and Base dos Dados-style names.
# Matching ignores case, accents, spaces and punctuation.
################################################################################

RAIS_FIELDS <- list(
  muni          = c("Município", "Municipio", "MUNI", "id_municipio", "codemun"),
  cnpj          = c("CNPJ / CEI", "CNPJ CEI", "CNPJ", "PLANT_ID", "id_estabelecimento", "cnpj_cei"),
  cnpj_root     = c("CNPJ Raiz", "cnpj_raiz", "FIRM_ID"),
  pis           = c("PIS", "id_pis", "pis_pasep"),
  cpf           = c("CPF", "id_cpf"),
  wage_avg      = c("Vl Remun Média Nom", "Vl Remun Media Nom", "Vl Rem Média Nom",
                    "EARN_AVG_MONTH_NOM", "valor_remuneracao_media_nominal"),
  wage_dec      = c("Vl Remun Dezembro Nom", "Vl Rem Dezembro Nom", "EARN_DEC_NOM",
                    "valor_remuneracao_dezembro_nominal"),
  wage_contract = c("Vl Salário Contratual", "Vl Salario Contratual", "CONTRACT_SALARY",
                    "valor_salario_contratual"),
  hours         = c("Qtd Hora Contr", "NUM_HOURS_CONTRACTED", "quantidade_horas_contratadas"),
  sex           = c("Sexo Trabalhador", "Sexo", "GENDER", "sexo"),
  race          = c("Raça Cor", "Raca Cor", "RACE_v2006", "raca_cor"),
  age           = c("Idade", "AGE"),
  birth_date    = c("Data de Nascimento", "Data Nascimento", "DATE_OF_BIRTH", "data_nascimento"),
  educ          = c("Escolaridade após 2005", "Escolaridade apos 2005", "Grau Instrução após 2005",
                    "EDUC_v2005", "grau_instrucao_apos_2005"),
  tenure        = c("Tempo Emprego", "TENURE_MONTHS", "tempo_emprego"),
  hire_date     = c("Data Admissão Declarada", "Data Admissao Declarada", "Data Admissão",
                    "HIRE_DATE", "data_admissao"),
  hire_month    = c("Mês Admissão", "Mes Admissao", "mes_admissao"),
  contract_type = c("Tipo Vínculo", "Tipo Vinculo", "CONTRACT_TYPE", "tipo_vinculo"),
  sep_month     = c("Mês Desligamento", "Mes Desligamento", "MONTH_OF_SEP", "mes_desligamento"),
  sep_cause     = c("Motivo Desligamento", "CAUSE_OF_SEP", "motivo_desligamento"),
  cbo           = c("CBO Ocupação 2002", "CBO Ocupacao 2002", "OCCUP_CBO2002", "cbo_2002"),
  cnae          = c("CNAE 2.0 Classe", "CNAE20_CLASS", "cnae_2", "cnae_2_classe"),
  nature        = c("Natureza Jurídica", "Natureza Juridica", "LEGAL_NATURE_CONCLA2002",
                    "natureza_juridica"),
  active_dec    = c("Vínculo Ativo 31/12", "Vinculo Ativo 31/12", "EMP_ON_DEC31", "vinculo_ativo_3112"),
  estab_size    = c("Tamanho Estabelecimento", "ESTAB_SIZE", "tamanho_estabelecimento")
)

REQUIRED_FIELDS <- c("muni", "cnpj", "wage_avg", "hours", "sep_cause", "cnae",
                     "nature", "contract_type", "sex", "tenure")

# Two keys per label: accent-transliterated, and raw-bytes-only (survives a
# header read under the wrong encoding, where accented bytes get mangled).
key_translit <- function(x) {
  x <- stri_trans_general(enc2utf8(as.character(x)), "Latin-ASCII")
  gsub("[^a-z0-9]", "", tolower(x))
}
key_bytes <- function(x) tolower(gsub("[^A-Za-z0-9]", "", as.character(x), useBytes = TRUE))

# Map canonical field -> column name in a given header.
map_columns <- function(header) {
  hk1 <- suppressWarnings(key_translit(header))
  hk2 <- key_bytes(header)
  out <- list()
  for (f in names(RAIS_FIELDS)) {
    if (!is.null(COLUMN_MAP[[f]])) {
      if (!COLUMN_MAP[[f]] %in% header) stop("COLUMN_MAP$", f, " = '", COLUMN_MAP[[f]], "' not in file header")
      out[[f]] <- COLUMN_MAP[[f]]
      next
    }
    labels <- RAIS_FIELDS[[f]]
    lk <- unique(c(key_translit(labels), key_bytes(labels)))
    hit <- which(hk1 %in% lk | hk2 %in% lk)
    if (length(hit) >= 1) out[[f]] <- header[hit[1]]
  }
  out
}

################################################################################
# File readers
################################################################################

file_kind <- function(path) {
  p <- tolower(path)
  if (grepl("\\.fst$", p)) return("fst")
  if (grepl("\\.parquet$", p)) return("parquet")
  if (grepl("\\.dta$", p)) return("dta")
  if (grepl("\\.rds$", p)) return("rds")
  if (grepl("\\.(txt|csv)(\\.gz)?$", p)) return("text")
  stop("Unsupported file type: ", path, " (extract .7z/.zip archives first)")
}

read_header <- function(path) {
  switch(file_kind(path),
    fst     = fst::metadata_fst(path)$columnNames,
    parquet = names(arrow::open_dataset(path)$schema),
    dta     = names(haven::read_dta(path, n_max = 0)),
    rds     = names(readRDS(path)),
    text    = names(fread(path, nrows = 0, sep = TXT_SEP, encoding = TXT_ENCODING))
  )
}

# Read selected columns; n_max limits rows (used by the audit).
read_cols <- function(path, cols, n_max = Inf) {
  kind <- file_kind(path)
  dt <- switch(kind,
    fst = {
      to <- if (is.finite(n_max)) min(n_max, fst::metadata_fst(path)$nrOfRows) else NULL
      as.data.table(read_fst(path, columns = cols, to = to))
    },
    parquet = {
      x <- as.data.table(arrow::read_parquet(path, col_select = dplyr::all_of(cols)))
      if (is.finite(n_max)) x[seq_len(min(.N, n_max))] else x
    },
    dta = as.data.table(haven::zap_labels(haven::read_dta(path, col_select = dplyr::all_of(cols),
                                                          n_max = n_max))),
    rds = {
      x <- as.data.table(readRDS(path))[, ..cols]
      if (is.finite(n_max)) x[seq_len(min(.N, n_max))] else x
    },
    text = fread(path, select = cols, colClasses = "character", sep = TXT_SEP,
                 encoding = TXT_ENCODING, nrows = if (is.finite(n_max)) n_max else Inf,
                 showProgress = FALSE)
  )
  dt
}

################################################################################
# Parsers
################################################################################

as_chr <- function(x) {
  if (is.numeric(x)) format(x, scientific = FALSE, trim = TRUE, digits = 15) else as.character(x)
}

to_num <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  x <- trimws(as.character(x))
  suppressWarnings(as.numeric(gsub(",", ".", x, fixed = TRUE)))
}

pad_digits <- function(x, width) {
  x <- gsub("[^0-9]", "", as_chr(x))
  n <- nchar(x)
  x[n == 0] <- NA_character_
  short <- !is.na(x) & n < width
  x[short] <- paste0(strrep("0", width - n[short]), x[short])
  x
}

clean_id <- function(x, width) {
  x <- pad_digits(x, width)
  x[!is.na(x) & grepl("^0+$", x)] <- NA_character_
  x
}

muni6 <- function(x) {
  d <- gsub("[^0-9]", "", as_chr(x))
  fifelse(nchar(d) >= 6, substr(d, 1, 6), NA_character_)
}

# Dates come as ddmmyyyy strings, numbers that lost a leading zero, or Date.
parse_dmy <- function(x) {
  if (inherits(x, "Date")) return(x)
  d <- pad_digits(x, 8)
  as.Date(d, format = "%d%m%Y")
}

educ_bin <- function(educ) {
  e <- suppressWarnings(as.numeric(educ))
  fcase(e %in% 1:4, "low", e %in% 5:7, "mid", e %in% 8:11, "high", default = NA_character_)
}

# Separation cause: "0"/"00" = no separation. Non-numeric codes (e.g. "{ñ class}")
# are treated as missing rather than as separations.
clean_sep_cause <- function(x) {
  s <- trimws(as_chr(x))
  s[s == "" | toupper(s) == "NA" | !grepl("^[0-9]+$", s)] <- NA_character_
  s[!is.na(s) & grepl("^0+$", s)] <- "0"
  s <- sub("^0+([1-9])", "\\1", s)
  s
}

################################################################################
# Deflator and crosswalk
################################################################################

load_deflator <- function() {
  d <- fread("inputs/ipca_deflator_2025usd.csv")
  d[, year := as.integer(year)]
  d
}

load_crosswalk <- function() {
  x <- fread("inputs/municipality_crosswalk.csv", colClasses = list(character = "municipality6"),
             encoding = "UTF-8")
  x[, .(municipality6, doce, alto_sao_francisco, paraopeba_10km, dam_dist_km, river_dist_km)]
}

################################################################################
# Harmonize one raw file-year into job-level records (all records, all states)
################################################################################

harmonize_jobs <- function(raw, map, year) {
  g <- function(f) if (!is.null(map[[f]])) raw[[map[[f]]]] else NULL
  n <- nrow(raw)
  na_chr <- rep(NA_character_, n)
  na_num <- rep(NA_real_, n)

  age <- if (!is.null(g("age"))) to_num(g("age")) else if (!is.null(g("birth_date"))) {
    year - as.integer(format(parse_dmy(g("birth_date")), "%Y"))
  } else na_num

  hire <- if (!is.null(g("hire_date"))) parse_dmy(g("hire_date")) else as.Date(na_chr)
  hire_m <- if (!is.null(g("hire_month"))) to_num(g("hire_month")) else na_num
  hire_m <- fifelse(is.na(hire_m) & !is.na(hire) & as.integer(format(hire, "%Y")) == year,
                    as.numeric(format(hire, "%m")), hire_m)
  hire_m <- fifelse(!is.na(hire) & as.integer(format(hire, "%Y")) < year, 1, hire_m)

  sex <- toupper(trimws(as_chr(g("sex"))))
  sep_m <- if (!is.null(g("sep_month"))) to_num(g("sep_month")) else na_num

  cnpj <- clean_id(g("cnpj"), 14)
  root <- if (!is.null(g("cnpj_root"))) clean_id(g("cnpj_root"), 8) else substr(cnpj, 1, 8)

  data.table(
    pis           = if (!is.null(g("pis"))) clean_id(g("pis"), 11) else na_chr,
    cpf           = if (!is.null(g("cpf"))) clean_id(g("cpf"), 11) else na_chr,
    year          = as.integer(year),
    municipality6 = muni6(g("muni")),
    cnpj          = cnpj,
    firm_root     = root,
    wage_avg      = to_num(g("wage_avg")),
    wage_dec      = if (!is.null(g("wage_dec"))) to_num(g("wage_dec")) else na_num,
    wage_contract = if (!is.null(g("wage_contract"))) to_num(g("wage_contract")) else na_num,
    hours         = to_num(g("hours")),
    male          = fifelse(sex %chin% c("1", "01", "M", "MASCULINO"), 1L,
                    fifelse(sex %chin% c("2", "02", "F", "FEMININO"), 0L, NA_integer_)),
    race          = if (!is.null(g("race"))) pad_digits(g("race"), 2) else na_chr,
    age           = age,
    education     = if (!is.null(g("educ"))) pad_digits(g("educ"), 2) else na_chr,
    tenure        = to_num(g("tenure")),
    hire_month    = hire_m,
    contract_type = trimws(as_chr(g("contract_type"))),
    sep_month     = sep_m,
    sep_cause     = clean_sep_cause(g("sep_cause")),
    occupation_3d = if (!is.null(g("cbo"))) substr(pad_digits(g("cbo"), 6), 1, 3) else na_chr,
    sector_2d     = substr(pad_digits(g("cnae"), 5), 1, 2),
    nature        = gsub("[^0-9]", "", as_chr(g("nature"))),
    estab_size    = if (!is.null(g("estab_size"))) to_num(g("estab_size")) else na_num
  )
}

# Core sample filters from the paper's script 01: positive pay, private sector
# (legal nature not 1xxx), valid establishment, public administration (CNAE 84) out.
apply_core_filters <- function(j) {
  j[!is.na(wage_avg) & wage_avg > 0 &
      !is.na(nature) & substr(nature, 1, 1) != "1" &
      !is.na(cnpj) &
      !is.na(sector_2d) & sector_2d != "84"]
}

# Full-time rule from script 01: contract type coded 1/2, or >= 30 weekly hours.
# With the Ministry's raw two-digit codes (10, 15, ...) this reduces to hours >= 30.
apply_fulltime <- function(j) {
  ct <- j$contract_type
  j[(ct %chin% c("1", "2", "01", "02")) | (!is.na(hours) & hours >= 30)]
}

# Months worked in the year, for annual earnings.
months_worked <- function(j) {
  start <- fifelse(is.na(j$hire_month), 1, pmin(pmax(j$hire_month, 1), 12))
  end <- fifelse(!is.na(j$sep_month) & j$sep_month >= 1 & j$sep_month <= 12, j$sep_month, 12)
  pmax(end - start + 1, 0)
}

################################################################################
# Analysis panel (used by scripts 03-05)
################################################################################

id_col <- function() WORKER_ID

# Bartik shift-share: baseline (2010) municipality sector shares x statewide
# sector employment growth relative to 2010. Identical to scripts 05/05d.
make_bartik <- function(counts, base_year = BARTIK_BASE) {
  base <- counts[year == base_year, .(emp = sum(n)), by = .(municipality6, sector_2d)]
  base[, share := emp / sum(emp), by = municipality6]
  st <- counts[, .(emp_s = sum(n)), by = .(sector_2d, year)]
  st <- merge(st, st[year == base_year, .(sector_2d, emp_base = emp_s)], by = "sector_2d", all.x = TRUE)
  st[, growth := fifelse(!is.na(emp_base) & emp_base > 0, (emp_s - emp_base) / emp_base, 0)]
  b <- merge(base[, .(municipality6, sector_2d, share)], st[, .(sector_2d, year, growth)],
             by = "sector_2d", allow.cartesian = TRUE)
  b[, .(bartik = sum(share * growth, na.rm = TRUE)), by = .(municipality6, year)]
}

load_panel <- function(years) {
  files <- file.path(WORK_DIR, sprintf("panel_mg_%d.fst", years))
  files <- files[file.exists(files)]
  if (!length(files)) stop("No panel files in ", WORK_DIR, " — run 02_build_panel.R first")
  rbindlist(lapply(files, read_fst, as.data.table = TRUE), use.names = TRUE, fill = TRUE)
}

load_counts <- function() read_fst(file.path(WORK_DIR, "counts_muni_sector_year.fst"), as.data.table = TRUE)

load_baseline <- function() read_fst(file.path(WORK_DIR, "baseline_2014.fst"), as.data.table = TRUE)

available_years <- function() {
  f <- list.files(WORK_DIR, pattern = "^panel_mg_\\d{4}\\.fst$")
  sort(as.integer(sub("panel_mg_(\\d{4})\\.fst", "\\1", f)))
}

# Wage/separation panel for the 2014 cohort: treatment fixed at 2014 location.
build_analysis_panel <- function(years) {
  p <- load_panel(years)
  base <- load_baseline()
  p <- merge(p, base[, .(wid, treat, paraopeba_10km, alto_sao_francisco, mining_muni)],
             by = "wid")
  p[, post := as.integer(year >= POST_START)]
  p[, treat_post := treat * post]
  p[, `:=`(age2 = age^2, tenure2 = tenure^2)]
  p[, displaced := fifelse(is.na(sep_cause), NA_integer_,
                   fifelse(sep_cause == "0" | sep_cause %chin% c("30", "31"), 0L, 1L))]
  p <- merge(p, make_bartik(load_counts()), by = c("municipality6", "year"), all.x = TRUE)
  p[is.na(bartik), bartik := 0]
  p
}

################################################################################
# Sample variants
# A: paper's control group. B: drop control municipalities within 10 km of the
# Paraopeba (Brumadinho, Jan 2019). C: drop the whole Alto São Francisco
# sub-basin from control. D: drop high-mining municipalities in both groups
# (Vale's statewide dam shutdowns after Brumadinho hit mining towns in both).
################################################################################

VARIANTS <- list(
  A_paper          = function(d) d,
  B_no_paraopeba   = function(d) d[!(treat == 0L & paraopeba_10km == 1L)],
  C_no_alto_sf     = function(d) d[!(treat == 0L & alto_sao_francisco == 1L)],
  D_no_mining_muni = function(d) d[mining_muni == 0L]
)

################################################################################
# Output helpers and disclosure control
################################################################################

BANNED_COLS <- c("wid", "pis", "cpf", "cnpj", "firm_root", "id", "worker_id")

write_out <- function(dt, name) {
  dt <- as.data.table(dt)
  bad <- intersect(tolower(names(dt)), BANNED_COLS)
  if (length(bad)) stop("Refusing to write identifiers to outputs: ", paste(bad, collapse = ", "))
  path <- file.path(OUT_DIR, name)
  fwrite(dt, path)
  log_msg("  wrote ", path, " (", nrow(dt), " rows)")
  invisible(path)
}

# Blank out statistics computed from fewer than MIN_CELL workers.
suppress_small <- function(dt, n_col = "n", value_cols = setdiff(names(dt), n_col)) {
  dt <- copy(dt)
  small <- dt[[n_col]] < MIN_CELL
  for (v in value_cols) if (is.numeric(dt[[v]])) set(dt, which(small), v, NA)
  set(dt, which(small), n_col, NA)
  dt
}

# Coefficient table for terms matching `pattern`; d is the estimation data,
# used to count clusters (2014 municipalities) in the estimation sample.
tidy_coefs <- function(m, pattern, d, cluster = "municipality6") {
  ct <- as.data.table(fixest::coeftable(m), keep.rownames = "term")
  setnames(ct, c("term", "coef", "se", "tstat", "pval"))
  ct <- ct[grepl(pattern, term)]
  ct[, `:=`(n_obs = nobs(m), n_clusters = uniqueN(d[[cluster]][fixest::obs(m)]))]
  ct
}
