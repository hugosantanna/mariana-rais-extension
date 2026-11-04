#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# Project: Mariana — RAIS extension (post-2018)
# Script: tests/make_fake_rais.R
# Summary: Writes synthetic RAIS files in the Ministry's raw layout (';'-separated,
#          Latin-1 headers, comma decimals) plus one year in the paper's English
#          .fst layout, to smoke-test the pipeline without real data.
#          The true wage effect is -0.02 for Doce workers from 2016 on.
# Usage:   cd rais_extension && Rscript tests/make_fake_rais.R /tmp/fake_rais
#          then set RAIS_DIR to that folder in config.R.
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

suppressPackageStartupMessages({
  library(data.table)
  library(fst)
})
set.seed(20260928)

args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args)) args[1] else file.path(tempdir(), "fake_rais")
dir.create(out, showWarnings = FALSE, recursive = TRUE)

YEARS <- 2010:2021
N_WORKERS <- 30000
N_FIRMS <- 3000
TRUE_EFFECT <- -0.02

xw <- fread("inputs/municipality_crosswalk.csv", colClasses = list(character = "municipality6"))
sectors <- c("01", "07", "10", "25", "41", "47", "49", "56", "62", "86")

firms <- data.table(
  firm = seq_len(N_FIRMS),
  cnpj = sprintf("%08d0001%02d", sample(1e7:9e7, N_FIRMS), sample(10:99, N_FIRMS, TRUE)),
  muni = sample(xw$municipality6, N_FIRMS, TRUE, prob = ifelse(xw$doce == 1, 1.5, 1)),
  sector = sample(sectors, N_FIRMS, TRUE),
  nature = sample(c("2062", "2054", "1031"), N_FIRMS, TRUE, prob = c(0.8, 0.17, 0.03)),
  psi = rnorm(N_FIRMS, 0, 0.2)
)
firms <- merge(firms, xw[, .(muni = municipality6, doce)], by = "muni")
setorder(firms, firm)

workers <- data.table(
  w = seq_len(N_WORKERS),
  pis = sprintf("%011.0f", sample(1e10:9e10, N_WORKERS)),
  cpf = sprintf("%011.0f", sample(1e10:9e10, N_WORKERS)),
  alpha = rnorm(N_WORKERS, 1.5, 0.4),
  birth = sample(1955:1995, N_WORKERS, TRUE),
  sex = sample(1:2, N_WORKERS, TRUE),
  race = sample(c(2, 4, 8), N_WORKERS, TRUE),
  educ = sample(1:11, N_WORKERS, TRUE),
  firm = sample(firms$firm, N_WORKERS, TRUE),
  start = sample(2000:2013, N_WORKERS, TRUE)
)

rows <- list()
for (y in YEARS) {
  move <- runif(N_WORKERS) < 0.12
  workers[move, `:=`(firm = sample(firms$firm, sum(move), TRUE), start = y)]
  emp <- workers[runif(.N) < 0.92]
  f <- firms[emp$firm]
  treat <- f$doce == 1
  lw <- emp$alpha + f$psi + 0.02 * (y - 2010) + TRUE_EFFECT * treat * (y >= 2016) + rnorm(nrow(emp), 0, 0.1)
  hours <- sample(c(44, 40, 20), nrow(emp), TRUE, prob = c(0.8, 0.15, 0.05))
  wage <- exp(lw) * hours * 4.33 * 3
  sep <- sample(c(0, 11, 21, 31), nrow(emp), TRUE, prob = c(0.85, 0.08, 0.06, 0.01))
  sep_m <- ifelse(sep > 0, sample(1:12, nrow(emp), TRUE), 0)
  hire <- sprintf("%02d%02d%d", sample(1:28, nrow(emp), TRUE), sample(1:12, nrow(emp), TRUE), emp$start)
  rows[[as.character(y)]] <- data.table(
    `Município` = f$muni, `CNPJ / CEI` = f$cnpj, PIS = emp$pis, CPF = emp$cpf,
    `Vl Remun Média Nom` = round(wage, 2), `Vl Remun Dezembro Nom` = round(wage * (sep == 0), 2),
    `Qtd Hora Contr` = hours, `Sexo Trabalhador` = emp$sex, `Raça Cor` = emp$race,
    Idade = y - emp$birth, `Escolaridade após 2005` = emp$educ,
    `Tempo Emprego` = round((y - emp$start) * 12 + 6, 1), `Data Admissão Declarada` = hire,
    `Tipo Vínculo` = 10, `Mês Desligamento` = sep_m, `Motivo Desligamento` = sep,
    `CBO Ocupação 2002` = 717020, `CNAE 2.0 Classe` = paste0(f$sector, "123"),
    `Natureza Jurídica` = f$nature, `Vínculo Ativo 31/12` = as.integer(sep == 0),
    `Tamanho Estabelecimento` = 5L
  )
  # a few out-of-state jobs for national-scope tests
  sp <- rows[[as.character(y)]][sample(.N, 200)]
  sp[, `Município` := "355030"]
  rows[[as.character(y)]] <- rbind(rows[[as.character(y)]], sp)
}

write_raw_txt <- function(dt, path) {
  con <- file(path, open = "wb")
  writeLines(iconv(paste(names(dt), collapse = ";"), "UTF-8", "latin1"), con, useBytes = TRUE)
  close(con)
  fwrite(dt, path, sep = ";", dec = ",", append = TRUE, col.names = FALSE)
}

for (y in YEARS) {
  dt <- rows[[as.character(y)]]
  if (y == 2011) {
    # the paper's harmonized English layout, as .fst
    en <- dt[, .(MUNI = `Município`, PLANT_ID = `CNPJ / CEI`, PIS, CPF,
                 EARN_AVG_MONTH_NOM = `Vl Remun Média Nom`, NUM_HOURS_CONTRACTED = `Qtd Hora Contr`,
                 GENDER = fifelse(`Sexo Trabalhador` == 1, "M", "F"), RACE_v2006 = `Raça Cor`,
                 AGE = Idade, EDUC_v2005 = `Escolaridade após 2005`, TENURE_MONTHS = `Tempo Emprego`,
                 HIRE_DATE = as.Date(`Data Admissão Declarada`, "%d%m%Y"), CONTRACT_TYPE = 1L,
                 MONTH_OF_SEP = `Mês Desligamento`, CAUSE_OF_SEP = `Motivo Desligamento`,
                 OCCUP_CBO2002 = `CBO Ocupação 2002`, CNAE20_CLASS = `CNAE 2.0 Classe`,
                 LEGAL_NATURE_CONCLA2002 = `Natureza Jurídica`, EMP_ON_DEC31 = `Vínculo Ativo 31/12`)]
    write_fst(en, file.path(out, sprintf("rais%d.fst", y)))
  } else {
    write_raw_txt(dt, file.path(out, sprintf("MG%d.txt", y)))
  }
}
message("Fake RAIS written to ", out, " (", length(YEARS), " years). True wage effect: ", TRUE_EFFECT)
