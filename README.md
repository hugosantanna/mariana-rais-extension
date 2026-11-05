# Mariana paper: RAIS after 2018

This folder extends the Mariana paper with the RAIS years after 2018, which the
Labour Economics referees asked for. You run it inside the Insper VM, where the
identified RAIS lives. Only aggregated results (coefficients, rates, counts)
come out at the end.

There are eight steps. Most of the time is the computer working, not you.

**[⬇ Download the code (zip)](https://github.com/hugosantanna/mariana-rais-extension/archive/refs/heads/main.zip)**
&nbsp;·&nbsp; [Browse the files](https://github.com/hugosantanna/mariana-rais-extension)

---

## Before you start

- **Insper VM access.** Log in through Citrix at
  [desk.datascience.insper.edu.br](https://desk.datascience.insper.edu.br) with
  your `@datascience.insper.edu.br` account. No VPN needed.
- **R 4.2 or newer and RStudio** inside the VM. Step 4 checks this for you.
- **Memory.** The full run handles about 60 million worker-years. A machine
  with 64 GB of RAM or more is best. If it runs out, there's a fix in
  [If something goes wrong](#if-something-goes-wrong).

---

## Step 1. Get the code into the VM

Open the download link above in the VM's web browser. No GitHub account is
needed. If the VM can't reach GitHub, download the zip on your own computer
and send it in through the
[CDIA file request form](https://forms.office.com/r/GGg73H0nan). CDIA will put
it in your **Entrada** folder.

Unzip it (you get a folder called `mariana-rais-extension-main`) into your
**Storage folder** (the one with your user name) or the
project folder on **T:**. Don't put it on **C:**, because C: is wiped after some
days without a login and the intermediate files would be lost.

## Step 2. Open the project

Double-click **`rais_extension.Rproj`**. RStudio opens already pointed at the
right folder. Open it this way every time you come back, since the scripts only
work when RStudio starts in this folder.

## Step 3. Tell the code where the RAIS files are

Open **[`config.R`](config.R)**. It's the only file you need to edit. Change
two lines.

```r
RAIS_DIR <- "T:/path/to/the/RAIS/folder"   # use forward slashes /
YEARS    <- 2010:2023                       # every year you have, from 2010 on
```

Windows shows paths with backslashes (`T:\Projetos\RAIS`). Either flip them to
forward slashes or double them (`T:\\Projetos\\RAIS`). Single backslashes will
not work in R.

If the folder has one file per state (`ac2010id.csv`, ..., `mg2010id.csv`), the
code reads only the Minas Gerais ones. Step 4 lists the files it picked for each
year.

Leave everything else as it is for now.

## Step 4. Check the setup (seconds)

In the RStudio **Console**, type

```r
source("00_check_setup.R")
```

It checks R, the packages, the RAIS folder, and that every year has files.
Every line should say **OK**. Anything marked **FIX** comes with a note saying
what to do. If packages are missing, it prints the exact `install.packages(...)`
line to paste. Run the check again until everything is OK.

## Step 5. Audit the data (minutes)

```r
source("01_audit.R")
```

This reads the column names and a sample of rows from each year, so we can be
sure the files look the way the code expects. When it finishes, open
**`outputs/run_log.txt`** and look for three things.

1. **No line says `NO FILES FOUND`, `MISSING REQUIRED FIELDS` or `ACTION NEEDED`.**
   If one does, the fix is in [If something goes wrong](#if-something-goes-wrong).
2. **The year lines look sensible.** For example
   `MG2019.txt: sampled 200,000 rows | MG 1.00 | PIS ok 0.99 | CPF ok 0.99 | wage NA 0.000`.
   `PIS ok` (or `CPF ok`) should be close to 1, and `wage NA` close to 0.
3. **The linkage lines at the bottom.** For example
   `2019: 2014 PIS found 0.620 | 2014 CPF found 0.640`.
   This is the share of 2014 workers we can find again in later years. If CPF
   is clearly higher than PIS after 2018 (say, five points or more), open
   `config.R` and set `WORKER_ID <- "cpf"`.

If all three look fine, go on to step 6. If something looks off, stop here and
send Hugo the `outputs/` folder (see [step 8](#step-8-send-the-results-out)).
It holds no worker-level data.

## Step 6. Test run on 5% of workers

In `config.R`, set

```r
SAMPLE_SHARE <- 0.05
```

then run

```r
source("run_all.R")
```

The point is only to see that it reaches **`Done.`** without errors. The numbers
won't match the paper on a 5% sample, and that's expected.

## Step 7. Full run (hours)

1. In `config.R`, set `SAMPLE_SHARE <- 1`.
2. Delete the **`work`** and **`outputs`** folders that the test run created.
3. In RStudio, go to **Session → Restart R** to free memory.
4. Run `source("run_all.R")` again.

This takes several hours. Leave it running, and don't sign out of the VM until
it says `Done.`

When it finishes, open **`outputs/03_replication.csv`**. It re-estimates the
paper's Table 2 on 2010–2018 as a check that the data were built the same way.
The `match` column says `TRUE` when a coefficient lands within 0.003 of the
paper. If some say `FALSE`, send the results anyway. The usual cause is how the
full-time or contract-type fields are coded, and Hugo can adjust that.

## Step 8. Send the results out

1. Right-click the **`outputs`** folder → **Send to → Compressed (zipped)
   folder**.
2. Copy `outputs.zip` into your **Saída** folder.
3. Fill in the [CDIA file request form](https://forms.office.com/r/GGg73H0nan)
   asking them to release it to you. They review the contents first.
4. Forward the zip to Hugo.

> **Never put the `work` folder in Saída or anywhere outside the VM.** It holds
> identified worker-level data. The `outputs` folder is safe. The code refuses
> to write any file with an ID column, and it blanks any cell built from fewer
> than 10 workers.

---

## If something goes wrong

| You see | What to do |
|---|---|
| `config.R not found` or `Run these scripts from inside the rais_extension/ folder` | Close RStudio and reopen it by double-clicking `rais_extension.Rproj` (step 2). |
| `RAIS_DIR does not exist` | Check the path in `config.R` for typos, and use `/` instead of `\`. |
| `there is no package called '...'` | Run `install.packages("...")` with that package name. If the VM can't install packages, ask CDIA. |
| `MISSING REQUIRED FIELDS: sep_cause` (or another field) | The file names that column differently. The next log line lists the file's columns that matched nothing, and one of them is the right one. Add it to `COLUMN_MAP` in `config.R`, for example `COLUMN_MAP <- list(sep_cause = "causa_deslig")`, and run the audit again. The name only applies to files that have it, so a 2010-only name doesn't break other years. If you're not sure which column it is, send Hugo a photo of those log lines. |
| `cannot allocate vector of size ...` or R crashes | The machine ran out of memory. Set `SAMPLE_SHARE <- 0.5` in `config.R`, delete `work` and `outputs`, restart R, and run again. Tell Hugo you did this. |
| The Citrix session dropped mid-run | Delete `work` and `outputs`, reopen the project, and start step 7 again. |
| Anything else | Send Hugo the error message (text only, no data rows) and `outputs/run_log.txt`. |

---

## Reference

### What each output answers

| Output file | Referee request |
|---|---|
| `03_replication.csv` | Check that this build reproduces Table 2 on 2010–2018 |
| `04_event_study.csv`, `04_period_averages.csv` | R1-2. Longer run for wages, separations and formal employment |
| `05_cumulative_earnings.csv` | R1-1(ii, iii) and R1-2. Cumulative earnings as % of 2014 pay, split by source, and years employed |
| `05_heterogeneity.csv` | R1-1(iv). By 2014 wage, age, tenure, establishment size and mining |
| `05_destinations.csv` | R1-1(i). Where workers are in 2017 and in the last year |
| `05_exit_rates.csv` | R1-4. Exit from formal employment by year and treatment |
| `log_hourly_dec` in `04_*` | R2-5. December pay as an alternative wage measure |

### Files in this folder

| File | What it does |
|---|---|
| [`config.R`](config.R) | Settings. The only file to edit |
| [`00_check_setup.R`](00_check_setup.R) | Checks R, packages, the RAIS folder and files per year |
| [`01_audit.R`](01_audit.R) | Column mapping, parsing checks, PIS vs CPF linkage |
| [`run_all.R`](run_all.R) | Runs 02 to 05 in order |
| [`02_build_panel.R`](02_build_panel.R) | Worker-year panel, 2014 baseline, earnings by source, Bartik counts |
| [`03_replicate_baseline.R`](03_replicate_baseline.R) | The paper's Table 2 on 2010–2018 |
| [`04_longrun_event_study.R`](04_longrun_event_study.R) | Event studies through the last year, four sample variants |
| [`05_worker_trajectories.R`](05_worker_trajectories.R) | Cumulative earnings, heterogeneity, destinations, exit |
| [`R/utils.R`](R/utils.R) | Shared helpers |
| [`inputs/municipality_crosswalk.csv`](inputs/municipality_crosswalk.csv) | 853 MG municipalities with Doce flag, sub-basin, Paraopeba distance, dam and river distance |
| [`inputs/ipca_deflator_2025usd.csv`](inputs/ipca_deflator_2025usd.csv) | IPCA deflator to 2025 USD, 2010–2024 (same as the paper) |
| [`tests/make_fake_rais.R`](tests/make_fake_rais.R) | Synthetic RAIS for testing without real data |
| `rais_extension.Rproj` | Opens RStudio in this folder |

<details>
<summary><strong>Design choices</strong> (how the code handles definitions and post-2018 confounders)</summary>

**Definitions follow the paper exactly.** Private-sector full-time formal jobs,
one primary job per worker-year, 2014 cohort, treatment fixed at the 2014
municipality (Doce basin), post = 2016 onward, standard errors clustered by
municipality.

**Brumadinho (January 2019)** contaminated the Paraopeba, which sits in the
control group. Variant `B_no_paraopeba` drops control municipalities within
10 km of the river, and `C_no_alto_sf` drops the whole Alto São Francisco
sub-basin.

**Vale shut down upstream dams statewide after Brumadinho**, which hit mining
towns in both groups. Variant `D_no_mining_muni` drops municipalities with 5%
or more of 2014 employment in ore mining.

**COVID-19** overlaps 2020 onward, so the period averages keep 2016–18, 2019,
2020–21 and 2022+ separate.

**Earnings by source** are defined relative to the worker's 2014 job, in this
order. Same CNPJ root as in 2014 (initial employer), then outside the origin
region (the other basin group, or another state in national scope), then same
2-digit sector, then other sector.

**Separation codes.** RAIS 2018 codes "no separation" as `0`, while 2010–2017
use `00`. The code treats any all-zero code as no separation, so the mix across
years doesn't matter.

</details>

<details>
<summary><strong>Testing without real data</strong></summary>

`tests/make_fake_rais.R` writes synthetic RAIS with a planted wage effect of
−0.02. Run `source("tests/make_fake_rais.R")`, point `RAIS_DIR` at the folder
it prints, set `YEARS <- 2010:2021` and `SAMPLE_SHARE <- 1`, and run
`01_audit.R` and then `run_all.R`. The wage effect in `03_replication.csv`
should come out near −0.013 (the planted −0.02, diluted by workers who switch
firms). `match` will say `FALSE`, since fake data can't match the paper. Put `config.R` back
afterwards.

</details>

<details>
<summary><strong>Running somewhere other than the Insper VM</strong></summary>

The steps are the same. Skip the Citrix, Entrada and Saída parts, and zip
`outputs/` directly. `FILE_SCOPE <- "national"` in `config.R` also follows 2014
Minas Gerais workers into other states, if the files cover all states.

</details>
