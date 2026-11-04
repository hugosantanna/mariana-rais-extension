#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###
# Project: Mariana — RAIS extension (post-2018)
# Script: run_all.R
# Summary: Runs steps 02-05 in order. Run 01_audit.R on its own first and
#          check its output (see README).
# Usage:   cd rais_extension && Rscript run_all.R
#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+#+###

steps <- c("02_build_panel.R", "03_replicate_baseline.R",
           "04_longrun_event_study.R", "05_worker_trajectories.R")
for (s in steps) {
  message("\n==================== ", s, " ====================")
  source(s, local = new.env())
}
message("\nDone. Send the outputs/ folder back (never the work/ folder).")
