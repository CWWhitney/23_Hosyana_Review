# ============================================================
# Set up three SIMULATED coders (AI agents) for the 300-record sample
# Run: Rscript R/setup_simulated_coders.R
# Each folder gets: the criteria, the 300 records (own random order), an empty labels file.
# Agents see NO triage, NO manual tags, NO other coder's folder.
# Output: data/screening/simulated_coders/<name>/
# ============================================================

library(dplyr)
library(readr)

sheet <- read_csv("data/screening/validation_sample_coding_sheet.csv", show_col_types = FALSE) |>
  select(sample_no, year, title, abstract)       # no key, no triage, no tags

# the three simulated coders (names only mark whose slot they fill)
coders <- c("Cory_sim", "Prajna_sim", "Dorcas_sim")

for (k in seq_along(coders)) {
  dir <- file.path("data/screening/simulated_coders", coders[k])
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  # own random order per coder, so order effects do not line up
  set.seed(100 + k)
  records <- slice_sample(sheet, prop = 1)
  write_csv(records, file.path(dir, "records.csv"))

  # same criteria for everyone
  file.copy("data/screening/criteria.md", file.path(dir, "criteria.md"), overwrite = TRUE)

  # header only; the agent fills it
  write_csv(tibble(sample_no = integer(), label = character(), reason = character()),
            file.path(dir, "labels.csv"))
}

# clear note: this is simulated
writeLines(c("SIMULATED CODERS (AI agents), not human coding.",
             "Agreement between them tests the criteria and the process, not the truth of the labels."),
           "data/screening/simulated_coders/README.txt")
message("Set up ", length(coders), " folders in data/screening/simulated_coders/")
