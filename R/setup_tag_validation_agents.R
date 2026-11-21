# Three blind AI screeners for the human-tag validation set (346 records).
# Each gets the same records in its own order, the criteria, and an empty labels file.
# The key (human decision) stays one folder up and is NOT given to the agents.
# Run: Rscript R/setup_tag_validation_agents.R
library(dplyr); library(readr)
base <- "data/screening/human_tag_validation"
recs <- read_csv(file.path(base, "records.csv"), show_col_types = FALSE)
for (k in 1:3) {
  d <- file.path(base, "agents", paste0("agent", k))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  set.seed(500 + k)                                   # own order per agent
  write_csv(slice_sample(recs, prop = 1), file.path(d, "records.csv"))
  file.copy(file.path(base, "criteria.md"), file.path(d, "criteria.md"), overwrite = TRUE)
  write_csv(tibble(sample_no = integer(), label = character(), reason = character()), file.path(d, "labels.csv"))
}
writeLines("AI screeners on records the team tagged by hand. Compare with key.csv only AFTER all agents finish.",
           file.path(base, "agents", "README.txt"))
