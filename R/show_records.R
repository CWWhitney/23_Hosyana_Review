# Print records from a coder folder, one per block, for reading.
# Usage: Rscript R/show_records.R <folder> <from_row> <to_row>
# Example: Rscript R/show_records.R data/screening/simulated_coders/Cory_sim 1 40
args <- commandArgs(trailingOnly = TRUE)
d <- read.csv(file.path(args[1], "records.csv"), stringsAsFactors = FALSE)
rows <- seq(as.integer(args[2]), min(as.integer(args[3]), nrow(d)))
for (i in rows) {
  cat("[", d$sample_no[i], "] (", d$year[i], ") ", d$title[i], "\n",
      gsub("\\s+", " ", d$abstract[i]), "\n\n", sep = "")
}
