# ============================================================
# Compare the three SIMULATED coders (AI agents)
# Run after the agents finish: Rscript R/check_simulated_agreement.R
# Output (data/screening/simulated_coders/):
#   agreement.csv       agreement + kappa per pair
#   labels_wide.csv     one row per record, 3 labels + consensus
#   disagreements.csv   records where coders differ, with their reasons
# This tests the criteria and the process. It is NOT human ground truth.
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)

base   <- "data/screening/simulated_coders"
coders <- c("Cory_sim", "Prajna_sim", "Dorcas_sim")
ok     <- c("include", "exclude", "unsure")

# ---- read each coder's labels, check them
read_labels <- function(who) {
  d <- read_csv(file.path(base, who, "labels.csv"), show_col_types = FALSE) |>
    mutate(label = str_to_lower(str_squish(label)))
  stopifnot(all(d$label %in% ok), !anyDuplicated(d$sample_no))
  message(who, ": ", nrow(d), " labels")
  d |> mutate(coder = who)
}
long <- map_dfr(coders, read_labels)

# all 300 records must be labelled by everyone
n_by <- count(long, coder)
if (any(n_by$n != 300)) warning("Not every coder has 300 labels: ", paste(n_by$coder, n_by$n, collapse = "; "))

lab <- long |> select(sample_no, coder, label) |> pivot_wider(names_from = coder, values_from = label)
why <- long |> select(sample_no, coder, reason) |> pivot_wider(names_from = coder, values_from = reason, names_prefix = "why_")

# ---- agreement + Cohen's kappa per pair
kappa <- function(a, b) {
  keep <- !is.na(a) & !is.na(b); a <- a[keep]; b <- b[keep]
  tab <- table(factor(a, ok), factor(b, ok))
  po <- sum(diag(tab)) / sum(tab)
  pe <- sum(rowSums(tab) * colSums(tab)) / sum(tab)^2
  tibble(n = sum(tab), agree = round(po, 3), kappa = round((po - pe) / (1 - pe), 3))
}
agree <- map_dfr(combn(coders, 2, simplify = FALSE),
                 ~ bind_cols(tibble(pair = paste(.x, collapse = " vs ")), kappa(lab[[.x[1]]], lab[[.x[2]]])))
write_csv(agree, file.path(base, "agreement.csv"))
print(agree)

# ---- consensus: majority; 3-way split = DISCUSS
lab$consensus <- apply(lab[, coders], 1, function(x) {
  tb <- sort(table(x), decreasing = TRUE)
  if (length(tb) == 1 || tb[1] > tb[2]) names(tb)[1] else "DISCUSS"
})
# how many coders said include (0-3): shows how "shared" each decision is
lab$n_include <- rowSums(lab[, coders] == "include")
lab$unanimous <- apply(lab[, coders], 1, function(x) length(unique(x)) == 1)

# ---- add record text, stratum and keyword triage (the coders never saw these)
key  <- read_csv("data/screening/validation_sample_key.csv", show_col_types = FALSE)
txt  <- read_csv("data/screening/validation_sample_coding_sheet.csv", show_col_types = FALSE) |> select(sample_no, year, title)
wide <- lab |> left_join(why, by = "sample_no") |> left_join(txt, by = "sample_no") |>
  left_join(select(key, sample_no, stratum, triage, has_abstract, tag_bin, tag_read), by = "sample_no")
write_csv(wide, file.path(base, "labels_wide.csv"))
write_csv(filter(wide, !unanimous), file.path(base, "disagreements.csv"))

cat("\nConsensus:\n"); print(count(wide, consensus))
cat("\nUnanimous:", sum(wide$unanimous), "of", nrow(wide), "\n")
cat("\nNumber of coders saying include (0-3):\n"); print(count(wide, n_include))

# ---- does keyword triage predict the consensus? (it should NOT be used to exclude)
cat("\nConsensus by keyword triage stratum (share of records):\n")
print(wide |> count(stratum, consensus) |> group_by(stratum) |> mutate(share = round(n / sum(n), 2)))
cat("\nConsensus by abstract present:\n")
print(wide |> count(has_abstract, consensus) |> group_by(has_abstract) |> mutate(share = round(n / sum(n), 2)))
