# ============================================================
# Full-text features, WITHOUT storing the full text
# Zotero already extracted the text of each PDF on this machine
# (~/Zotero/storage/<KEY>/.zotero-ft-cache). We read each file once,
# keep only small results, and drop the text.
#
# Run after enrich_records.R:  Rscript R/fulltext_features.R
# Output: data/fulltext/fulltext_features.csv   (a few MB; one row per usable record)
#   n_chars            length of the text
#   uncertain_per10k   how often uncertainty words appear (per 10,000 chars)
#   decision_per10k    how often decision words appear
#   m_*                counts of method terms (see 'groups' below)
#   methods_excerpt    first ~1500 chars after a "Methods" heading (for screening / classification)
# ============================================================

library(dplyr)
library(stringr)
library(purrr)
library(readr)

source("R/fulltext_abstract.R")     # finds + reads Zotero text, checks title match

dir.create("data/fulltext", showWarnings = FALSE)

# ------------------------------------------------------------
# 1. WHICH RECORDS: included, with a usable text file
# ------------------------------------------------------------
rec <- readRDS("data/prisma/records_full.rds") |>
  filter(status == "included") |>
  select(id, key, title, file)

# ------------------------------------------------------------
# 2. WHAT TO COUNT: method term groups (lowercase regex)
# ------------------------------------------------------------
groups <- c(
  m_monte_carlo   = "monte carlo",
  m_bayesian      = "bayesian|bayes' |bayes theorem|posterior|prior distribution",
  m_voi           = "value of (perfect |sample |partial perfect )?information|\\bevpi\\b|\\bevppi\\b|\\bevsi\\b",
  m_sensitivity   = "sensitivity analys|tornado|one-way sensitivity|probabilistic sensitivity",
  m_decision_tree = "decision tree|influence diagram|markov model|markov decision",
  m_mcda          = "multi-?criteria|\\bmcda\\b|\\bmcdm\\b|analytic hierarchy|\\bahp\\b|topsis|electre|promethee",
  m_elicitation   = "elicit|delphi|expert judg|expert opinion|calibrat",
  m_scenario      = "scenario (analysis|planning|discovery)|robust decision|info-?gap|deep uncertainty",
  m_real_options  = "real options?",
  m_game_theory   = "game theor|nash equilibri",
  m_optimization  = "linear programming|stochastic programming|dynamic programming|optimi[sz]ation",
  m_fuzzy         = "fuzzy",
  m_simulation    = "agent-based|system dynamics|discrete[- ]event|microsimulation|simulation model",
  m_cost_effect   = "cost-effectiveness|cost-utility|cost-benefit|net benefit|\\bicer\\b|\\bqaly"
)
uncertain_pat <- "uncertain|probabilis|stochastic|variabilit|risk"
decision_pat  <- "decision|policy|policies|intervention|strateg|choice|option"

# ------------------------------------------------------------
# 3. ONE PASS: read text, count, keep a methods excerpt, forget the text
# ------------------------------------------------------------
features <- function(file_field, title) {
  f <- ft_file_for_record(file_field)
  if (is.na(f)) return(NULL)
  txt <- read_ft_text(f)
  if (nchar(txt) < 1000 || !title_in_text(title, txt)) return(NULL)   # empty or wrong attachment

  x  <- txt |> str_replace_all("-\\s*\\n\\s*", "") |> str_replace_all("\\s+", " ")
  lo <- tolower(x)
  n  <- nchar(x)

  # counts per method group + density of uncertainty and decision words
  counts <- map_int(groups, ~ str_count(lo, .x))
  out <- c(n_chars = n,
           uncertain_per10k = round(str_count(lo, uncertain_pat) / n * 1e4, 1),
           decision_per10k  = round(str_count(lo, decision_pat) / n * 1e4, 1),
           counts)

  # excerpt: text right after a "Methods" style heading (after the first 5% to skip the contents list)
  start <- floor(n * 0.05)
  m <- str_locate(substr(x, start, n), regex("\\b(materials and methods|methodology|methods|model description|the model)\\b\\s", ignore_case = TRUE))
  excerpt <- if (is.na(m[1, 1])) NA_character_ else substr(x, start + m[1, 2], start + m[1, 2] + 1500)

  c(as.list(out), methods_excerpt = excerpt)
}

message("Reading text for ", nrow(rec), " records ...")
res <- map2(rec$file, rec$title, features)
keep <- !map_lgl(res, is.null)

feat <- bind_rows(res[keep]) |>
  mutate(across(-methods_excerpt, as.numeric))
feat <- bind_cols(rec[keep, c("id", "key")], feat)

write_csv(feat, "data/fulltext/fulltext_features.csv")

# ------------------------------------------------------------
# 4. SUMMARY
# ------------------------------------------------------------
cat("\nRecords with usable full text:", nrow(feat), "of", nrow(rec), "\n")
cat("File size (MB):", round(file.size("data/fulltext/fulltext_features.csv") / 1e6, 1), "\n")
cat("With a methods excerpt:", sum(!is.na(feat$methods_excerpt)), "\n\n")
cat("Records mentioning each method (>= 3 times):\n")
print(sort(colSums(feat[, names(groups)] >= 3), decreasing = TRUE))
cat("\nUncertainty words per 10k chars (usable full texts):\n")
print(summary(feat$uncertain_per10k))
