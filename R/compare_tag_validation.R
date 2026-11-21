# ============================================================
# Compare the AI screeners with the team's own Zotero tags
# Run after the 3 agents finish: Rscript R/compare_tag_validation.R
#   human 'exclude' = binned (not relevant)      human 'keep' = read, not binned
# Output (data/screening/human_tag_validation/):
#   comparison.csv        confusion tables
#   contradictions.csv    where the AI consensus disagrees with the human tag
# ============================================================

library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)

base <- "data/screening/human_tag_validation"
ok   <- c("include", "exclude", "unsure")

# ---- read the 3 agents
read_agent <- function(k) {
  d <- read_csv(file.path(base, "agents", paste0("agent", k), "labels.csv"), show_col_types = FALSE) |>
    mutate(label = str_to_lower(str_squish(label)))
  stopifnot(all(d$label %in% ok), !anyDuplicated(d$sample_no))
  d |> mutate(agent = paste0("agent", k))
}
long <- map_dfr(1:3, read_agent)
stopifnot(all(count(long, agent)$n == 346))

lab <- long |> select(sample_no, agent, label) |> pivot_wider(names_from = agent, values_from = label)
why <- long |> select(sample_no, agent, reason) |> pivot_wider(names_from = agent, values_from = reason, names_prefix = "why_")

# ---- AI consensus = majority; 3-way split -> unsure
lab$ai <- apply(lab[, c("agent1", "agent2", "agent3")], 1, function(x) {
  tb <- sort(table(x), decreasing = TRUE)
  if (length(tb) == 1 || tb[1] > tb[2]) names(tb)[1] else "unsure"
})
lab$unanimous <- apply(lab[, c("agent1", "agent2", "agent3")], 1, function(x) length(unique(x)) == 1)

# ---- join the human decision (the agents never saw this) and the text
key <- read_csv(file.path(base, "key.csv"), show_col_types = FALSE)
txt <- read_csv(file.path(base, "records.csv"), show_col_types = FALSE)
d <- lab |> left_join(key, by = "sample_no") |> left_join(txt, by = "sample_no") |> left_join(why, by = "sample_no") |>
  mutate(has_abstract = abstract != "[no abstract]")

# ---- confusion table: human decision x AI consensus
cat("\nHuman decision x AI consensus (counts):\n")
conf <- d |> count(human_decision, ai) |> pivot_wider(names_from = ai, values_from = n, values_fill = 0)
print(conf)

# headline numbers
bins  <- filter(d, human_decision == "exclude")
keeps <- filter(d, human_decision == "keep")
cat("\nOf", nrow(bins), "records you binned (not relevant):\n")
cat("  AI also excludes:        ", sum(bins$ai == "exclude"), sprintf("(%.0f%%)", 100 * mean(bins$ai == "exclude")), "\n")
cat("  AI unsure:               ", sum(bins$ai == "unsure"),  sprintf("(%.0f%%)", 100 * mean(bins$ai == "unsure")), "\n")
cat("  AI includes (disagree):  ", sum(bins$ai == "include"), sprintf("(%.0f%%)", 100 * mean(bins$ai == "include")), "\n")
cat("Of", nrow(keeps), "records you read and kept:\n")
cat("  AI includes:             ", sum(keeps$ai == "include"), sprintf("(%.0f%%)", 100 * mean(keeps$ai == "include")), "\n")
cat("  AI unsure:               ", sum(keeps$ai == "unsure"),  sprintf("(%.0f%%)", 100 * mean(keeps$ai == "unsure")), "\n")
cat("  AI excludes (disagree):  ", sum(keeps$ai == "exclude"), sprintf("(%.0f%%)", 100 * mean(keeps$ai == "exclude")), "\n")

cat("\nBy abstract present (share of binned records the AI excludes / is unsure about / includes):\n")
print(bins |> group_by(has_abstract) |> summarise(n = n(), exclude = round(mean(ai == "exclude"), 2),
                                                  unsure = round(mean(ai == "unsure"), 2), include = round(mean(ai == "include"), 2)))
cat("\nAgent agreement on this set (unanimous):", sum(d$unanimous), "of", nrow(d), "\n")

# ---- save
write_csv(conf, file.path(base, "comparison.csv"))
contra <- d |> filter((human_decision == "exclude" & ai == "include") | (human_decision == "keep" & ai == "exclude")) |>
  select(sample_no, human_decision, ai, agent1, agent2, agent3, year, title, abstract, starts_with("why_"))
write_csv(contra, file.path(base, "contradictions.csv"))
cat("\nContradictions saved:", nrow(contra), "\n")
