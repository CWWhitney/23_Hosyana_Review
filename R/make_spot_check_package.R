# ============================================================
# Spot-check package for human reviewers (Prajna, Dorcas, Cory)
# Run: Rscript R/make_spot_check_package.R
#
# Records = the ones where the AI screening and Cory's Zotero tags disagree
# (human_tag_validation/contradictions.csv). Blind: no AI label, no tags shown.
#
# Output
#   data/screening/spot_check/          <- SHARE THIS FOLDER
#     README.txt
#     spot_check_<name>.xlsx            one per reviewer, one sheet, same records, own order
#     spot_check_records.bib            same records, to import as a Zotero collection
#   data/screening/spot_check_private/  <- KEEP (answer key, do not share)
#     key.csv
# ============================================================

library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(writexl)
library(httr2)
library(stringdist)

base   <- "data/screening"
out    <- file.path(base, "spot_check")
priv   <- file.path(base, "spot_check_private")
dir.create(out,  recursive = TRUE, showWarnings = FALSE)
dir.create(priv, recursive = TRUE, showWarnings = FALSE)

reviewers <- c("Cory", "Prajna", "Dorcas")

# ------------------------------------------------------------
# 1. THE RECORDS (54): disagreements between AI and the Zotero tags
# ------------------------------------------------------------
contra <- read_csv(file.path(base, "human_tag_validation/contradictions.csv"), show_col_types = FALSE) |>
  select(sample_no, human_tag = human_decision, ai)
key    <- read_csv(file.path(base, "human_tag_validation/key.csv"), show_col_types = FALSE) |> select(sample_no, id)
recs   <- read_csv(file.path(base, "human_tag_validation/records.csv"), show_col_types = FALSE)   # title + best abstract
full   <- readRDS("data/prisma/records_full.rds") |>
  select(id, key, type, author, journal, publisher, doi = doi_clean)

set <- contra |> left_join(key, by = "sample_no") |> left_join(recs, by = "sample_no") |>
  select(-title, -year, -abstract) |>
  left_join(full, by = "id") |>
  left_join(select(recs, sample_no, year, title, abstract), by = "sample_no") |>
  mutate(spot_no = row_number())     # neutral number for everyone

# ------------------------------------------------------------
# 2. CROSSREF CHECK of every DOI (so the .bib header is true)
# verified = DOI exists in Crossref AND its title matches ours
# ------------------------------------------------------------
norm <- function(x) x |> str_replace_all("&amp;", "&") |> str_remove_all("<[^>]+>") |> tolower() |>
  str_replace_all("[^a-z0-9]+", " ") |> str_squish()
set$cr_status <- ifelse(is.na(set$doi), "no DOI in our data", "not found in Crossref")
dois <- unique(na.omit(set$doi))
if (length(dois) > 0) {
  resp <- request("https://api.crossref.org/works") |>
    req_url_query(filter = paste0("doi:", dois, collapse = ","), rows = length(dois), select = "DOI,title") |>
    req_user_agent("hosyana-review-spot-check (R httr2)") |> req_retry(max_tries = 3) |>
    req_error(is_error = \(r) FALSE) |> req_perform()
  if (resp_status(resp) == 200) {
    for (it in resp_body_json(resp)$message$items) {
      i <- which(set$doi == tolower(it$DOI))
      sim <- stringsim(norm(it$title[[1]] %||% ""), norm(set$title[i[1]]), "jw")
      set$cr_status[i] <- ifelse(sim >= 0.9 || startsWith(norm(set$title[i[1]]), norm(it$title[[1]])),
                                 "checked in Crossref: DOI and title match", "Crossref: DOI exists but title differs")
    }
  } else set$cr_status[!is.na(set$doi)] <- "Crossref check failed (not verified)"
}
cat("Crossref status:\n"); print(count(set, cr_status))

# ------------------------------------------------------------
# 3. ANSWER KEY (private)
# ------------------------------------------------------------
write_csv(select(set, spot_no, sample_no, id, key, human_tag, ai), file.path(priv, "key.csv"))

# ------------------------------------------------------------
# 4. ONE EXCEL PER REVIEWER: one sheet, no abstract (they read it in Zotero)
# columns: authors, year, title, then the 4 criteria (y / n / ?) and the result
# ------------------------------------------------------------
# short author text: first author's surname, "et al." if there are more
short_auth <- function(a) {
  au <- str_split(a, " and ")[[1]]
  s1 <- str_remove(au[1], ",.*$")
  if (length(au) > 1) paste(s1, "et al.") else s1
}
set$authors_short <- map_chr(set$author, ~ if (is.na(.x)) "" else short_auth(.x))

for (k in seq_along(reviewers)) {
  set.seed(900 + k)                       # own order per reviewer
  sheet <- set |> slice_sample(prop = 1) |>
    transmute(no = spot_no, authors = authors_short, year, title,
              `C1 research` = "", `C2 decision` = "", `C3 uncertainty` = "", `C4 method` = "",
              result = "")              # include / exclude / unsure
  write_xlsx(list(check = sheet), file.path(out, paste0("spot_check_", reviewers[k], ".xlsx")))
}

# ------------------------------------------------------------
# 5. BIBTEX FOR A ZOTERO COLLECTION
# Same records, no tags/keywords (nothing that shows our decisions).
# ------------------------------------------------------------
esc <- function(x) x |> str_replace_all("\\s+", " ") |> str_squish()     # one line per field
bib_entry <- function(r) {
  f <- function(name, val) if (is.na(val) || val == "") NULL else sprintf("  %s = {%s}", name, esc(val))
  ab <- if (r$abstract %in% c("[no abstract]", "") || is.na(r$abstract)) NA_character_ else r$abstract
  # strict BibTeX parsers need certain fields per type; use @misc when they are missing
  type <- case_when(
    r$type == "article" && !is.na(r$journal) && r$journal != ""     ~ "article",
    r$type == "book"    && !is.na(r$publisher) && r$publisher != "" ~ "book",
    TRUE                                                            ~ "misc")
  fields <- compact(list(
    f("title", r$title), f("author", r$author), f("year", as.character(r$year)),
    if (type == "article") f("journal", r$journal) else f("publisher", r$publisher),
    f("doi", r$doi), f("abstract", ab),
    f("note", paste0("Spot check ", r$spot_no, ". ", r$cr_status, "."))))
  paste0("@", type, "{", r$key, ",\n", paste(fields, collapse = ",\n"), "\n}\n")
}
header <- c(
  "% Spot-check records, copied from the project's own Zotero export (hosyana_review).",
  paste0("% DOIs were checked against Crossref (api.crossref.org) on ", Sys.Date(), "; the result is in each entry's note field."),
  "% No reference was added or changed. Abstracts are the best available (OpenAlex / Crossref / Zotero full text).",
  "% Import into a NEW Zotero collection to read the abstracts. Answers go in the Excel file.",
  "")
writeLines(c(header, map_chr(split(set, set$spot_no), bib_entry)), file.path(out, "spot_check_records.bib"))

# ------------------------------------------------------------
# 6. README.txt (short, plain)
# ------------------------------------------------------------
writeLines(c(
  "SPOT CHECK: does this record belong in our review?",
  "==================================================",
  "",
  "OUR REVIEW",
  "Methods that help people decide when outcomes are uncertain",
  "(decision analysis, value of information, probabilistic / Bayesian models, expert judgement ...).",
  "",
  paste0("YOUR TASK  (", nrow(set), " records, about 25 minutes, please work alone)"),
  "1. Zotero: import spot_check_records.bib into a NEW collection. Read title + abstract only.",
  "2. Excel: open spot_check_<your name>.xlsx. One row per record. Fill the 4 criteria columns, then 'result'.",
  "3. Send the Excel file back to Cory.",
  "",
  "THE 4 CRITERIA  (answer  y = yes   n = no   ? = cannot tell)",
  "C1 research     Is it a research output (article, chapter, book, report, thesis)?",
  "                n = news, syllabus, course notes, catalogue, journal issue.",
  "C2 decision     Is it about choosing between options (policy, intervention, investment, management)?",
  "C3 uncertainty  Does it deal with uncertainty explicitly",
  "                (probabilities, Bayesian, simulation, value of information, risk analysis,",
  "                sensitivity analysis, expert judgement)?",
  "C4 method       Does it describe, apply or test a method that supports the decision?",
  "                (just mentioning one is not enough)",
  "",
  "RESULT  (type exactly one word)",
  "include   C1, C2, C3 and C4 are all y",
  "exclude   at least one is n",
  "unsure    you cannot tell (no abstract, too vague)",
  "",
  "RULES OF THUMB",
  "- Decisions or uncertainty only mentioned in passing = n.",
  "- Descriptive or behavioural study with no decision method = exclude.",
  "- Pure statistics, machine learning or engineering with no decision = exclude.",
  "- Decision analysis with NO uncertainty (fully deterministic) = exclude.",
  "- A case study is NOT required. Language does not matter for now.",
  "- No abstract: judge from the title; if the title is too vague, write unsure.",
  "- Text starting [front matter only] is just the first lines of the document: weak evidence.",
  "- Not sure about a kind of paper (for example a review)? Write ? and tell Cory."),
  file.path(out, "README.txt"))
unlink(file.path(out, c("README.md", "criteria.md")))     # one place for the criteria: README.txt

cat("\nWrote ", nrow(set), " records for ", length(reviewers), " reviewers\n", sep = "")
print(list.files(out)); print(list.files(priv))
