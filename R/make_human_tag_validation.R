# ============================================================
# Validation set from the team's OWN Zotero tags (real human decisions)
# Run: Rscript R/make_human_tag_validation.R
#   'bin'                 -> human said EXCLUDE   (read and thrown out)
#   'read CW/PKA' only    -> human said KEEP      (read, not binned)
# The AI screening is then compared against these, with no new human time.
# Output: data/screening/human_tag_validation/
#   records.csv  (blind: no tag shown)   key.csv  (tag + decision)
# Caveats: 'bin' may also mean "no access" or "duplicate"; these records were not
# drawn at random (they are what the team chose to read).
# ============================================================

library(dplyr)
library(stringr)
library(stringi)
library(readr)

dir.create("data/screening/human_tag_validation", recursive = TRUE, showWarnings = FALSE)

full <- readRDS("data/prisma/records_full.rds")
enr  <- read_csv("data/prisma/records_enriched.csv", show_col_types = FALSE)

# clean text (strip control chars / non-characters)
clean <- function(x) x |> stri_replace_all_regex("[\\p{C}]", " ") |> str_squish()

# drop records that left only because they were exact copies of another record
dup_reasons <- c("duplicate_citekey", "duplicate_doi", "duplicate_title_year", "duplicate_reviewed")

tagged <- full |>
  filter(tag_bin | tag_read, !reason %in% dup_reasons) |>
  left_join(select(enr, id, abstract_enriched = abstract_final, abstract_source), by = "id") |>
  mutate(
    human_decision = ifelse(tag_bin, "exclude", "keep"),   # bin wins (read then binned)
    # use the enriched abstract when we have one, else the one from Zotero
    abstract = clean(coalesce(abstract_enriched, abstract)),
    # front matter only (no real abstract found): mark it, so the screener knows it is weaker evidence
    abstract = ifelse(!is.na(abstract_source) & abstract_source == "zotero_fulltext_lead" & !is.na(abstract),
                      paste("[front matter only]", abstract), abstract),
    abstract = ifelse(abstract == "" | is.na(abstract), "[no abstract]", abstract),
    title = clean(title)
  )

# blind sheet in random order, same layout the agents already know
set.seed(77)
sheet <- tagged |> slice_sample(prop = 1) |>
  transmute(sample_no = row_number(), year, title, abstract, id, human_decision)

write_csv(select(sheet, sample_no, year, title, abstract), "data/screening/human_tag_validation/records.csv")
write_csv(select(sheet, sample_no, id, human_decision), "data/screening/human_tag_validation/key.csv")
file.copy("data/screening/criteria.md", "data/screening/human_tag_validation/criteria.md", overwrite = TRUE)
write_csv(tibble(sample_no = integer(), label = character(), reason = character()),
          "data/screening/human_tag_validation/labels.csv")

cat("Records:", nrow(sheet), "\n")
print(count(sheet, human_decision))
cat("\nWith a real abstract:\n")
print(sheet |> count(human_decision, has_abstract = abstract != "[no abstract]"))
