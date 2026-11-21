# ============================================================
# Fill missing abstracts for the human-tag validation set (Crossref only)
# Run after make_human_tag_validation.R:  Rscript R/enrich_tag_validation.R
# 1. records with a DOI: Crossref by DOI, 40 per request
# 2. records without: Crossref title search, accepted ONLY if title, year (<=1)
#    and first author all match (same strict rule as enrich_records.R)
# Writes the abstract back into records.csv (and keeps a log of what was added).
# ============================================================

library(dplyr)
library(stringr)
library(stringi)
library(readr)
library(purrr)
library(httr2)
library(stringdist)

dir <- "data/screening/human_tag_validation"
recs <- read_csv(file.path(dir, "records.csv"), show_col_types = FALSE)
key  <- read_csv(file.path(dir, "key.csv"), show_col_types = FALSE)
full <- readRDS("data/prisma/records_full.rds")

# add DOI, author, year from the full table (via key id)
recs <- recs |> left_join(key |> select(sample_no, id), by = "sample_no") |>
  left_join(full |> select(id, doi_clean, author, file), by = "id")

# --- helpers (same idea as enrich_records.R)
norm <- function(x) x |> str_replace_all("&amp;", "&") |> str_replace_all("&lt;", "<") |> str_replace_all("&gt;", ">") |>
  str_remove_all("<[^>]+>") |> str_remove_all("&[a-z]+;") |> tolower() |>
  str_replace_all("[^a-z0-9]+", " ") |> str_squish()
clean_abs <- function(ab) if (is.null(ab)) NA_character_ else
  ab |> str_remove_all("<[^>]+>") |> str_remove("^\\s*Abstract\\s*") |> stri_replace_all_regex("[\\p{C}]", " ") |> str_squish()
cr_get <- function(query) {
  tryCatch({
    resp <- request("https://api.crossref.org/works") |> req_url_query(!!!query) |>
      req_user_agent("hosyana-review-enrichment (R httr2)") |> req_throttle(rate = 4 / 1) |>
      req_retry(max_tries = 3, backoff = ~ 2) |> req_error(is_error = \(r) FALSE) |> req_perform()
    if (resp_status(resp) == 200) resp_body_json(resp, simplifyVector = FALSE)$message$items else NULL
  }, error = function(e) NULL)
}
surname <- function(a) a |> str_split(" and ") |> map_chr(1) |> str_remove(",.*$") |> norm() |> str_extract("[a-z]+$")

need <- recs$abstract == "[no abstract]"
recs$new_abstract <- NA_character_
recs$source <- NA_character_

# --- 1. by DOI, batches of 40
dois <- unique(na.omit(recs$doi_clean[need]))
for (b in split(dois, ceiling(seq_along(dois) / 40))) {
  items <- cr_get(list(filter = paste0("doi:", b, collapse = ","), rows = length(b), select = "DOI,title,abstract"))
  for (it in items) {
    i <- which(recs$doi_clean == tolower(it$DOI) & need)
    ab <- clean_abs(it$abstract)
    # take it only if the Crossref title matches ours (guards against wrong DOIs)
    ok <- length(i) > 0 && !is.na(ab) && nchar(ab) > 50 &&
      stringsim(norm(it$title[[1]]), norm(recs$title[i[1]]), "jw") >= 0.9
    if (ok) { recs$new_abstract[i] <- ab; recs$source[i] <- "crossref_doi" }
  }
}

# --- 2. title search for records still without abstract and without a DOI
todo <- which(need & is.na(recs$new_abstract) & is.na(recs$doi_clean) & nchar(recs$title) >= 15)
for (i in todo) {
  items <- cr_get(list(query.bibliographic = paste(recs$title[i], surname(recs$author[i])), rows = 3,
                       select = "DOI,title,abstract,issued,author"))
  for (it in items) {
    sim  <- stringsim(norm(it$title[[1]]), norm(recs$title[i]), "jw")
    yr   <- c(suppressWarnings(as.numeric(unlist(it$issued$`date-parts`))), NA)[1]   # NA if no date
    ygap <- if (is.na(yr) || is.na(recs$year[i])) 0 else abs(yr - recs$year[i])
    auth <- paste(map_chr(it$author %||% list(), ~ .x$family %||% ""), collapse = " ") |> norm()
    sn   <- surname(recs$author[i])
    ab   <- clean_abs(it$abstract)
    if (sim >= 0.95 && ygap <= 1 && !is.na(sn) && str_detect(auth, fixed(sn)) && !is.na(ab) && nchar(ab) > 50) {
      recs$new_abstract[i] <- ab; recs$source[i] <- "crossref_title_match"; break
    }
  }
}

# --- 3. Zotero's own extracted full text (local), for records with an attachment
source("R/fulltext_abstract.R")
for (i in which(need & is.na(recs$new_abstract) & !is.na(recs$file))) {
  a <- ft_abstract_for_record(recs$file[i], recs$title[i])
  if (!is.na(a$abstract)) {
    # lead = front matter only: mark it
    recs$new_abstract[i] <- if (a$source == "zotero_fulltext_lead") paste("[front matter only]", a$abstract) else a$abstract
    recs$source[i] <- a$source
  }
}

# --- 4. Semantic Scholar (batch) and Europe PMC (one by one), for records with a DOI and still no abstract
left <- which(need & is.na(recs$new_abstract) & !is.na(recs$doi_clean))
if (length(left) > 0) {
  resp <- tryCatch(request("https://api.semanticscholar.org/graph/v1/paper/batch?fields=title,abstract") |>
    req_body_json(list(ids = paste0("DOI:", recs$doi_clean[left]))) |> req_user_agent("hosyana-review-enrichment (R httr2)") |>
    req_retry(max_tries = 3, backoff = ~ 3) |> req_error(is_error = \(r) FALSE) |> req_perform(), error = function(e) NULL)
  if (!is.null(resp) && resp_status(resp) == 200) {
    hits <- resp_body_json(resp)
    for (k in seq_along(left)) {
      h <- hits[[k]]; i <- left[k]
      if (!is.null(h) && !is.null(h$abstract) && nchar(h$abstract) > 50 &&
          stringsim(norm(h$title %||% ""), norm(recs$title[i]), "jw") >= 0.9) {
        recs$new_abstract[i] <- h$abstract; recs$source[i] <- "semanticscholar_doi"
      }
    }
  }
  for (i in which(need & is.na(recs$new_abstract) & !is.na(recs$doi_clean))) {
    r <- tryCatch(request("https://www.ebi.ac.uk/europepmc/webservices/rest/search") |>
      req_url_query(query = paste0('DOI:"', recs$doi_clean[i], '"'), resultType = "core", format = "json") |>
      req_user_agent("hosyana-review-enrichment (R httr2)") |> req_throttle(rate = 5 / 1) |>
      req_error(is_error = \(x) FALSE) |> req_perform(), error = function(e) NULL)
    if (is.null(r) || resp_status(r) != 200) next
    h <- resp_body_json(r)$resultList$result
    if (length(h) > 0 && !is.null(h[[1]]$abstractText) && nchar(h[[1]]$abstractText) > 50 &&
        stringsim(norm(h[[1]]$title %||% ""), norm(recs$title[i]), "jw") >= 0.9) {
      recs$new_abstract[i] <- clean_abs(h[[1]]$abstractText); recs$source[i] <- "europepmc_doi"
    }
  }
}

# --- write back
recs <- recs |> mutate(abstract = ifelse(!is.na(new_abstract), new_abstract, abstract))
write_csv(recs |> select(sample_no, year, title, abstract), file.path(dir, "records.csv"))
write_csv(recs |> filter(!is.na(source)) |> select(sample_no, source), file.path(dir, "abstracts_added.csv"))

cat("Needed:", sum(need), "| added:", sum(!is.na(recs$source)), "\n")
print(count(recs, source))
k <- key |> left_join(recs |> select(sample_no, abstract), by = "sample_no")
print(k |> count(human_decision, has_abstract = abstract != "[no abstract]"))
