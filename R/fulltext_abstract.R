# ============================================================
# Abstracts from Zotero's own extracted full text (local, free, no API)
# Zotero keeps text extracted from each PDF in the attachment folder:
#   ~/Zotero/storage/<KEY>/.zotero-ft-cache   (or .zotero-ft-unprocessed)
# The record's `file` field lists the attachment paths; their folders hold these files.
#
# Source("R/fulltext_abstract.R") gives:
#   ft_abstract_for_record(file_field, title) -> list(abstract, source)   (title used to catch wrong attachments)
#     source "zotero_fulltext"       text found after an "Abstract" heading
#     source "zotero_fulltext_lead"  no heading found: first ~1800 characters (lower quality)
# ============================================================

library(stringr)
library(purrr)
library(jsonlite)

# --- 1. find the extracted text file for a record
# `file` field: attachment paths separated by ";"
ft_file_for_record <- function(file_field) {
  if (is.na(file_field) || file_field == "") return(NA_character_)
  dirs <- unique(dirname(str_squish(str_split(file_field, ";")[[1]])))
  cand <- unlist(map(dirs, ~ file.path(.x, c(".zotero-ft-cache", ".zotero-ft-unprocessed"))))
  cand <- cand[file.exists(cand)]
  if (length(cand) == 0) NA_character_ else cand[1]
}

# --- 2. read the text out of the Zotero JSON ({"text": "..."}); "" if broken
# control characters and Unicode non-characters (e.g. in HAL files) break tolower(): turn them into spaces
read_ft_text <- function(f) {
  txt <- tryCatch(fromJSON(f)$text %||% "", error = function(e) "")
  stringi::stri_replace_all_regex(txt, "[\\p{C}&&[^\\n]]", " ")
}

# --- 3a. is this text really about THIS record? (attachments are sometimes the wrong file)
# share of the title's words (4+ letters) that appear in the first 6000 characters
title_in_text <- function(title, txt) {
  words <- unique(str_extract_all(tolower(title), "[a-z]{4,}")[[1]])
  if (length(words) < 2) return(FALSE)          # title too short to check
  head <- tolower(substr(txt, 1, 6000))
  mean(map_lgl(words, ~ str_detect(head, fixed(.x)))) >= 0.6
}

# --- 3b. abstract out of one text
abstract_from_text <- function(txt, title = NA_character_) {
  none <- list(abstract = NA_character_, source = NA_character_)
  if (is.na(txt) || nchar(txt) < 300) return(none)
  # wrong attachment: the text does not mention the record's title words
  if (!is.na(title) && !title_in_text(title, txt)) return(none)

  # glue words split by a hyphen at a line end, then flatten whitespace
  x <- txt |> str_replace_all("-\\s*\\n\\s*", "") |> str_replace_all("\\s+", " ") |> str_squish()
  head <- substr(x, 1, 9000)

  # the heading "Abstract" / "Summary" (whole word), first one in the front matter
  m <- str_locate(head, regex("\\b(abstract|summary)\\b\\s*[:.\\u2014\\u2013-]?\\s*", ignore_case = TRUE))
  if (!is.na(m[1, 1])) {
    body <- substr(head, m[1, 2] + 1, nchar(head))
    # where the abstract stops: keywords / JEL / introduction / copyright
    stop_at <- str_locate(substr(body, 200, nchar(body)),
                          regex("\\b(key\\s?-?words?|index terms|JEL (classification|codes?)|1\\.?\\s+introduction|introduction\\b|I\\.\\s+INTRODUCTION|\\u00a9|copyright)", ignore_case = TRUE))[1, 1]
    end <- if (is.na(stop_at)) min(2500, nchar(body)) else min(2500, 199 + stop_at - 1)
    ab <- str_squish(substr(body, 1, end))
    # a table of contents ("I. Introduction II. ...") is not an abstract
    is_toc <- str_detect(ab, regex("^(I\\.|1\\.?)\\s+introduction", ignore_case = TRUE)) ||
      str_count(ab, "\\b\\d+(\\.\\d+)+\\s+[A-Z]") >= 4      # many "3.2.1 Heading" = a contents list
    if (nchar(ab) >= 150 && !is_toc) return(list(abstract = ab, source = "zotero_fulltext"))
  }
  # no usable heading: front matter as a stand-in (title, authors, first lines)
  list(abstract = substr(x, 1, 1800), source = "zotero_fulltext_lead")
}

# --- 4. one call per record
ft_abstract_for_record <- function(file_field, title = NA_character_) {
  f <- ft_file_for_record(file_field)
  if (is.na(f)) return(list(abstract = NA_character_, source = NA_character_))
  abstract_from_text(read_ft_text(f), title)
}
