# fetch_oa_journal_data.R
# Refreshes the live fields on the "Where to publish" page from DOAJ and
# OpenAlex, and flags anything that needs a human to review the curated data.
#
# Inputs (curated by hand):
#   _data/oa_journals.csv  one row per journal: area, publisher, ISSNs, which
#                          UofG open access route applies (route_key), notes
#                          Optional columns: fee_note (replaces the live fee
#                          on the page) and alert_ignore (free_apc and/or
#                          inactive, to silence a known false alarm)
#   _data/oa_routes.csv    one row per route: category and explanation. When a
#                          University agreement changes, edit the route here
#                          once rather than every journal.
#
# Outputs:
#   _data/oa_journals_live.json    DOAJ and OpenAlex fields per journal
#   _data/oa_journals_changes.csv  running log of changes detected between runs
#   _data/oa_alerts_state.txt      last set of standing alerts (stops repeats)
#   oa_alerts.md                   written only when something new needs review;
#                                  the workflow turns it into a GitHub issue.
#                                  Not committed.
#
# If an API cannot be reached, the previous live values are kept, so a failed
# run never blanks the page.

library(httr2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(jsonlite)

# ---- Configuration ----

polite_mailto   <- "mvls-lts@glasgow.ac.uk"
journals_path   <- "_data/oa_journals.csv"
routes_path     <- "_data/oa_routes.csv"
live_path       <- "_data/oa_journals_live.json"
changes_path    <- "_data/oa_journals_changes.csv"
state_path      <- "_data/oa_alerts_state.txt"
alerts_path     <- "oa_alerts.md"
doaj_batch      <- 20     # ISSNs per DOAJ query (keeps the URL short)
openalex_batch  <- 50     # OpenAlex allows up to 50 values in an OR filter
request_pause   <- 0.5    # seconds between requests
free_category   <- "Free to publish"
stopped_category <- "Not currently an option"

# ---- Helpers ----

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

fmt_price <- function(p) {
  format(round(as.numeric(p)), big.mark = ",", scientific = FALSE, trim = TRUE)
}

#' Fill a glue template for each row of a data frame; empty input gives character(0)
glue_lines <- function(df, template) {
  if (is.null(df) || nrow(df) == 0) return(character())
  as.character(glue::glue_data(df, template, .envir = parent.frame()))
}

price_string <- function(prices) {
  if (length(prices) == 0) return(NA_character_)
  map_chr(prices, \(p) paste(p$currency %||% "", fmt_price(p$price %||% NA))) |>
    paste(collapse = " / ")
}

#' GET a URL and parse JSON; returns NULL (with a message) on failure
get_json <- function(url, query = list()) {
  resp <- tryCatch(
    request(url) |>
      req_url_query(!!!query) |>
      req_user_agent(paste0("MVLS LTS Network site (mailto:", polite_mailto, ")")) |>
      req_retry(max_tries = 3) |>
      req_timeout(60) |>
      req_perform(),
    error = function(e) {
      message("  request failed: ", conditionMessage(e))
      NULL
    }
  )
  Sys.sleep(request_pause)
  if (is.null(resp)) return(NULL)
  resp_body_json(resp, simplifyVector = FALSE)
}

# ---- Read curated data ----

journals <- read_csv(journals_path, col_types = cols(.default = col_character()))
routes   <- read_csv(routes_path, col_types = cols(.default = col_character()))

issn_long <- journals |>
  select(journal, issn_online, issn_print) |>
  pivot_longer(-journal, values_to = "issn") |>
  filter(!is.na(issn), issn != "") |>
  distinct(journal, issn)

all_issns <- unique(issn_long$issn)
message("Journals: ", nrow(journals), "; ISSNs to look up: ", length(all_issns))

old_live <- if (file.exists(live_path)) {
  as_tibble(fromJSON(live_path)$journals)
} else {
  NULL
}

# ---- DOAJ ----

parse_doaj <- function(rec) {
  b   <- rec$bibjson %||% list()
  apc <- b$apc %||% list()
  tibble(
    issns              = list(unlist(c(b$eissn, b$pissn))),
    doaj_title         = b$title %||% NA_character_,
    doaj_has_apc       = isTRUE(apc$has_apc),
    doaj_apc           = if (isTRUE(apc$has_apc)) price_string(apc$max %||% list()) else NA_character_,
    doaj_other_charges = isTRUE(b$other_charges$has_other_charges),
    doaj_licence       = paste(map_chr(b$license %||% list(), \(l) l$type %||% NA_character_), collapse = "; "),
    doaj_url           = paste0("https://doaj.org/toc/", b$eissn %||% b$pissn %||% ""),
    doaj_last_updated  = rec$last_updated %||% NA_character_
  )
}

message("Querying DOAJ...")
doaj_ok <- TRUE
doaj_records <- split(all_issns, ceiling(seq_along(all_issns) / doaj_batch)) |>
  map(\(batch) {
    q   <- paste0("issn:", batch, collapse = " OR ")
    url <- paste0("https://doaj.org/api/search/journals/", str_replace_all(q, " ", "%20"))
    res <- get_json(url, list(pageSize = 100))
    if (is.null(res)) doaj_ok <<- FALSE
    res$results %||% list()
  }) |>
  list_flatten()

doaj <- map(doaj_records, parse_doaj) |> list_rbind()
if (is.null(doaj)) doaj <- tibble()

doaj_match <- if (nrow(doaj) > 0) {
  doaj |>
    mutate(rec = row_number()) |>
    unnest_longer(issns, values_to = "issn") |>
    inner_join(issn_long, by = "issn", relationship = "many-to-many") |>
    distinct(journal, .keep_all = TRUE) |>
    select(journal, starts_with("doaj_"))
} else {
  tibble(journal = character())
}
message("  DOAJ records matched: ", nrow(doaj_match), if (!doaj_ok) " (some requests failed)")

# ---- OpenAlex ----

parse_openalex <- function(src) {
  last_year <- as.integer(format(Sys.Date(), "%Y")) - 1L
  cby  <- src$counts_by_year %||% list()
  prev <- keep(cby, \(y) identical(as.integer(y$year), last_year))
  tibble(
    issns                 = list(unlist(src$issn %||% list())),
    openalex_id           = src$id %||% NA_character_,
    openalex_works        = as.integer(src$works_count %||% 0L),
    openalex_is_oa        = isTRUE(src$is_oa),
    openalex_in_doaj      = isTRUE(src$is_in_doaj),
    openalex_apc          = price_string(src$apc_prices %||% list()),
    openalex_apc_usd      = as.numeric(src$apc_usd %||% NA),
    last_publication_year = as.integer(src$last_publication_year %||% NA),
    works_last_year       = if (length(prev)) as.integer(prev[[1]]$works_count) else 0L
  )
}

message("Querying OpenAlex...")
openalex_ok <- TRUE
openalex_key <- Sys.getenv("OPENALEX_API_KEY")
oa_sources <- split(all_issns, ceiling(seq_along(all_issns) / openalex_batch)) |>
  map(\(batch) {
    query <- list(filter = paste0("issn:", paste(batch, collapse = "|")),
                  `per-page` = 200, mailto = polite_mailto)
    if (nzchar(openalex_key)) query$api_key <- openalex_key
    res <- get_json("https://api.openalex.org/sources", query)
    if (is.null(res)) openalex_ok <<- FALSE
    res$results %||% list()
  }) |>
  list_flatten()

openalex <- map(oa_sources, parse_openalex) |> list_rbind()
if (is.null(openalex)) openalex <- tibble()

openalex_match <- if (nrow(openalex) > 0) {
  openalex |>
    unnest_longer(issns, values_to = "issn") |>
    inner_join(issn_long, by = "issn", relationship = "many-to-many") |>
    arrange(journal, desc(openalex_works)) |>   # duplicates: keep the main record
    distinct(journal, .keep_all = TRUE) |>
    select(journal, starts_with("openalex_"), last_publication_year, works_last_year)
} else {
  tibble(journal = character())
}
message("  OpenAlex records matched: ", nrow(openalex_match), if (!openalex_ok) " (some requests failed)")

# ---- Combine, keeping old values for any source that failed ----

live <- journals |> select(journal)
doaj_cols     <- c("doaj_title", "doaj_has_apc", "doaj_apc", "doaj_other_charges",
                   "doaj_licence", "doaj_url", "doaj_last_updated")
openalex_cols <- c("openalex_id", "openalex_works", "openalex_is_oa", "openalex_in_doaj",
                   "openalex_apc", "openalex_apc_usd", "last_publication_year", "works_last_year")

use_old <- function(cols, ok) !ok && !is.null(old_live) && all(cols %in% names(old_live))

live <- if (use_old(doaj_cols, doaj_ok)) {
  message("  DOAJ incomplete: keeping previous DOAJ values")
  left_join(live, select(old_live, journal, all_of(doaj_cols)), by = "journal")
} else {
  left_join(live, doaj_match, by = "journal")
}
live <- if (use_old(openalex_cols, openalex_ok)) {
  message("  OpenAlex incomplete: keeping previous OpenAlex values")
  left_join(live, select(old_live, journal, all_of(openalex_cols)), by = "journal")
} else {
  left_join(live, openalex_match, by = "journal")
}
for (col in c(doaj_cols, openalex_cols)) if (!col %in% names(live)) live[[col]] <- NA
live <- live |> mutate(in_doaj = !is.na(doaj_title))

fetched_at <- format(Sys.time(), "%Y-%m-%d %H:%M %Z")
write_json(
  list(fetched_at = fetched_at, doaj_ok = doaj_ok, openalex_ok = openalex_ok, journals = live),
  live_path, auto_unbox = TRUE, pretty = TRUE, na = "null"
)
message("Wrote ", live_path)

# ---- Detect changes since the last run ----

watch <- c("in_doaj", "doaj_has_apc", "doaj_apc", "doaj_licence", "openalex_apc")
changes <- tibble(date = character(), journal = character(), field = character(),
                  old = character(), new = character())

if (!is.null(old_live) && doaj_ok && openalex_ok) {
  if (!"in_doaj" %in% names(old_live)) old_live$in_doaj <- !is.na(old_live$doaj_title)
  common <- intersect(watch, names(old_live))
  before <- old_live |> select(journal, all_of(common)) |>
    mutate(across(-journal, as.character)) |>
    pivot_longer(-journal, names_to = "field", values_to = "old")
  after <- live |> select(journal, all_of(common)) |>
    mutate(across(-journal, as.character)) |>
    pivot_longer(-journal, names_to = "field", values_to = "new")
  changes <- inner_join(before, after, by = c("journal", "field")) |>
    filter(coalesce(old, "") != coalesce(new, "")) |>
    mutate(date = as.character(Sys.Date()), .before = 1)
}

if (nrow(changes) > 0) {
  write_csv(changes, changes_path, append = file.exists(changes_path))
  message("Changes detected: ", nrow(changes))
}

# ---- Standing checks on the curated data ----

this_year <- as.integer(format(Sys.Date(), "%Y"))
today     <- as.character(Sys.Date())

checked <- journals |>
  left_join(select(routes, route_key, route_category = category), by = "route_key") |>
  mutate(category = coalesce(category, route_category)) |>
  left_join(live, by = "journal")

standing <- c(
  # Listed as free, but DOAJ now shows a fee
  checked |>
    filter(category == free_category, doaj_has_apc | doaj_other_charges,
           !str_detect(coalesce(alert_ignore, ""), "free_apc")) |>
    glue_lines("- **{journal}** is listed as free to publish, but DOAJ now shows an APC or other charge ({coalesce(doaj_apc, 'other charges')})."),
  # Possibly ceased: nothing indexed in OpenAlex for over a year
  checked |>
    filter(category != stopped_category, !is.na(last_publication_year),
           last_publication_year < this_year - 1,
           !str_detect(coalesce(alert_ignore, ""), "inactive")) |>
    glue_lines("- **{journal}**: OpenAlex has nothing published since {last_publication_year}. Check the journal is still active."),
  # Review dates that have passed
  routes |>
    filter(!is.na(review_by), review_by < today) |>
    glue_lines("- Route **{route_key}** ({category}) was due for review by {review_by}. Check the UofG publisher arrangements page and update _data/oa_routes.csv."),
  journals |>
    filter(!is.na(review_by), review_by < today) |>
    glue_lines("- **{journal}** was due for review by {review_by}. Check its entry in _data/oa_journals.csv.")
)

# ---- Write alerts only when something is new ----

old_state <- if (file.exists(state_path)) readLines(state_path, warn = FALSE) else character()
new_state <- sort(standing)
state_changed <- !identical(new_state, sort(old_state))
writeLines(new_state, state_path)

if (nrow(changes) > 0 || (state_changed && length(new_state) > 0)) {
  change_lines <- if (nrow(changes) > 0) {
    c("## Changes in DOAJ / OpenAlex since the last run", "",
      glue_lines(changes, "- **{journal}**: {field} changed from `{coalesce(old, 'none')}` to `{coalesce(new, 'none')}`"), "")
  }
  standing_lines <- if (length(new_state) > 0) {
    c("## Items to review", "", new_state, "")
  }
  writeLines(c(
    paste("Weekly check of the Where to publish page, run", fetched_at), "",
    change_lines, standing_lines,
    "Edit `_data/oa_journals.csv` or `_data/oa_routes.csv` as needed, then close this issue."
  ), alerts_path)
  message("Wrote ", alerts_path)
}
