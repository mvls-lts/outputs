# fetch_abstracts.R
# Enriches _data/publications.json with abstracts and subject terms so that the
# theme classifier has more than a title to work with.
#
# Source order: OpenAlex (batched, 50 DOIs per request), then Crossref as a
# fallback for anything OpenAlex does not hold.
#
# Results are cached in _data/abstracts.json and the cache is only topped up,
# never refetched, so re-running this is cheap. Delete the cache file to force
# a full refresh.
#
# Run after fetch_orcid_publications.R and before classify_themes.R.

library(httr2)
library(purrr)
library(dplyr)
library(stringr)
library(jsonlite)
library(glue)

# ---- Configuration ----

# OpenAlex asks for a contact address to put you in the polite pool. This is a
# shared network mailbox, not a personal one.
polite_mailto  <- "mvls-lts@glasgow.ac.uk"
batch_size     <- 50      # OpenAlex allows up to 50 values in an OR filter
request_pause  <- 0.3     # seconds between requests
cache_path     <- "_data/abstracts.json"
pub_path       <- "_data/publications.json"

# ---- Helpers ----

`%||%` <- function(x, y) if (is.null(x)) y else x

#' Rebuild readable text from an OpenAlex inverted index
#' @param inv Named list of word -> integer positions
#' @return Single character string, or NA
invert_abstract <- function(inv) {
  if (is.null(inv) || length(inv) == 0) return(NA_character_)
  words     <- rep(names(inv), lengths(inv))
  positions <- unlist(inv, use.names = FALSE)
  if (length(words) == 0) return(NA_character_)
  out <- paste(words[order(positions)], collapse = " ")
  if (str_squish(out) == "") NA_character_ else out
}

#' Strip JATS/XML markup from a Crossref abstract
clean_jats <- function(x) {
  if (is.null(x) || is.na(x)) return(NA_character_)
  out <- x |>
    str_replace_all("<[^>]*>", " ") |>
    str_replace_all("&lt;", "<") |>
    str_replace_all("&gt;", ">") |>
    str_replace_all("&amp;", "&") |>
    str_squish()
  # Journals routinely prefix the abstract with the word "Abstract"
  out <- str_remove(out, regex("^abstract[:\\s]*", ignore_case = TRUE))
  if (out == "") NA_character_ else out
}

#' Normalise a DOI the same way fetch_orcid_publications.R does
normalise_doi <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_remove("^https?://(dx\\.)?doi\\.org/") |>
    na_if("")
}

# ---- Load publications ----

if (!file.exists(pub_path)) {
  stop(glue("{pub_path} not found. Run R/fetch_orcid_publications.R first."))
}

pub_data <- fromJSON(pub_path, simplifyVector = TRUE)
pubs     <- as_tibble(pub_data$publications)

message(glue("Loaded {nrow(pubs)} publications"))

# Only outputs flagged as SoTL need abstracts; the theme page covers those only.
target_dois <- pubs |>
  filter(is_sotl %in% TRUE) |>
  mutate(doi = normalise_doi(doi)) |>
  filter(!is.na(doi)) |>
  # A comma or a pipe inside a DOI would break the OR filter syntax
  filter(!str_detect(doi, "[,|]")) |>
  pull(doi) |>
  unique()

message(glue("SoTL outputs with a usable DOI: {length(target_dois)}"))

# ---- Load cache ----

empty_cache <- tibble(
  doi      = character(),
  abstract = character(),
  subjects = character(),
  source   = character()
)

cache <- if (file.exists(cache_path)) {
  cached <- fromJSON(cache_path, simplifyVector = TRUE)
  if (is.null(cached$records) || length(cached$records) == 0) {
    empty_cache
  } else {
    as_tibble(cached$records)
  }
} else {
  empty_cache
}

# Refetch anything not yet tried. Records previously tried and found empty are
# stored with source = "none" so they are not retried on every run.
to_fetch <- setdiff(target_dois, cache$doi)

message(glue("Already cached: {length(intersect(target_dois, cache$doi))}"))
message(glue("To fetch: {length(to_fetch)}"))

# ---- OpenAlex ----

fetch_openalex_batch <- function(dois) {
  filter_arg <- paste0("doi:", paste(dois, collapse = "|"))

  resp <- tryCatch({
    request("https://api.openalex.org/works") |>
      req_url_query(
        filter     = filter_arg,
        `per-page` = length(dois),
        select     = "doi,title,abstract_inverted_index,topics",
        mailto     = polite_mailto
      ) |>
      req_timeout(60) |>
      req_retry(max_tries = 3, backoff = ~ 2) |>
      req_perform()
  }, error = function(e) {
    message(glue("  OpenAlex batch failed: {e$message}"))
    NULL
  })

  if (is.null(resp) || resp_status(resp) != 200) return(tibble())

  body    <- resp_body_json(resp)
  results <- body$results %||% list()
  if (length(results) == 0) return(tibble())

  map_dfr(results, function(w) {
    doi <- normalise_doi(w$doi %||% NA_character_)
    if (is.na(doi)) return(NULL)

    abstract <- invert_abstract(w$abstract_inverted_index)

    subjects <- if (!is.null(w$topics) && length(w$topics) > 0) {
      paste(map_chr(w$topics, ~ .x$display_name %||% ""), collapse = "; ")
    } else {
      NA_character_
    }

    tibble(
      doi      = doi,
      abstract = abstract %||% NA_character_,
      subjects = subjects,
      source   = "openalex"
    )
  })
}

# Start from the full schema so downstream column references are always valid,
# even when nothing is fetched or every batch comes back empty.
openalex_results <- empty_cache

if (length(to_fetch) > 0) {
  message("\n--- Fetching from OpenAlex ---")
  batches <- split(to_fetch, ceiling(seq_along(to_fetch) / batch_size))

  openalex_results <- bind_rows(empty_cache, imap_dfr(batches, function(dois, i) {
    message(glue("  Batch {i}/{length(batches)} ({length(dois)} DOIs)"))
    Sys.sleep(request_pause)
    fetch_openalex_batch(dois)
  }))

  n_abs <- sum(!is.na(openalex_results$abstract))
  message(glue("  OpenAlex returned {nrow(openalex_results)} record(s), {n_abs} with an abstract"))
}

# ---- Crossref fallback ----

# Anything OpenAlex did not return, or returned without an abstract
still_missing <- setdiff(
  to_fetch,
  openalex_results |> filter(!is.na(abstract)) |> pull(doi)
)

fetch_crossref_one <- function(doi) {
  resp <- tryCatch({
    request(glue("https://api.crossref.org/works/{doi}")) |>
      req_url_query(mailto = polite_mailto) |>
      req_timeout(30) |>
      req_retry(max_tries = 2, backoff = ~ 2) |>
      req_perform()
  }, error = function(e) NULL)

  if (is.null(resp) || resp_status(resp) != 200) return(NULL)

  msg      <- resp_body_json(resp)$message
  abstract <- clean_jats(msg$abstract %||% NA_character_)

  subjects <- if (!is.null(msg$subject) && length(msg$subject) > 0) {
    paste(unlist(msg$subject), collapse = "; ")
  } else {
    NA_character_
  }

  if (is.na(abstract) && is.na(subjects)) return(NULL)

  tibble(
    doi      = doi,
    abstract = abstract,
    subjects = subjects,
    source   = "crossref"
  )
}

crossref_results <- empty_cache

if (length(still_missing) > 0) {
  message(glue("\n--- Crossref fallback for {length(still_missing)} DOI(s) ---"))
  crossref_results <- bind_rows(empty_cache, map_dfr(seq_along(still_missing), function(i) {
    if (i %% 25 == 0) message(glue("  {i}/{length(still_missing)}"))
    Sys.sleep(request_pause)
    fetch_crossref_one(still_missing[i])
  }))
  n_abs <- sum(!is.na(crossref_results$abstract))
  message(glue("  Crossref returned {nrow(crossref_results)} record(s), {n_abs} with an abstract"))
}

# ---- Merge and save ----

new_records <- bind_rows(empty_cache, openalex_results, crossref_results) |>
  filter(!is.na(doi)) |>
  # Prefer the record that actually carries an abstract
  arrange(doi, is.na(abstract)) |>
  distinct(doi, .keep_all = TRUE)

# Mark DOIs we tried and got nothing for, so they are not retried every run
tried_and_empty <- setdiff(to_fetch, new_records$doi)
if (length(tried_and_empty) > 0) {
  new_records <- bind_rows(
    new_records,
    tibble(
      doi      = tried_and_empty,
      abstract = NA_character_,
      subjects = NA_character_,
      source   = "none"
    )
  )
}

cache <- bind_rows(empty_cache, cache, new_records) |>
  arrange(doi, is.na(abstract)) |>
  distinct(doi, .keep_all = TRUE)

write_json(
  list(
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    n_records    = nrow(cache),
    n_abstracts  = sum(!is.na(cache$abstract)),
    records      = cache
  ),
  cache_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

n_target_with_abstract <- cache |>
  filter(doi %in% target_dois, !is.na(abstract)) |>
  nrow()

message(glue("\n✓ Cache written to {cache_path}"))
message(glue("✓ {nrow(cache)} DOI(s) cached, {sum(!is.na(cache$abstract))} with abstracts"))
message(glue("✓ Abstract coverage of SoTL outputs with a DOI: {n_target_with_abstract}/{length(target_dois)}"))
