# fetch_orcid_publications.R
# Fetches publications from ORCID API for all network members
# and saves them as a structured data file for the Quarto website

library(httr2)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(jsonlite)
library(glue)
library(readxl)

# ---- Configuration ----

# Keywords that suggest a publication is SoTL/education-related
# Applied to title (case-insensitive)
sotl_keywords <- c(

  # Core SoTL terms
  "teaching", "learning", "pedagog", "education", "student",
  "curriculum", "assessment", "classroom", "instruction",
  "academic development", "faculty development", "staff development",
  

  # Higher education specific
  "higher education", "undergraduate", "postgraduate", "graduate student",
  "university student", "medical student", "dental student", "veterinary student",
  "first.?year", "transition", "widening participation", "access programme",
  
  # Teaching methods and approaches
  "flipped", "blended learning", "online learning", "e-learning", "elearning",
  "active learning", "problem.?based learning", "team.?based learning",
  "simulation", "virtual patient", "objective structured clinical examination", "osce",
  "feedback", "formative", "summative", "peer instruction", "peer assessment",
  
  # Educational research
  "educational research", "scholarship of teaching", "sotl",
  "course design", "module design", "learning outcome", "competenc",
  "self.?regulated learning", "metacognition", "study skill", "academic skill",
  "student engagement", "student experience", "student retention", "attainment gap",
  
  # Professional education
  "clinical education", "clinical teaching", "medical education",
  "health profession", "healthcare education", "interprofessional",
  "work.?based learning", "placement", "practicum", "clerkship",
  
  # Assessment and evaluation
  "exam", "test.?taking", "grading", "marking", "rubric",
  "programmatic assessment", "portfolio", "reflective practice"
)

# Known education/SoTL journals (case-insensitive matching)
# Add journals relevant to your network's disciplines
sotl_journals <- c(
  # General higher education
  "higher education", "studies in higher education", "teaching in higher education",
  "higher education research", "research in higher education",
  "journal of higher education", "review of higher education",
  "active learning in higher education", "arts and humanities in higher education",
  "innovations in education and teaching international",
  "international journal for academic development",
  "journal of further and higher education",
  "quality in higher education",
  
  # SoTL journals
  "scholarship of teaching and learning", "teaching and learning inquiry",
  "international journal for the scholarship of teaching",
  "journal of the scholarship of teaching and learning",
  "practice and evidence of scholarship of teaching",
  
  # Medical/health professions education
  "medical education", "academic medicine", "medical teacher",
  "bmc medical education", "advances in health sciences education",
  "clinical teacher", "journal of graduate medical education",
  "perspectives on medical education", "teaching and learning in medicine",
  "anatomical sciences education", "biochemistry and molecular biology education",
  "advances in physiology education", "journal of dental education",
  "journal of veterinary medical education", "nurse education today",
  "nurse education in practice", "journal of nursing education",
  "pharmacy education", "american journal of pharmaceutical education",
  "currents in pharmacy teaching and learning",
  "journal of surgical education", "simulation in healthcare",
  
  # Science education
  "journal of chemical education", "chemistry education research and practice",
  "journal of biological education", "cbe life sciences education",
  "journal of science education", "international journal of science education",
  "journal of research in science teaching", "science education",
  "journal of geoscience education", "physics education",
  "european journal of physics", "american journal of physics",
  
  # Psychology education
  "teaching of psychology", "psychology learning and teaching",
  "scholarship of teaching and learning in psychology",
  
  # Other discipline-specific
  "journal of economic education", "journal of legal education",
  "journal of management education", "academy of management learning",
  "journal of marketing education", "journal of engineering education",
  "computer science education", "journal of statistics education",
  "mathematics education", "journal of mathematics teacher education",
  
  # Educational technology
  "british journal of educational technology", "computers and education",
  "internet and higher education", "journal of computer assisted learning",
  "educational technology research and development",
  
  # Educational psychology/research methods
  "educational psychology", "contemporary educational psychology",
  "learning and instruction", "educational researcher",
  "review of educational research", "assessment and evaluation in higher education",
  "assessment in education", "studies in educational evaluation"
)

# List of ORCID IDs for network members
# Add or remove IDs as needed
# ---- Load ORCID IDs from Excel ----

# Read the Excel file with network members
# Source is now a Microsoft Form export, so column headers are the full
# question text (e.g. "In which School are you based?") rather than short
# labels. Rename them back to ORCiD / School before processing. Matching on
# keywords keeps this robust to minor wording changes in the form.
members_df <- read_excel("_data/MVLS LTS ORCiD.xlsx") |>
  rename_with(~ "ORCiD", .cols = matches("(?i)orcid")) |>
  rename_with(~ "School", .cols = matches("(?i)school")) |>
  mutate(
    # Clean up ORCID URLs to extract just the ID
    orcid_id = str_extract(ORCiD, "[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]"),
    # Clean up school names for consistency
    school = str_trim(School),
    name = str_trim(Name)
  ) |>
  filter(!is.na(orcid_id)) |>  # Remove any rows without valid ORCID
  distinct(orcid_id, .keep_all = TRUE)  # Drop duplicate form submissions, keep first

orcid_ids <- members_df$orcid_id

# ---- Helper Functions ----

#' Fetch author name and affiliation from ORCID
#' @param orcid_id Character string of ORCID ID
#' @return Named list with given_name, family_name, and affiliation
get_orcid_name <- function(orcid_id) {
  
  url <- glue("https://pub.orcid.org/v3.0/{orcid_id}/person")
  
  resp <- tryCatch({
    request(url) |>
      req_headers(Accept = "application/json") |>
      req_perform()
  }, error = function(e) {
    message(glue("Error fetching name for {orcid_id}: {e$message}"))
    return(NULL)
  })
  
  if (is.null(resp)) return(list(given_name = NA, family_name = NA, affiliation = NA))
  
  data <- resp_body_json(resp)
  
  # Extract name
  given_name <- data$name$`given-names`$value %||% NA_character_
  family_name <- data$name$`family-name`$value %||% NA_character_
  
  # Now fetch employments for affiliation
  affiliation <- NA_character_
  emp_url <- glue("https://pub.orcid.org/v3.0/{orcid_id}/employments")
  
  emp_resp <- tryCatch({
    request(emp_url) |>
      req_headers(Accept = "application/json") |>
      req_perform()
  }, error = function(e) {
    return(NULL)
  })
  
  if (!is.null(emp_resp) && resp_status(emp_resp) == 200) {
    emp_data <- resp_body_json(emp_resp)
    
    # Get the most recent/current affiliation
    # ORCID returns affiliation-group, each containing summaries
    if (length(emp_data$`affiliation-group`) > 0) {
      # Try to find current employment (no end date) first
      for (group in emp_data$`affiliation-group`) {
        summaries <- group$summaries
        if (length(summaries) > 0) {
          for (summary in summaries) {
            emp <- summary$`employment-summary`
            # Check if this is current (no end date)
            if (is.null(emp$`end-date`) || is.null(emp$`end-date`$year)) {
              org_name <- emp$organization$name %||% NA_character_
              if (!is.na(org_name)) {
                affiliation <- org_name
                break
              }
            }
          }
          if (!is.na(affiliation)) break
        }
      }
      
      # If no current employment found, take the first one
      if (is.na(affiliation) && length(emp_data$`affiliation-group`) > 0) {
        first_group <- emp_data$`affiliation-group`[[1]]
        if (length(first_group$summaries) > 0) {
          first_emp <- first_group$summaries[[1]]$`employment-summary`
          affiliation <- first_emp$organization$name %||% NA_character_
        }
      }
    }
  }
  
  list(
    given_name = given_name,
    family_name = family_name,
    affiliation = affiliation
  )
}

#' Fetch all works (publications) for an ORCID ID
#' @param orcid_id Character string of ORCID ID
#' @return Tibble of publications
get_orcid_works <- function(orcid_id) {
  
  message(glue("Fetching publications for ORCID: {orcid_id}"))
  
  url <- glue("https://pub.orcid.org/v3.0/{orcid_id}/works")
  
  resp <- tryCatch({
    request(url) |>
      req_headers(Accept = "application/json") |>
      req_perform()
  }, error = function(e) {
    message(glue("  Error: {e$message}"))
    return(tibble())
  })

  
  if (resp_status(resp) != 200) {
    message(glue("  HTTP error: {resp_status(resp)}"))
    return(tibble())
  }
  
  data <- resp_body_json(resp)
  
  works <- data$group
  
  if (length(works) == 0) {
    message("  No publications found")
    return(tibble())
  }
  
  message(glue("  Found {length(works)} publication(s)"))
  
  # Extract relevant fields from each work
  pubs <- map_dfr(works, function(work_group) {
    summaries <- work_group$`work-summary`
    # Use first summary as the primary source for title, type, year, and url
    work <- summaries[[1]]

    # Extract DOI and journal — scan all summaries for the first non-NA value,
    # since ORCID's preferred summary is not always the richest source
    doi <- NA_character_
    journal <- NA_character_
    for (summary in summaries) {
      if (is.na(doi)) {
        external_ids <- summary$`external-ids`$`external-id`
        if (!is.null(external_ids)) {
          doi_entry <- keep(external_ids, ~ .x$`external-id-type` == "doi")
          if (length(doi_entry) > 0) {
            doi <- doi_entry[[1]]$`external-id-value`
          }
        }
      }
      if (is.na(journal)) {
        journal <- summary$`journal-title`$value %||% NA_character_
      }
      if (!is.na(doi) && !is.na(journal)) break
    }

    # Extract publication date (month and day are often absent on ORCID records)
    pub_year  <- work$`publication-date`$year$value  %||% NA_character_
    pub_month <- work$`publication-date`$month$value %||% NA_character_
    pub_day   <- work$`publication-date`$day$value   %||% NA_character_

    tibble(
      orcid_id = orcid_id,
      put_code = work$`put-code`,
      title = work$title$title$value %||% NA_character_,
      type = work$type %||% NA_character_,
      year = as.integer(pub_year),
      month = as.integer(pub_month),
      day = as.integer(pub_day),
      journal = journal,
      doi = doi,
      url = if (!is.na(doi)) glue("https://doi.org/{doi}") else work$url$value %||% NA_character_
    )
  })
  
  return(pubs)
}

#' Format author names for display
#' @param authors_df Data frame with given_name and family_name columns
#' @return Character string of formatted author names
format_authors <- function(authors_df) {
  authors_df |>
    mutate(
      display_name = case_when(
        is.na(given_name) & is.na(family_name) ~ "Unknown",
        is.na(given_name) ~ family_name,
        is.na(family_name) ~ given_name,
        TRUE ~ paste(family_name, given_name, sep = ", ")
      )
    ) |>
    pull(display_name) |>
    paste(collapse = "; ")
}

# ---- Main Execution ----

# First, fetch all author names and merge with Excel data
message("\n--- Fetching author names ---")
authors_orcid <- map_dfr(orcid_ids, function(id) {
  Sys.sleep(0.5)  # Be polite to the API
  name <- get_orcid_name(id)
  tibble(
    orcid_id = id,
    given_name = name$given_name,
    family_name = name$family_name,
    affiliation = name$affiliation
  )
})

# Merge with Excel data to get School
authors <- authors_orcid |>
  left_join(
    members_df |> select(orcid_id, name, school),
    by = "orcid_id"
  ) |>
  # Use Excel name if available, otherwise construct from ORCID
  mutate(
    display_name = if_else(
      !is.na(name),
      name,
      paste(given_name, family_name) |> str_trim()
    )
  )

message(glue("\nSuccessfully retrieved {sum(!is.na(authors$family_name))} author names"))
message(glue("Successfully retrieved {sum(!is.na(authors$affiliation))} affiliations"))
message(glue("Successfully matched {sum(!is.na(authors$school))} schools from Excel"))

# Fetch all publications
message("\n--- Fetching publications ---")
all_publications <- map_dfr(orcid_ids, function(id) {
  Sys.sleep(0.5)  # Be polite to the API
  get_orcid_works(id)
})

message(glue("\nTotal raw publications: {nrow(all_publications)}"))

# Join author names to publications
publications <- all_publications |>
  left_join(authors, by = "orcid_id") |>
  mutate(
    author_display = paste(
      if_else(is.na(given_name), "", given_name),
      if_else(is.na(family_name), "", family_name)
    ) |> str_trim(),
    author_with_school = if_else(
      !is.na(school) & school != "",
      glue("{display_name} [{school}]"),
      display_name
    )
  )

# ---- Deduplication / network-author aggregation ----

# Helper: first non-NA value in a vector, or a typed NA if none are present
first_non_na <- function(x) {
  x_present <- x[!is.na(x)]
  if (length(x_present) > 0) x_present[1] else x[NA_integer_]
}

# Helper: normalise a title string for cross-stream fuzzy matching
normalise_title <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[[:punct:]]+", " ") |>
    str_squish()
}

# Helper: de-duplicate a "; "-separated list, keeping the first occurrence of
# each entry (case-insensitive). Used to tidy the aggregated author/orcid/school
# lists after cross-stream merges, which can otherwise repeat a name.
dedupe_semicolon_list <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || s == "") return(s)
    parts <- str_trim(str_split(s, ";\\s*")[[1]])
    parts <- parts[parts != ""]
    parts <- parts[!duplicated(str_to_lower(parts))]
    paste(parts, collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}

# Helper: de-duplicate an authors-with-school list by *person* (the text before
# the " [School ...]" suffix), keeping the first occurrence. This absorbs cases
# where the same person carries slightly different school spellings (e.g.
# "& Wellbeing" vs "and Wellbeing") which a plain string de-dupe would miss.
dedupe_authors_school_list <- function(x) {
  vapply(x, function(s) {
    if (is.na(s) || s == "") return(s)
    parts <- str_trim(str_split(s, ";\\s*")[[1]])
    parts <- parts[parts != ""]
    person <- str_trim(str_remove(parts, "\\s*\\[.*$"))
    parts <- parts[!duplicated(str_to_lower(person))]
    paste(parts, collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
}

# Normalise DOI (strip URL prefix, lowercase, strip OSF version suffixes)
publications2 <- publications |>
  mutate(
    doi = str_trim(doi),
    doi = str_to_lower(doi),
    doi = str_remove(doi, "^https?://(dx\\.)?doi\\.org/"),
    doi = str_remove(doi, "[._]v\\d+$"),  # collapse versioned DOIs (e.g. _v1, .v1, .v2 from OSF/figshare)
    doi = na_if(doi, "")
  )

with_doi <- publications2 |>
  filter(!is.na(doi)) |>
  group_by(doi) |>
  mutate(
    all_authors = paste(unique(author_display[author_display != ""]), collapse = "; "),
    all_authors_with_school = paste(unique(author_with_school[author_with_school != ""]), collapse = "; "),
    all_orcids = paste(unique(orcid_id), collapse = "; "),
    all_schools = paste(unique(school[!is.na(school) & school != ""]), collapse = "; "),
    # Keep the most complete date across duplicate records of the same DOI
    year = first_non_na(year),
    month = first_non_na(month),
    day = first_non_na(day)
  ) |>
  ungroup() |>
  distinct(doi, .keep_all = TRUE) |>
  mutate(title_key = normalise_title(title))

no_doi <- publications2 |>
  filter(is.na(doi)) |>
  # Merge no-DOI records on normalised title alone. Two network members logging
  # the same un-DOI'd output (e.g. a conference abstract) often record slightly
  # different venue strings, years, or types, so title is the only reliable key.
  mutate(merge_key = normalise_title(title)) |>
  group_by(merge_key) |>
  mutate(
    all_authors = paste(unique(author_display[author_display != ""]), collapse = "; "),
    all_authors_with_school = paste(unique(author_with_school[author_with_school != ""]), collapse = "; "),
    all_orcids = paste(unique(orcid_id), collapse = "; "),
    all_schools = paste(unique(school[!is.na(school) & school != ""]), collapse = "; "),
    # Keep the most complete metadata across the merged records
    journal = first_non_na(journal),
    type = first_non_na(type),
    year = first_non_na(year),
    month = first_non_na(month),
    day = first_non_na(day)
  ) |>
  ungroup() |>
  distinct(merge_key, .keep_all = TRUE) |>
  rename(title_key = merge_key)

# Title fallback: attach no-DOI rows to DOI rows where the normalised title matches.
# This catches cases where one author's record has a DOI and another's does not,
# even when the year or other metadata differs between the two records.
no_doi_matched   <- no_doi |> semi_join(with_doi, by = "title_key")
no_doi_unmatched <- no_doi |> anti_join(with_doi, by = "title_key")

if (nrow(no_doi_matched) > 0) {
  message(glue("  Merging {nrow(no_doi_matched)} no-DOI row(s) into DOI rows via title match"))

  extra <- no_doi_matched |>
    group_by(title_key) |>
    summarise(
      extra_authors        = paste(unique(all_authors[all_authors != ""]),             collapse = "; "),
      extra_authors_school = paste(unique(all_authors_with_school[all_authors_with_school != ""]), collapse = "; "),
      extra_orcids         = paste(unique(all_orcids[all_orcids != ""]),               collapse = "; "),
      extra_schools        = paste(unique(all_schools[!is.na(all_schools) & all_schools != ""]), collapse = "; "),
      .groups = "drop"
    )

  with_doi <- with_doi |>
    left_join(extra, by = "title_key") |>
    mutate(
      all_authors = case_when(
        !is.na(extra_authors) & extra_authors != "" ~
          paste(all_authors, extra_authors, sep = "; "),
        TRUE ~ all_authors
      ),
      all_authors_with_school = case_when(
        !is.na(extra_authors_school) & extra_authors_school != "" ~
          paste(all_authors_with_school, extra_authors_school, sep = "; "),
        TRUE ~ all_authors_with_school
      ),
      all_orcids = case_when(
        !is.na(extra_orcids) & extra_orcids != "" ~
          paste(all_orcids, extra_orcids, sep = "; "),
        TRUE ~ all_orcids
      ),
      all_schools = case_when(
        !is.na(extra_schools) & extra_schools != "" ~
          paste(all_schools, extra_schools, sep = "; "),
        TRUE ~ all_schools
      )
    ) |>
    select(-starts_with("extra_"))
}

publications_deduped <- bind_rows(
    with_doi        |> select(-title_key),
    no_doi_unmatched |> select(-title_key)
  ) |>
  mutate(
    # Construct a sortable date. Missing month/day default to the start of the
    # period so that year-only records still order sensibly against fuller dates.
    pub_date = suppressWarnings(as.Date(sprintf(
      "%04d-%02d-%02d",
      year,
      dplyr::coalesce(month, 1L),
      dplyr::coalesce(day, 1L)
    )))
  ) |>
  select(
    title,
    year,
    month,
    day,
    pub_date,
    journal,
    type,
    doi,
    url,
    network_authors = all_authors,
    network_authors_school = all_authors_with_school,
    network_orcids = all_orcids,
    network_schools = all_schools
  ) |>
  # Tidy aggregated lists: cross-stream merges can repeat a name/orcid/school.
  mutate(
    network_authors        = dedupe_semicolon_list(network_authors),
    network_authors_school = dedupe_authors_school_list(network_authors_school),
    network_orcids         = dedupe_semicolon_list(network_orcids),
    network_schools        = dedupe_semicolon_list(network_schools)
  ) |>
  arrange(desc(pub_date), desc(year), title) |>
  filter(!is.na(title), title != "")

# ---- Preprint suppression ----
# When a non-preprint row exists with the same normalised title, suppress the preprint.
# This handles the common case of a paper appearing as both a preprint and a journal article.

# A record counts as a preprint if its type says so, OR if its journal/DOI points
# to a known preprint server. ORCID often types preprints as "journal-article" or
# "other", so the type field alone misses many of them.
is_preprint <- function(type, journal, doi) {
  type_l    <- str_to_lower(coalesce(type, ""))
  source    <- str_to_lower(paste(coalesce(journal, ""), coalesce(doi, "")))
  doi_l     <- str_to_lower(coalesce(doi, ""))
  str_detect(type_l, "preprint") |
    str_detect(source, "biorxiv|medrxiv|chemrxiv|psyarxiv|arxiv|ssrn|research ?square|preprint") |
    str_detect(doi_l, "^10\\.1101/|^10\\.21203/|^10\\.31234/|^10\\.31219/|^10\\.2139/|^10\\.26434/")
}

publications_deduped <- publications_deduped |>
  mutate(
    title_key   = normalise_title(title),
    .is_preprint = is_preprint(type, journal, doi)
  )

titles_with_published_version <- publications_deduped |>
  filter(!.is_preprint) |>
  pull(title_key) |>
  unique()

n_suppressed <- publications_deduped |>
  filter(.is_preprint, title_key %in% titles_with_published_version) |>
  nrow()

publications_deduped <- publications_deduped |>
  filter(!(.is_preprint & title_key %in% titles_with_published_version)) |>
  select(-title_key, -.is_preprint)

if (n_suppressed > 0) {
  message(glue("  Suppressed {n_suppressed} preprint(s) superseded by a published version"))
}


message(glue("\nDeduplicated publications: {nrow(publications_deduped)}"))

# ---- SoTL Classification ----

#' Classify a publication as SoTL/education-related
#' @param title Publication title
#' @param journal Journal name
#' @return List with is_sotl (logical) and matched_on (character)
classify_sotl <- function(title, journal) {
  title_lower <- tolower(title %||% "")
  journal_lower <- tolower(journal %||% "")
  
  # Handle empty strings after NA conversion

if (is.na(journal_lower) || journal_lower == "") {
    journal_lower <- ""
  }
  if (is.na(title_lower) || title_lower == "") {
    title_lower <- ""
  }

  # Check journal first (more reliable signal)
  # Only check if journal is not empty
  if (nchar(journal_lower) > 0) {
    journal_match <- any(map_lgl(sotl_journals, ~ str_detect(journal_lower, fixed(.x))), na.rm = TRUE)
    if (isTRUE(journal_match)) {
      return(list(is_sotl = TRUE, matched_on = "journal"))
    }
  }
  
  # Then check title keywords
  # Only check if title is not empty
  if (nchar(title_lower) > 0) {
    keyword_match <- any(map_lgl(sotl_keywords, ~ str_detect(title_lower, regex(.x, ignore_case = TRUE))), na.rm = TRUE)
    if (isTRUE(keyword_match)) {
      return(list(is_sotl = TRUE, matched_on = "keyword"))
    }
  }
  
  return(list(is_sotl = FALSE, matched_on = NA_character_))
}

# Apply classification
message("\n--- Classifying SoTL publications ---")

publications_classified <- publications_deduped |>
  rowwise() |>
  mutate(
    sotl_result = list(classify_sotl(title, journal)),
    is_sotl = sotl_result$is_sotl,
    sotl_matched_on = sotl_result$matched_on
  ) |>
  ungroup() |>
  select(-sotl_result)

n_sotl <- sum(publications_classified$is_sotl)
n_sotl_journal <- sum(publications_classified$sotl_matched_on == "journal", na.rm = TRUE)
n_sotl_keyword <- sum(publications_classified$sotl_matched_on == "keyword", na.rm = TRUE)

message(glue("  SoTL publications identified: {n_sotl}"))
message(glue("    - Matched by journal: {n_sotl_journal}"))
message(glue("    - Matched by keyword: {n_sotl_keyword}"))

# Use the classified data
publications_final <- publications_classified

# Create output directory if needed
if (!dir.exists("_data")) dir.create("_data")

# Save as JSON for the website
write_json(
  list(
    last_updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    n_authors = nrow(authors),
    n_publications = nrow(publications_final),
    n_sotl = sum(publications_final$is_sotl),
    authors = authors,
    publications = publications_final,
    sotl_keywords = sotl_keywords,
    sotl_journals = sotl_journals
  ),
  "_data/publications.json",
  pretty = TRUE,
  auto_unbox = TRUE
)

# Also save as RDS for easier R manipulation
saveRDS(
  list(
    last_updated = Sys.time(),
    authors = authors,
    publications = publications_final,
    sotl_keywords = sotl_keywords,
    sotl_journals = sotl_journals
  ),
  "_data/publications.rds"
)

message(glue("\n✓ Data saved to _data/publications.json and _data/publications.rds"))
message(glue("✓ {nrow(publications_final)} unique publications from {nrow(authors)} authors"))
message(glue("✓ {sum(publications_final$is_sotl)} classified as SoTL/education-related"))
