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
members_df <- read_excel("_data/MVLS LTS ORCiD.xlsx") |>
  mutate(
    # Clean up ORCID URLs to extract just the ID
    orcid_id = str_extract(ORCiD, "[0-9]{4}-[0-9]{4}-[0-9]{4}-[0-9]{3}[0-9X]"),
    # Clean up school names for consistency
    school = str_trim(School),
    name = str_trim(Name)
  ) |>
  filter(!is.na(orcid_id))  # Remove any rows without valid ORCID

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
    # Get the preferred work summary (first one)
    work <- work_group$`work-summary`[[1]]
    
    # Extract DOI if available
    doi <- NA_character_
    external_ids <- work$`external-ids`$`external-id`
    if (!is.null(external_ids)) {
      doi_entry <- keep(external_ids, ~ .x$`external-id-type` == "doi")
      if (length(doi_entry) > 0) {
        doi <- doi_entry[[1]]$`external-id-value`
      }
    }
    
    # Extract publication year
    pub_year <- work$`publication-date`$year$value %||% NA_character_
    
    # Extract journal/container title
    journal <- work$`journal-title`$value %||% NA_character_
    
    tibble(
      orcid_id = orcid_id,
      put_code = work$`put-code`,
      title = work$title$title$value %||% NA_character_,
      type = work$type %||% NA_character_,
      year = as.integer(pub_year),
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

# Normalise DOI first (helps matching)
publications2 <- publications |>
  mutate(
    doi = str_trim(doi),
    doi = str_to_lower(doi),
    doi = str_remove(doi, "^https?://(dx\\.)?doi\\.org/"),
    doi = na_if(doi, "")
  )

with_doi <- publications2 |>
  filter(!is.na(doi)) |>
  group_by(doi) |>
  mutate(
    all_authors = paste(unique(author_display[author_display != ""]), collapse = "; "),
    all_authors_with_school = paste(unique(author_with_school[author_with_school != ""]), collapse = "; "),
    all_orcids = paste(unique(orcid_id), collapse = "; "),
    all_schools = paste(unique(school[!is.na(school) & school != ""]), collapse = "; ")
  ) |>
  ungroup() |>
  distinct(doi, .keep_all = TRUE)

no_doi <- publications2 |>
  filter(is.na(doi)) |>
  mutate(
    title_key = title |>
      str_to_lower() |>
      str_replace_all("[[:punct:]]+", " ") |>
      str_squish(),
    journal_key = journal %||% "" |>
      str_to_lower() |>
      str_replace_all("[[:punct:]]+", " ") |>
      str_squish(),
    merge_key = paste0("t:", title_key, "|y:", year, "|ty:", type, "|j:", journal_key)
  ) |>
  group_by(merge_key) |>
  mutate(
    all_authors = paste(unique(author_display[author_display != ""]), collapse = "; "),
    all_authors_with_school = paste(unique(author_with_school[author_with_school != ""]), collapse = "; "),
    all_orcids = paste(unique(orcid_id), collapse = "; "),
    all_schools = paste(unique(school[!is.na(school) & school != ""]), collapse = "; ")
  ) |>
  ungroup() |>
  distinct(merge_key, .keep_all = TRUE) |>
  select(-title_key, -journal_key, -merge_key)

publications_deduped <- bind_rows(with_doi, no_doi) |>
  select(
    title,
    year,
    journal,
    type,
    doi,
    url,
    network_authors = all_authors,
    network_authors_school = all_authors_with_school,
    network_orcids = all_orcids,
    network_schools = all_schools
  ) |>
  arrange(desc(year), title) |>
  filter(!is.na(title), title != "")


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
