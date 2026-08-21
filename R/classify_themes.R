# classify_themes.R
# Groups the SoTL outputs into browsable themes by keyword matching over
# title, journal and (where available) abstract.
#
# This is a browsing aid, not a taxonomy. It will be wrong sometimes. Corrections
# go in _data/theme_overrides.csv, which is applied last and always wins.
#
# Run after fetch_orcid_publications.R and fetch_abstracts.R.
#
# Scoring
#   Each theme holds a set of regex patterns. For one output:
#     title    : 3 points per distinct pattern matched
#     journal  : 2 points if any pattern matches
#     abstract : 1 point per distinct pattern matched, capped at 4
#   A theme is assigned if its score reaches `score_threshold`. This means a
#   single title hit is enough, a single journal or abstract mention is not.
#   At most `max_themes` themes are kept per output, highest scoring first.

library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(jsonlite)
library(glue)

# ---- Configuration ----

score_threshold <- 3
max_themes      <- 3
w_title         <- 3
w_journal       <- 2
w_abstract      <- 1
abstract_cap    <- 4   # stop long abstracts dominating on repetition

pub_path       <- "_data/publications.json"
abstract_path  <- "_data/abstracts.json"
override_path  <- "_data/theme_overrides.csv"
out_path       <- "_data/themes.json"

# ---- Theme definitions ----
#
# Revised August 2026 following the network symposium. Seven topic themes plus
# one cross-cutting tag applied in addition to them. See
# Themes/theme-revision-proposal.md for the rationale.

theme_defs <- list(
  list(
    id    = "teaching-practice",
    label = "Teaching practice & curriculum design",
    blurb = "Pedagogy, active and problem-based learning, flipped classrooms, course and programme design, experiential and service learning, study skills and metacognition.",
    patterns = c(
      "pedagog", "active learning", "experiential learning", "problem.?based learning",
      "team.?based learning", "case.?based learning", "enquiry.?based", "inquiry.?based",
      "flipped", "small.?group (teaching|learning|work)", "large.?class", "classroom",
      "lecture(s|r|rs)?\\b", "tutorial", "seminar", "workshop",
      "curricul", "course design", "module design", "programme design", "syllabus",
      "teaching (method|approach|practice|strateg|innovation)", "instructional design",
      "service learning", "field course", "fieldwork",
      "laboratory (class|teaching|practical)", "practical class",
      "retrieval practice", "spaced (practice|learning|repetition)",
      "self.?regulated learning", "metacogniti", "study skill", "note.?taking",
      "constructive alignment", "learning outcome", "signature pedagog", "embodied learning"
    )
  ),
  list(
    id    = "digital-simulation",
    label = "Digital, simulation & immersive learning",
    blurb = "Learning technology, blended and online delivery, lecture capture, generative AI, simulation, virtual reality, 3D printing, anatomical visualisation and learning analytics.",
    patterns = c(
      "learning technolog", "educational technolog", "technology.?enhanced",
      "digital (learning|education|literac|tool|technolog|resource|skill)",
      "online (learning|teaching|delivery|course|education|module)", "e-?learning",
      "blended (learning|delivery|teaching)", "hybrid (learning|teaching|delivery)",
      "distance learning", "mooc", "lecture (capture|recording)", "recorded lecture",
      "generative a\\.?i\\.?\\b", "\\bgenai\\b", "chatgpt", "large language model",
      "\\bllm(s)?\\b", "artificial intelligence", "machine learning",
      "simulat", "manikin", "mannequin", "virtual patient", "standardi[sz]ed patient",
      "virtual reality", "\\bvr\\b", "augmented reality", "immersive",
      "3.?d.?print", "three.?dimensional model", "virtual microscop", "digital pathology",
      "visuali[sz]ation", "anatom(y|ical) (model|education|teaching|learning)",
      "learning analytic", "educational data mining", "\\bapp\\b", "mobile learning",
      "video (resource|based|tutorial)", "podcast", "screencast", "virtual clinical",
      "telehealth", "telemedicine", "gamif", "serious game",
      "virtual learning environment"
    )
  ),
  list(
    id    = "assessment",
    label = "Assessment, feedback & academic integrity",
    blurb = "Assessment design, authentic and programmatic assessment, feedback, OSCEs, marking and moderation, plagiarism and academic integrity.",
    patterns = c(
      "assessment", "assessing", "\\bassessed\\b", "assignment", "coursework",
      "feedback", "feed.?forward",
      "exam(ination|s)?\\b", "\\bosce", "objective structured clinical",
      "grading", "\\bgrade", "marking", "marker", "moderation", "rubric",
      "multiple.?choice", "\\bmcq", "open.?book", "take.?home (exam|assessment)",
      "summative", "formative", "programmatic assessment", "portfolio",
      "entrustable professional activit", "\\bepa(s)?\\b",
      "plagiar", "academic integrity", "academic misconduct", "contract cheating",
      "authentic assessment", "peer assessment", "self.?assessment", "peer review"
    )
  ),
  list(
    id    = "inclusion-wellbeing",
    label = "Inclusive education & student wellbeing",
    blurb = "EDI, decolonising the curriculum, accessibility and disability, neurodiversity, belonging, awarding gaps, and the wellbeing and mental health of students and staff.",
    patterns = c(
      "inclusi(ve|on|vity)", "exclusion", "equity", "equalit", "diversity", "\\bedi\\b",
      "decoloni", "widening participation", "widening access", "access programme",
      "accessib", "disabilit", "disabled", "assistive technolog", "universal design",
      "\\budl\\b", "autis", "neurodiver", "dyslexi", "\\badhd\\b",
      "belonging", "attainment gap", "awarding gap", "degree gap",
      "marginali[sz]", "under.?represent", "under.?served",
      "gender", "ethnic", "racial", "racism", "\\brace\\b",
      "\\blgbt", "queer", "trans(gender)? (student|staff|inclusion)", "rainbow",
      "socio.?economic", "first.?generation", "care.?experienced", "estranged student",
      "international student", "epistemic (justice|injustice)", "hidden curriculum",
      "content warning", "trigger warning", "sensitivit",
      "well.?being", "mental health", "anxiet", "\\bstress\\b", "burnout", "resilien",
      "loneli", "isolation", "imposter", "psychological safety", "substance use",
      "emotion(al)? (regulation|labour|work)", "perfectionism", "student support"
    )
  ),
  list(
    id    = "employability",
    label = "Employability, skills & professional identity",
    blurb = "Graduate attributes, work-based learning, placements and clerkships, careers, interprofessional education, and professional and clinical skills.",
    patterns = c(
      "employab", "graduate attribute", "graduate skill", "transferable skill",
      "work.?based learning", "work.?related learning", "workplace learning",
      "placement", "clerkship", "internship", "work experience", "apprenticeship",
      "career", "employer", "job market", "recruitment",
      "professional identit", "professionalism", "professional development",
      "interprofessional", "multi.?professional",
      "clinical skill", "communication skill", "consultation skill", "practical skill",
      "competenc", "capabilit", "skills? (gap|development|training|acquisition)",
      "out of hours", "workforce", "practice.?ready", "work.?readiness",
      "leadership (training|development|skill)", "entrepreneur"
    )
  ),
  list(
    id    = "transitions-voice",
    label = "Transitions, engagement & student voice",
    blurb = "Transition and induction, first-year experience, retention, student–staff partnership and co-creation, peer-assisted learning, and widening access outreach.",
    patterns = c(
      "transition (to|into|from)", "first.?year experience", "induction", "orientation",
      "\\bretention\\b", "attrition", "drop.?out", "progression",
      "student voice", "student.?staff partnership", "students as partners",
      "co.?creation", "co.?design", "co.?production", "student.?led", "student partner",
      "student engagement", "attendance", "participation",
      "peer.?assisted (learning|study)", "\\bpals\\b", "peer mentor", "peer support",
      "school pupil", "pre.?entry", "readiness for university",
      "student representat", "student experience", "student perception",
      "student expectation", "outreach", "public engagement", "community engagement"
    )
  ),
  list(
    id    = "sustainability-global",
    label = "Sustainability & global/civic education",
    blurb = "Sustainability and the SDGs, planetary and global health, civic and public engagement, and global citizenship.",
    patterns = c(
      "sustainab", "climate", "environmental (education|literac|sustainab)",
      "carbon", "planetary health", "\\bsdg", "sustainable development goal",
      "one health", "global health", "global mental health", "global citizen",
      "civic (education|engagement|university)", "internationali[sz]ation",
      "decarbon", "green (curriculum|skill)", "ecolog"
    )
  )
)

# Cross-cutting: applied in addition to a topic theme, never instead of one.
cross_def <- list(
  id    = "sotl-methods",
  label = "SoTL methods, data & open scholarship",
  blurb = "Cross-cutting. Research methods and study design, statistics and data skills, open and reproducible scholarship, OERs, evaluation and review methodology.",
  patterns = c(
    "open (science|research|scholarship|access|data|educational resource)", "\\boer(s)?\\b",
    "reproduc", "replicat", "pre.?registration", "registered report",
    "statistic", "data (skill|science|literac)", "\\br programming\\b",
    "\\brstudio\\b", "coding", "computational", "bioinformatic",
    "research method", "methodolog", "qualitative", "quantitative", "mixed method",
    "psychometric", "scoping review", "systematic review", "narrative review",
    "meta.?analys", "scholarship of teaching", "\\bsotl\\b",
    "evaluation framework", "programme evaluation", "publication ethic",
    "supervis", "survey design"
  )
)

all_defs <- c(theme_defs, list(cross_def))

# ---- Helpers ----

#' Count how many distinct patterns match a string
n_matching <- function(patterns, x) {
  if (is.na(x) || x == "") return(0L)
  sum(map_lgl(patterns, ~ str_detect(x, regex(.x, ignore_case = TRUE))))
}

#' Score one theme against one output
score_theme <- function(patterns, title, journal, abstract) {
  s_title    <- n_matching(patterns, title)
  s_journal  <- min(n_matching(patterns, journal), 1L)
  s_abstract <- min(n_matching(patterns, abstract), abstract_cap)
  w_title * s_title + w_journal * s_journal + w_abstract * s_abstract
}

normalise_doi <- function(x) {
  x |>
    str_trim() |>
    str_to_lower() |>
    str_remove("^https?://(dx\\.)?doi\\.org/") |>
    na_if("")
}

# ---- Load ----

if (!file.exists(pub_path)) {
  stop(glue("{pub_path} not found. Run R/fetch_orcid_publications.R first."))
}

pub_data <- fromJSON(pub_path, simplifyVector = TRUE)

pubs <- as_tibble(pub_data$publications) |>
  filter(is_sotl %in% TRUE) |>
  mutate(doi_key = normalise_doi(doi))

message(glue("SoTL outputs to classify: {nrow(pubs)}"))

# Abstracts are optional; the classifier degrades to title + journal without them
empty_abstracts <- tibble(
  doi_key  = character(),
  abstract = character(),
  subjects = character()
)

abstracts <- if (file.exists(abstract_path)) {
  recs <- fromJSON(abstract_path, simplifyVector = TRUE)$records
  if (is.null(recs) || length(recs) == 0) {
    empty_abstracts
  } else {
    recs <- as_tibble(recs)
    # Tolerate an older cache that lacks the subjects column
    if (!"subjects" %in% names(recs)) recs$subjects <- NA_character_
    recs |> select(doi_key = doi, abstract, subjects)
  }
} else {
  message("  No abstract cache found — classifying on title and journal only.")
  message("  Run R/fetch_abstracts.R for materially better coverage.")
  empty_abstracts
}

pubs <- pubs |>
  # na_matches = "never": outputs without a DOI must not all join to each other
  left_join(abstracts, by = "doi_key", na_matches = "never") |>
  mutate(
    abstract = as.character(abstract),
    subjects = as.character(subjects),
    # Subject terms from OpenAlex/Crossref are treated as extra abstract text
    match_abstract = str_squish(paste(
      coalesce(abstract, ""), coalesce(subjects, "")
    )),
    match_abstract = na_if(match_abstract, ""),
    match_title    = coalesce(title, ""),
    match_journal  = coalesce(journal, "")
  )

n_with_abstract <- sum(!is.na(pubs$abstract))
message(glue("  With an abstract: {n_with_abstract} ({round(100 * n_with_abstract / nrow(pubs))}%)"))

# ---- Score ----

message("\n--- Scoring ---")

score_cols <- map(all_defs, function(d) {
  message(glue("  {d$id}"))
  pmap_dbl(
    list(pubs$match_title, pubs$match_journal, pubs$match_abstract),
    function(ti, jo, ab) score_theme(d$patterns, ti, jo, ab)
  )
})
names(score_cols) <- map_chr(all_defs, "id")
scores <- as_tibble(score_cols)

topic_ids <- map_chr(theme_defs, "id")
cross_id  <- cross_def$id

# ---- Assign ----

assign_row <- function(i) {
  row <- scores[i, topic_ids, drop = FALSE] |> unlist()
  keep <- row[row >= score_threshold]
  keep <- sort(keep, decreasing = TRUE)
  if (length(keep) > max_themes) keep <- keep[seq_len(max_themes)]
  # names() on an empty numeric vector is NULL, not character(0)
  nm <- names(keep)
  if (is.null(nm)) character(0) else nm
}

assigned <- map(seq_len(nrow(pubs)), assign_row)

cross_flag <- scores[[cross_id]] >= score_threshold

pubs <- pubs |>
  mutate(
    themes        = assigned,
    n_themes      = lengths(assigned),
    is_cross      = cross_flag,
    theme_source  = if_else(n_themes > 0, "auto", "unclassified")
  )

# ---- Apply manual overrides ----
#
# theme_overrides.csv columns:
#   doi         normalised DOI, or blank to match on title
#   title       exact title, used only when doi is blank
#   themes      semicolon-separated theme ids, or "none" to clear all
#   cross       TRUE / FALSE / blank (blank leaves the automatic value)
#   note        free text, not used by the site

if (file.exists(override_path)) {
  ov <- read.csv(override_path, stringsAsFactors = FALSE, colClasses = "character") |>
    as_tibble()

  # Tolerate a template that is present but has no rows, or missing columns
  for (col in c("doi", "title", "themes", "cross", "note")) {
    if (!col %in% names(ov)) ov[[col]] <- character(nrow(ov))
  }

  ov <- ov |>
    mutate(across(everything(), ~ na_if(str_trim(.x), ""))) |>
    filter(!is.na(themes) | !is.na(cross))

  if (nrow(ov) > 0) {
    message(glue("\n--- Applying {nrow(ov)} override(s) ---"))
    valid_ids <- c(topic_ids, cross_id)

    for (k in seq_len(nrow(ov))) {
      o   <- ov[k, ]
      idx <- if (!is.na(o$doi) && o$doi != "") {
        which(pubs$doi_key == normalise_doi(o$doi))
      } else if (!is.na(o$title) && o$title != "") {
        which(str_squish(str_to_lower(pubs$title)) == str_squish(str_to_lower(o$title)))
      } else {
        integer(0)
      }

      if (length(idx) == 0) {
        message(glue("  No match for override row {k}: {coalesce(o$doi, o$title)}"))
        next
      }

      if (!is.na(o$themes) && o$themes != "") {
        new_themes <- if (str_to_lower(str_trim(o$themes)) == "none") {
          character(0)
        } else {
          ids <- str_trim(str_split(o$themes, ";")[[1]])
          ids <- ids[ids != ""]
          bad <- setdiff(ids, valid_ids)
          if (length(bad) > 0) {
            message(glue("  Unknown theme id(s) in row {k}: {paste(bad, collapse = ', ')}"))
          }
          intersect(ids, topic_ids)
        }
        for (j in idx) pubs$themes[[j]] <- new_themes
        pubs$theme_source[idx] <- "manual"
      }

      if (!is.na(o$cross) && o$cross != "") {
        pubs$is_cross[idx] <- str_to_lower(str_trim(o$cross)) %in% c("true", "yes", "1")
        pubs$theme_source[idx] <- "manual"
      }
    }

    pubs <- pubs |> mutate(n_themes = lengths(themes))
  }
} else {
  message(glue("\nNo override file at {override_path} — skipping."))
}

# ---- Report ----

message("\n--- Theme counts ---")
counts <- map_dfr(all_defs, function(d) {
  n <- if (d$id == cross_id) {
    sum(pubs$is_cross)
  } else {
    sum(map_lgl(pubs$themes, ~ d$id %in% .x))
  }
  tibble(id = d$id, label = d$label, n = n)
})

for (i in seq_len(nrow(counts))) {
  message(glue("  {counts$n[i]}\t{counts$label[i]}"))
}

n_unclassified <- sum(pubs$n_themes == 0)
message(glue("\n  Unclassified: {n_unclassified} ({round(100 * n_unclassified / nrow(pubs))}%)"))
message(glue("  Coverage: {nrow(pubs) - n_unclassified}/{nrow(pubs)} ({round(100 * (nrow(pubs) - n_unclassified) / nrow(pubs))}%)"))

# ---- Save ----

export <- pubs |>
  mutate(themes_str = map_chr(themes, ~ paste(.x, collapse = ";"))) |>
  # Drop the list column before reusing the name for its flattened form
  select(-themes) |>
  select(
    title, year, journal, type, doi, url,
    network_authors, network_authors_school, network_schools,
    themes = themes_str, is_cross, theme_source, n_themes
  )

write_json(
  list(
    last_updated    = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
    n_outputs       = nrow(export),
    n_unclassified  = n_unclassified,
    n_with_abstract = n_with_abstract,
    score_threshold = score_threshold,
    max_themes      = max_themes,
    themes = map(theme_defs, ~ list(id = .x$id, label = .x$label, blurb = .x$blurb)),
    cross  = list(id = cross_def$id, label = cross_def$label, blurb = cross_def$blurb),
    counts = counts,
    outputs = export
  ),
  out_path,
  pretty = TRUE,
  auto_unbox = TRUE,
  na = "null"
)

message(glue("\n✓ Written to {out_path}"))
