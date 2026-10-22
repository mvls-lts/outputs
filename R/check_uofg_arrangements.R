# check_uofg_arrangements.R
# Watches the University's Publisher Arrangements page, which has no API, and
# flags when its text changes so the routes in _data/oa_routes.csv can be
# reviewed. It does not try to interpret the change.
#
# Keeps a plain-text snapshot of the page in _data/uofg_arrangements_snapshot.txt.
# On the first run it only saves the snapshot. On later runs, if lines were
# added or removed, it appends them to oa_alerts.md, which the workflow turns
# into a GitHub issue.
#
# Run after fetch_oa_journal_data.R (which may already have started oa_alerts.md).

library(httr2)
library(xml2)
library(stringr)
library(purrr)

page_url    <- "https://www.gla.ac.uk/myglasgow/openresearch/openaccess/publisherarrangements/"
snap_path   <- "_data/uofg_arrangements_snapshot.txt"
alerts_path <- "oa_alerts.md"
min_lines   <- 30    # fewer lines than this suggests an error or holding page
max_show    <- 40    # lines listed per section in the alert

html <- tryCatch(
  request(page_url) |>
    req_user_agent("MVLS LTS Network site checker (mailto:mvls-lts@glasgow.ac.uk)") |>
    req_retry(max_tries = 3) |>
    req_timeout(60) |>
    req_perform() |>
    resp_body_string(),
  error = function(e) {
    message("Could not fetch the UofG page: ", conditionMessage(e))
    NULL
  }
)
if (is.null(html)) quit(save = "no", status = 0)

# Text of headings, paragraphs, list items and table cells, excluding site
# navigation, header and footer
lines <- read_html(html) |>
  xml_find_all(paste0(
    "//body//*[self::h1 or self::h2 or self::h3 or self::h4 or self::p or self::li or self::td]",
    "[not(ancestor::nav) and not(ancestor::header) and not(ancestor::footer)]"
  )) |>
  xml_text() |>
  str_squish() |>
  discard(\(x) x == "") |>
  unique()

if (length(lines) < min_lines) {
  message("Only ", length(lines), " lines of text found; not treating this as a real change.")
  quit(save = "no", status = 0)
}

if (!file.exists(snap_path)) {
  writeLines(lines, snap_path)
  message("Saved first snapshot (", length(lines), " lines).")
  quit(save = "no", status = 0)
}

old_lines <- readLines(snap_path, warn = FALSE)
added     <- setdiff(lines, old_lines)
removed   <- setdiff(old_lines, lines)

if (length(added) == 0 && length(removed) == 0) {
  message("No change to the UofG Publisher Arrangements page.")
  quit(save = "no", status = 0)
}

show <- function(x) {
  x <- str_trunc(x, 300)
  if (length(x) > max_show) x <- c(head(x, max_show), paste("...and", length(x) - max_show, "more"))
  paste0("> ", x)
}

section <- c(
  "## UofG Publisher Arrangements page has changed", "",
  paste0("Page: ", page_url), "",
  "Check whether any route in `_data/oa_routes.csv` (or a journal-specific route in `_data/oa_journals.csv`) needs updating.", "",
  if (length(added)) c("**New or changed text:**", "", show(added), ""),
  if (length(removed)) c("**Removed or changed text:**", "", show(removed), "")
)

existing <- if (file.exists(alerts_path)) readLines(alerts_path, warn = FALSE) else
  c("Weekly check of the Where to publish page", "")
writeLines(c(existing, "", section), alerts_path)
writeLines(lines, snap_path)
message("Page changed: ", length(added), " lines added, ", length(removed), " removed. Wrote ", alerts_path)
