# Network Publications Website

An automatically-updating Quarto website that displays publications from a research network, sourced from ORCID profiles.

## Features

- **Automatic updates**: Weekly GitHub Actions workflow fetches latest publication data from ORCID
- **Deduplication**: Collaborative publications appearing on multiple profiles are merged
- **Searchable**: Interactive tables with filtering and sorting
- **Visualisations**: Publication counts by year and type
- **Responsive**: Works on desktop and mobile

## Quick Start

### 1. Fork or Clone This Repository

```bash
git clone https://github.com/YOUR-USERNAME/network-publications.git
cd network-publications
```

### 2. Configure ORCID IDs

Edit `R/fetch_orcid_publications.R` and update the `orcid_ids` vector with your network members' ORCID iDs:

```r
orcid_ids <- c(
  "0000-0002-1234-5678",
  "0000-0003-8765-4321",
  # Add more...
)
```

### 3. Update Site Configuration

Edit `_quarto.yml`:
- Change `title` and `description`
- Update the GitHub link in the navbar

Edit `about.qmd`:
- Update the contact/issues link

### 4. Enable GitHub Pages

1. Go to your repository's **Settings** → **Pages**
2. Under **Build and deployment**, select **GitHub Actions**

### 5. Run the First Build

Either:
- Push to `main` branch (triggers automatic build)
- Go to **Actions** → **Update Publications** → **Run workflow**

## Local Development

### Prerequisites

- R (≥ 4.1.0)
- Quarto (≥ 1.3.0)

### Install R Dependencies

```r
install.packages(c(
  "httr2",
  "purrr",
  "dplyr",
  "tidyr",
  "stringr",
  "jsonlite",
  "glue",
  "DT",
  "ggplot2",
  "here"
))
```

### Fetch Data and Preview

Run the three data scripts in order, then preview:

```bash
# 1. Fetch publication data from ORCID (slow, ~1 request per author)
Rscript R/fetch_orcid_publications.R

# 2. Fetch abstracts from OpenAlex, falling back to Crossref
#    Cached in _data/abstracts.json and only topped up on later runs
Rscript R/fetch_abstracts.R

# 3. Group the SoTL outputs into themes
Rscript R/classify_themes.R

# Preview the site
quarto preview
```

Steps 2 and 3 are cheap to repeat. If you are only tuning the theme keywords,
re-run step 3 alone.

## Themes

`themes.qmd` groups the SoTL publications into eight themes. Assignment is
automatic and multi-label: a publication can carry up to three themes. The page
is a chart and a filterable table, nothing else.

**Publications only.** `publication_types` in `R/classify_themes.R` restricts the
page to journal articles, books and book chapters. Conference papers, abstracts,
posters, presentations, reports, blog posts and theses are excluded, as are
preprints — the fetch script already suppresses any preprint with a published
version, so the remainder are unpublished. Add `"preprint"` to that vector to
count them. The script prints what it excluded and why on every run.

**Scoring.** Each theme holds a list of regular expressions matched against the
title, journal and abstract. A title match scores 3, a journal match 2, and each
distinct abstract match 1 (capped at 4). A theme is assigned at a score of 3 or
more, so one title hit is enough but a single passing mention in an abstract is
not. Thresholds are the configuration block at the top of `R/classify_themes.R`.

**Abstracts matter.** On title and journal alone roughly a quarter of outputs
match nothing. Abstracts are available for the outputs that carry a DOI, which
is a substantial but partial subset. Anything unmatched is listed openly at the
foot of the themes page rather than hidden.

**Corrections.** Edit `_data/theme_overrides.csv`. It is applied after the
keyword pass and always wins, so a correction made once stays made. Match on DOI
where possible, or on exact title where there is no DOI. Set `themes` to `none`
to remove an output from every theme.

**Changing the themes themselves.** Edit `theme_defs` in
`R/classify_themes.R`. Each entry needs an `id` (used in the override file and
in `themes.json`), a `label`, a `blurb` shown on the page, and a `patterns`
vector. Re-run step 3 and check the counts it prints before rendering.

## Project Structure

```
network-publications/
├── _quarto.yml           # Quarto configuration
├── index.qmd             # Homepage
├── publications.qmd      # Full publication list
├── themes.qmd            # SoTL outputs grouped into themes
├── authors.qmd           # Network authors
├── about.qmd             # About page
├── styles.css            # Custom styling
├── R/
│   ├── fetch_orcid_publications.R  # ORCID data fetching script
│   ├── fetch_abstracts.R           # OpenAlex / Crossref abstract enrichment
│   └── classify_themes.R           # Keyword theme classifier
├── _data/                # Generated data (gitignored except JSON)
│   ├── publications.json
│   ├── publications.rds
│   ├── abstracts.json          # Abstract cache, topped up not refetched
│   ├── themes.json             # Theme assignments used by themes.qmd
│   └── theme_overrides.csv     # Manual corrections, applied last
├── docs/                 # Rendered site (gitignored)
└── .github/
    └── workflows/
        └── update-publications.yml  # Automated updates
```

## Customisation

### Change Update Frequency

Edit `.github/workflows/update-publications.yml`:

```yaml
schedule:
  # Daily at 2am UTC
  - cron: '0 2 * * *'
  
  # Weekly on Mondays at 2am UTC
  - cron: '0 2 * * 1'
```

### Add Custom Pages

1. Create a new `.qmd` file in the root directory
2. Add it to the navbar in `_quarto.yml`

### Change Styling

Edit `styles.css` or change the theme in `_quarto.yml`:

```yaml
format:
  html:
    theme: cosmo  # or any Bootswatch theme
```

## Troubleshooting

### API Rate Limiting

The ORCID public API has generous rate limits, but if you have many authors, you might hit them. The script includes 0.5-second delays between requests. If issues persist, increase the delay in `R/fetch_orcid_publications.R`.

### Missing Publications

Publications must be:
1. Added to the author's ORCID profile
2. Set to public visibility

### Build Failures

Check the GitHub Actions logs. Common issues:
- Missing R packages (update the workflow file)
- Invalid ORCID IDs (check for typos)
- Network timeouts (re-run the workflow)

## Licence

MIT

## Acknowledgements

- [ORCID](https://orcid.org) for the public API
- [Quarto](https://quarto.org) for the publishing system
- [DT](https://rstudio.github.io/DT/) for interactive tables
