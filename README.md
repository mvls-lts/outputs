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

```bash
# Fetch publication data
Rscript R/fetch_orcid_publications.R

# Preview the site
quarto preview
```

## Project Structure

```
network-publications/
├── _quarto.yml           # Quarto configuration
├── index.qmd             # Homepage
├── publications.qmd      # Full publication list
├── authors.qmd           # Network authors
├── about.qmd             # About page
├── styles.css            # Custom styling
├── R/
│   └── fetch_orcid_publications.R  # ORCID data fetching script
├── _data/                # Generated data (gitignored except JSON)
│   ├── publications.json
│   └── publications.rds
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
