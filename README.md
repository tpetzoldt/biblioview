# biblioview

<!-- badges: start -->
[![R-CMD-check](https://github.com/tpetzoldt/biblioview/workflows/R-CMD-check/badge.svg)](https://github.com/tpetzoldt/biblioview/actions)
<!-- badges: end -->

`{biblioview}` is an R package and interactive Shiny dashboard designed to streamline, enrich, and explore literature libraries directly from Zotero Group Libraries. It implements a resource-polite, multi-pass optimization pipeline to pull base metadata, resolve missing abstracts, and track live citation metrics.

## Key Features

*   **Step 0: Selective Collection Mapping:** Scan and select specific non-hierarchical sub-folders within your Zotero group library before pulling data down.
*   **Dual-Source Abstract Enrichment:** Automatically checks Crossref and OpenAlex via a resource-efficient fallback loop to patch missing abstract text.
*   **Batch Citation Metrics:** Mass-queries the OpenAlex API in unified chunks of up to 50 items at once, populating up-to-date citation counts in less than a second.
*   **Smart Sorting Architecture:** Missing citation metrics are dynamically flagged with a negative marker (`-1`), ensuring incomplete data gracefully stacks at the bottom of your searchable database tables during high-to-low ranking queries.

---

## Installation

You can install the development version of `{biblioview}` directly from GitHub:

```R
# install.packages("devtools")
devtools::install_github("tpetzoldt/biblioview")
```

---

## Configuration & Pre-requisites

To unlock the prioritized API rate limits (the OpenAlex "Polite Pool"), add your contact email to your local system environment variables. 

Run the following command in your R console:

```R
usethis::edit_r_environ()
```

Add your email address profile rule to the `.Renviron` file that opens up, then restart your R session:

```env
POLITE_EMAIL="your.name@example.com"
```

---

## Getting Started

### 1. Launching the Interactive Dashboard

You can start the bundled Shiny dashboard directly from the package environment:

```R
library(biblioview)

# Launch the app locally
biblioview::run_app()
```

### 2. Using Package Functions Independently

If you are building custom analysis pipelines, you can skip the UI and run the modular batch operations directly in your own R scripts:

```R
library(biblioview)

# 1. Grab metadata from a specific collection sub-folder
raw_data <- fetch_all_zotero_data(
  group_id = "1234567", 
  api_key = "your_secret_zotero_key",
  collection_id = "A8F3X2Z9"
)

# 2. Patch gaps in abstract listings
abstract_enriched <- enrich_missing_abstracts(raw_data)

# 3. Pull live batch metrics from OpenAlex (respects POLITE_EMAIL env variable)
final_dataset <- fetch_citation_counts(abstract_enriched)
```

---

## The Dashboard Processing Pipeline

The layout operates as a progressive, four-step workflow loop to protect network resources and keep performance snappy:

1.  **Step 0 (Scan Folders):** Authenticates credentials and dynamically generates multi-select options map of your target sub-collections.
2.  **Step 1 (Fetch Selected Library):** Downloads references from Zotero and isolates row objects using native R pipes (`|>`) to drop duplicated DOIs.
3.  **Step 2 (Enrich Missing Abstracts):** Executes the fallback abstract search logic targeting *only* reference items missing abstracts.
4.  **Step 3 (Fetch Citation Metrics):** Triggers the fast-batch OpenAlex lookup to instantly append citation records to your interactive data table interface.

---

## License

This project is licensed under the GPL (>= 2) License.
