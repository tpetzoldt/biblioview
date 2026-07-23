# biblioview

<!-- badges: start -->
[![R-CMD-check](https://github.com/tpetzoldt/biblioview/workflows/R-CMD-check/badge.svg)](https://github.com/tpetzoldt/biblioview/actions)
<!-- badges: end -->

`{biblioview}` is an R package and interactive Shiny dashboard designed to streamline, enrich, and explore literature libraries directly from Zotero Group Libraries. 
It implements a resource-polite, multi-pass pipeline to pull base metadata, resolve missing abstracts, and track live citation metrics.

## Key Features

* **Selective Collection Mapping:** Scan and select specific non-hierarchical sub-folders within your Zotero group library before pulling data down.
* **Batch Citation Metrics:** Mass-queries the OpenAlex API in unified chunks of up to 50 items at once, populating up-to-date citation counts.
* **Enrichment:** Automatically checks Crossref, OpenAlex and Europe PMC via a fallback loop to patch missing abstract text.
* **Sorting, Filtering and Export:** Retrieved libraries can be sorted, filtered and exported.

---

## Installation

Install the development version of `{biblioview}` directly from GitHub:

```R
# install.packages("remotes")
remotes::install_github("tpetzoldt/biblioview")
```

---

## Getting Started

### 1. Launching the Interactive Dashboard

You can start the bundled Shiny dashboard directly from the package environment using `launch_dashboard()`. 

The launcher function accepts optional arguments to pre-populate your credentials and dynamically customize the application window title at startup, removing the need to manage complex URL query strings manually.

```R
library(biblioview)

# Option A: Launch a clean, empty dashboard
launch_dashboard()

# Option B: Launch pre-configured for a project team
launch_dashboard(
  group = "1234567",
  key   = "your_secret_zotero_key",
  title = "Water Quality Research Portal"
)
```

> **Deployment Note:** If hosting the dashboard on a server infrastructure (such as Shiny Server or Posit Connect), credentials can be passed dynamically via standard web URL routing parameter rules instead (e.g., `https://your-shiny-server.org/biblioview/?group=1234567&key=your_secret_zotero_key&title=Water%20Quality%20Research%20Portal`).

### 2. Using Package Functions Independently

If you are building custom analysis pipelines, you can skip the UI and run the modular batch operations directly in your own R scripts:

```R
library(biblioview)

# 1. Grab metadata from a specific collection sub-folder
raw_data <- fetch_all_zotero_data(
  group_id = "numeric_group_id", 
  api_key = "your_secret_zotero_key",
  collection_id = "collection_id"
)

# 2. Pull live batch metrics from OpenAlex
final_dataset <- fetch_citation_counts(abstract_enriched, email_contact)

# 3. Patch gaps in abstract listings
abstract_enriched <- enrich_missing_abstracts(raw_data, email_contact)
```

---

## The Dashboard Processing Pipeline

The layout operates as a progressive workflow loop to protect network resources and keep UI performance snappy:

1. **Step 0 (Scan Folders):** Authenticates your credentials against the Zotero API and dynamically generates a multi-select dropdown map of your target sub-collections.
2. **Step 1 (Fetch Selected Library):** Downloads reference items and isolates row entries to drop duplicate DOIs.
3. **Step 2 (Fetch Citation Metrics):** Triggers the fast-batch OpenAlex lookup to append citation records. 
  This step leverages the optional API email input text box, defaulting to your system's `POLITE_EMAIL` variable if left blank.
4. **Step 3 (Run Abstract Enrichment):** Executes defensive, item-by-item fallback abstract search queries. 


---

## Permanent Setting of Email Address

Instead of entering your email address every time to the dashboard, consider to set a local system environment variable. 

Run the following command in your R console:

```R
usethis::edit_r_environ()
```

Add your email address profile rule to the `.Renviron` file that opens up, then restart your R session:

```env
POLITE_EMAIL="your.name@example.com"
```

---

## License

This project is licensed under the GPL (>= 2) License.
