# Sync Missing Abstracts Back to Zotero API

Scans a Zotero group library (or specific collections) for top-level
items missing abstracts, fetches them from external APIs, sanitizes
formatting, and writes them back to Zotero.

## Usage

``` r
sync_missing_abstracts_to_zotero(
  group_id,
  api_key,
  collections = NULL,
  force_overwrite = FALSE,
  email_contact = Sys.getenv("POLITE_EMAIL"),
  providers = list(epmc = fetch_abstract_epmc, crossref = fetch_abstract_crossref,
    openalex = fetch_abstract_openalex)
)
```

## Arguments

- group_id:

  Character or Numeric. The Zotero Group ID.

- api_key:

  Character. A Zotero API key with write access to the group.

- collections:

  Character vector (optional). One or more Zotero folder/collection
  names. If NULL (default), scans the entire group library.

- force_overwrite:

  Logical. If TRUE, re-fetches and overwrites pre-existing abstracts.
  Defaults to FALSE.

- email_contact:

  Character. Contact email used for polite external API pools. Defaults
  to environment variable `POLITE_EMAIL`.

- providers:

  Named list of provider functions passed to external APIs.

## Value

A named list containing summary statistics of the sync operation and
details on any unresolvable items.
