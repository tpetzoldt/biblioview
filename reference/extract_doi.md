# Extract and Normalize DOI from Zotero Item Data

Inspects a Zotero item's metadata list for a DOI, checking the primary
`DOI` field first, followed by the `extra` field if necessary.
Automatically normalizes URL prefixes and validates the '10.' prefix
structure.

## Usage

``` r
extract_doi(data)
```

## Arguments

- data:

  List. The `data` element of a Zotero API item list (e.g.,
  `item$data`).

## Value

Clean character string containing the DOI, or `NULL` if no valid DOI is
found.
