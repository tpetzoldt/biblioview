# Fetch Abstract from Crossref

Fetch Abstract from Crossref

## Usage

``` r
fetch_abstract_crossref(doi, req_template)
```

## Arguments

- doi:

  Clean DOI string.

- req_template:

  httr2 base request.

## Value

Character string containing abstract or NULL if not found.
