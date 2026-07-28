# Fetch Abstract from OpenAlex

Fetch Abstract from OpenAlex

## Usage

``` r
fetch_abstract_openalex(doi, req_template)
```

## Arguments

- doi:

  Clean DOI string.

- req_template:

  httr2 base request.

## Value

Character string containing abstract or NULL if not found.
