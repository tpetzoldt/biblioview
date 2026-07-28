# Fetch Abstract from Europe PMC

Fetch Abstract from Europe PMC

## Usage

``` r
fetch_abstract_epmc(doi, req_template)
```

## Arguments

- doi:

  Clean DOI string.

- req_template:

  httr2 base request.

## Value

Character string containing abstract or NULL if not found.
