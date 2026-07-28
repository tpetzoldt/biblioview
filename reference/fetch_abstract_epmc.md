# Fetch Abstract from Europe PMC API

Queries Europe PMC for an article's abstract using its DOI. Requires
`resultType = "core"` in the query parameters to ensure abstract text is
included.

## Usage

``` r
fetch_abstract_epmc(encoded_doi, base_req)
```

## Arguments

- encoded_doi:

  Character string. Clean or URL-encoded DOI.

- base_req:

  An httr2 request object configured with user-agent/headers.

## Value

Character string containing abstract text, or `NULL` if not found.
