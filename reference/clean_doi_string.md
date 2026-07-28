# Clean and Normalize DOI Strings

Strips common URL prefixes (including bare 'doi.org/'), leading/trailing
whitespace, and verifies that the resulting string follows standard DOI
format.

## Usage

``` r
clean_doi_string(raw_doi)
```

## Arguments

- raw_doi:

  Character string containing a raw DOI or DOI URL.

## Value

Character string containing the clean DOI (e.g., "10.1579/..."), or
`NULL` if input is invalid or missing the '10.' prefix.
