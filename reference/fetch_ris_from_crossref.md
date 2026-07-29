# Fetch Bibliography Metadata in RIS Format from Crossref

Takes a vector of DOIs and retrieves their bibliographic data formatted
as RIS using Crossref Content Negotiation. Includes robust error
handling and automatic retry logic for network rate-limiting.

## Usage

``` r
fetch_ris_from_crossref(dois, email = NULL)
```

## Arguments

- dois:

  Character vector of DOIs.

- email:

  Optional email address for Crossref's polite pool (recommended).

## Value

A character vector of RIS formatted entries.

## See also

[`write_ris()`](https://tpetzoldt.github.io/biblioview/reference/write_ris.md)

## Examples

``` r
if (FALSE) { # \dontrun{
ris <- fetch_ris_from_crossref(
  dois = c("10.1016/j.ecolmodel.2020.109282", "10.1111/gcb.15000"),
  email = "user@example.com"
)
write_ris(ris, "export.ris")
} # }
```
