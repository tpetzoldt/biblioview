# Check Abstract Availability Across Providers for a Single DOI

Debugging utility to query external APIs directly for a specific DOI and
print abstract availability and character length.

## Usage

``` r
check_doi_abstracts(doi, email_contact = Sys.getenv("POLITE_EMAIL"))
```

## Arguments

- doi:

  Character string. Raw or clean DOI (e.g.,
  "10.1007/s10452-015-9564-x").

- email_contact:

  Character. Contact email for API headers.

## Value

A named list of retrieved abstracts (or NULL per provider).

## Examples

``` r
# 1. PLOS ONE: "phyloseq: An R Package for Reproducible Interactive Analysis..."
check_doi_abstracts("10.1371/journal.pone.0061217")
#> 
#> === DIRECT API CHECK FOR DOI: 10.1371/journal.pone.0061217 ===
#>   epmc             : ✓ FOUND (1889 characters)
#>   crossref         : ✗ Not available / NULL
#>   openalex         : ✓ FOUND (1870 characters)
#>   semanticscholar  : ✗ Not available / NULL
#> ====================================================
#> 

# 2. Journal of Statistical Software: "Data Validation Infrastructure for R" (validate package)
check_doi_abstracts("10.18637/jss.v097.i10")
#> 
#> === DIRECT API CHECK FOR DOI: 10.18637/jss.v097.i10 ===
#>   epmc             : ✗ Not available / NULL
#>   crossref         : ✗ Not available / NULL
#>   openalex         : ✓ FOUND (1074 characters)
#>   semanticscholar  : ✗ Not available / NULL
#> ====================================================
#> 

# 3. PLOS ONE: "Dose-Response Analysis Using R" (drc package)
check_doi_abstracts("10.1371/journal.pone.0146021")
#> 
#> === DIRECT API CHECK FOR DOI: 10.1371/journal.pone.0146021 ===
#>   epmc             : ✓ FOUND (912 characters)
#>   crossref         : ✗ Not available / NULL
#>   openalex         : ✓ FOUND (912 characters)
#>   semanticscholar  : ✗ Not available / NULL
#> ====================================================
#> 
```
