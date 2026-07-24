# Fetch Sub-Folders (Collections) from a Zotero Group

Connects to the Zotero API and retrieves a named vector of all
sub-folders (collections) within the specified group library.

## Usage

``` r
fetch_zotero_collections(group_id, api_key)
```

## Arguments

- group_id:

  Character or numeric string containing the Zotero Group ID.

- api_key:

  Character string containing the Zotero API access key.

## Value

A named character vector where the values are the unique Zotero
collection hashes (e.g., `"A8F3X2Z9"`) and the names are the
human-readable folder titles (e.g., `"Marine Biology"`). Returns an
empty vector if no folders exist.

## Examples

``` r
if (FALSE) { # \dontrun{
folders <- fetch_zotero_collections("1234567", "your_api_key")
} # }
```
