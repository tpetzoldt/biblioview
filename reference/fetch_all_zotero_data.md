# Fetch All Data from a Zotero Group Library

Loops through specified or all collections in a Zotero group library,
retrieves their items, formats them into a tabular structure, and
appends the collection name.

## Usage

``` r
fetch_all_zotero_data(group_id, api_key, collection_id = NULL)
```

## Arguments

- group_id:

  Character or numeric. The target Zotero Group ID.

- api_key:

  Character. Your secret Zotero Web API v3 key.

- collection_id:

  Optional character vector. Unique Zotero collection keys (hashes) to
  pull down. If `NULL` (default), all collections in the library are
  processed.

## Value

A data frame containing structured reference data with columns:
Sub_Collection, Authors, Year, Title, DOI, APA_Citation, and Abstract.
