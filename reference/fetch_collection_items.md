# Fetch All Items Within a Specific Zotero Collection

Handles paginated API requests (100 items per page) to retrieve all
top-level items from a specific Zotero collection key.

## Usage

``` r
fetch_collection_items(group_id, api_key, collection_key)
```

## Arguments

- group_id:

  Character or numeric. The target Zotero Group ID.

- api_key:

  Character. Your secret Zotero Web API v3 key.

- collection_key:

  Character. The unique alphanumeric key for the sub-collection.

## Value

A data frame of processed items belonging to the collection.
