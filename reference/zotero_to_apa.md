# Convert Zotero Item Data to Simplified Plain-Text APA 7 Format

Parses Zotero JSON items across articles, books, chapters, reports, and
webpages, enforcing APA 7th rules without rich text (no italics or
bolding).

## Usage

``` r
zotero_to_apa(item)
```

## Arguments

- item:

  List. A single raw item element from the Zotero JSON API.

## Value

A single-row data frame with standardized bibliographic columns.
