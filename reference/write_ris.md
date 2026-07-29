# Export RIS Entries to File

Writes a character vector of RIS records to a file.

## Usage

``` r
write_ris(ris_data, file)
```

## Arguments

- ris_data:

  Character vector of RIS records (e.g. from `fetch_ris_from_crossref`).

- file:

  Path to the destination file.

## Value

The file path invisibly.

## See also

[`fetch_ris_from_crossref()`](https://tpetzoldt.github.io/biblioview/reference/fetch_ris_from_crossref.md)
