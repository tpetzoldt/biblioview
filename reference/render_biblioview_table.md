# Create Standard Biblioview DataTable

Handles hyperlink formatting, JS tooltips, column truncation, and export
button configurations uniformly.

## Usage

``` r
render_biblioview_table(df, title = "export", show_buttons = TRUE)
```

## Arguments

- df:

  Data frame containing reference entries

- title:

  Character. Export filename prefix

- show_buttons:

  Logical. Include Copy/CSV/Excel export buttons
