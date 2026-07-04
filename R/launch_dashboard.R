#' Launch the Biblioview Dashboard Locally
#' @export
launch_dashboard <- function() {
  qmd_path <- system.file("dashboard", "index.qmd", package = "biblioview")
  if (qmd_path == "") stop("Dashboard template file not found.")

  # Renders the template right where it sits and opens it in the browser
  quarto::quarto_preview(qmd_path)
}
