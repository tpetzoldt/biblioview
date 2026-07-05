#' Launch the Biblioview Dashboard Locally
#'
#' @param group Optional character string containing a default Zotero Group ID.
#' @param key Optional character string containing a default Zotero API Key.
#' @param title Optional character string to override the default portal header title.
#'
#' @importFrom shiny shinyAppDir
#' @export
launch_dashboard <- function(group = NULL, key = NULL, title = NULL) {
  app_path <- system.file("dashboard", package = "biblioview")

  if (app_path == "") {
    stop("Dashboard application directory not found. Please reinstall the package.", call. = FALSE)
  }

  # Set R session options if parameters are explicitly provided in the console call
  if (!is.null(group)) options(biblioview.group = group)
  if (!is.null(key))   options(biblioview.key   = key)
  if (!is.null(title)) options(biblioview.title = title)

  # Launch the Shiny directory
  shiny::shinyAppDir(appDir = app_path)
}
