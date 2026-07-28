#' Build Base Request Template with Polite Headers and Retries
#'
#' @param email_contact Character. Email used for polite API headers.
#' @return An httr2 request object.
#' @keywords internal
build_base_req <- function(email_contact) {
  httr2::request("https://localhost") |>
    httr2::req_user_agent(paste0("mailto:", email_contact)) |>
    httr2::req_retry(
      max_tries = 3,
      backoff = ~ 2,
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429, 500, 502, 503)
      }
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)
}
