# Immutable, logged raw downloads.
# A file already on disk is never re-requested or overwritten.

ua_string <- function() "draftpool-research/0.1 (public-data pipeline; R httr2)"

request_log_path <- function() file.path("data", "raw", "request_log.csv")

log_request <- function(url, dest, status, bytes, sha256, note = "") {
  row <- data.frame(
    ts_utc = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    url = url, dest = dest, status = status, bytes = bytes,
    sha256 = sha256, note = note, stringsAsFactors = FALSE
  )
  p <- request_log_path()
  dir.create(dirname(p), recursive = TRUE, showWarnings = FALSE)
  utils::write.table(row, p, sep = ",", row.names = FALSE,
                     col.names = !file.exists(p), append = file.exists(p),
                     qmethod = "double")
}

# Download url -> dest once. min_interval: seconds between live requests to the same host.
.last_hit <- new.env()
polite_get <- function(url, dest, min_interval = 1, max_tries = 4) {
  if (file.exists(dest) && file.size(dest) > 0) return(invisible(dest))
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  host <- httr2::url_parse(url)$hostname
  last <- .last_hit[[host]]
  if (!is.null(last)) {
    wait <- min_interval - as.numeric(difftime(Sys.time(), last, units = "secs"))
    if (wait > 0) Sys.sleep(wait)
  }
  resp <- httr2::request(url) |>
    httr2::req_user_agent(ua_string()) |>
    httr2::req_retry(max_tries = max_tries, backoff = function(i) 5 * 2^i,
                     is_transient = function(r) httr2::resp_status(r) %in% c(429, 500, 502, 503, 504)) |>
    httr2::req_error(is_error = function(r) FALSE) |>
    httr2::req_perform()
  .last_hit[[host]] <- Sys.time()
  st <- httr2::resp_status(resp)
  body <- httr2::resp_body_raw(resp)
  if (st == 200) {
    tmp <- paste0(dest, ".part")
    writeBin(body, tmp)
    file.rename(tmp, dest)
    log_request(url, dest, st, length(body), digest::digest(body, algo = "sha256", serialize = FALSE))
  } else {
    log_request(url, dest, st, length(body), NA_character_, "non-200; not saved")
    warning(sprintf("HTTP %s for %s", st, url))
  }
  invisible(dest)
}
