# Internal: base URL for Kolada v3
.kld_base_url <- "https://api.kolada.se/v3"

# Internal: normalize a parsed v3 response into a tibble
# Internal: normalize a parsed v3 response into a tibble of **rows**
# Internal: normalize a parsed v3 response into a tibble of **rows**
.kld_as_df <- function(x) {
  # Common v3 shape: { count, next_url, previous_url, results = [ {...}, ... ] }
  # Other shapes sometimes: data.results / items / kpis / municipalities / values
  candidates <- list(
    x[["results"]],
    if (!is.null(x[["data"]])) x[["data"]][["results"]] else NULL,
    x[["items"]],
    x[["kpis"]],
    x[["municipalities"]],
    x[["values"]]
  )

  pick_rows <- function(cand) {
    if (is.null(cand)) return(NULL)

    # Already a data.frame?
    if (is.data.frame(cand)) {
      return(tibble::as_tibble(cand))
    }

    # List-of-rows? (each element is a list or data.frame with fields)
    if (is.list(cand) && length(cand) > 0L &&
        all(vapply(cand, function(e) is.list(e) || is.data.frame(e), logical(1)))) {
      # bind_rows handles differing fields & missing values
      df <- tryCatch(dplyr::bind_rows(cand), error = function(e) NULL)
      if (!is.null(df)) return(tibble::as_tibble(df))
    }

    # Atomic vector? Put it in a single column called "value"
    if (is.atomic(cand)) {
      return(tibble::tibble(value = cand))
    }

    NULL
  }

  for (cand in candidates) {
    df <- pick_rows(cand)
    if (!is.null(df)) return(df)
  }

  # Explicit empty results
  if (!is.null(x[["results"]]) && length(x[["results"]]) == 0L) {
    return(tibble::tibble())
  }

  # Fallback to empty tibble (avoid mixing paging fields like next_url/previous_url)
  tibble::tibble()
}



# Internal: GET with pagination + robust parsing
# path: string like "/kpi"
# query: named list of query parameters
# page_max: maximum pages to follow (for tests)
# pause: politeness delay between pages
.kld_get <- function(path, query = list(), base_url = "https://api.kolada.se/v3",
                     page_max = Inf, pause = 0.2) {
  url <- paste0(base_url, path)

  # Build request
  req <- httr2::request(url) |>
    httr2::req_user_agent("LabAPI (https://github.com/asetzhumatai/LabAPI)") |>
    httr2::req_error(is_error = ~ FALSE) |>
    httr2::req_timeout(15) |>
    httr2::req_retry(max_tries = 3)

  if (length(query)) {
    req <- httr2::req_url_query(req, !!!query)
  }

  # Perform
  resp <- req |> httr2::req_perform()

  # Error handling
  if (httr2::resp_status(resp) >= 400) {
    rlang::abort(
      paste("Kolada request failed [", httr2::resp_status(resp), "]: ", path),
      class = "kolada_http_error"
    )
  }

  # Parse JSON → tibble
  payload <- httr2::resp_body_json(resp)
  .kld_as_df(payload)
}


