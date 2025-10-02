#' Is Kolada v3 available?
#' Returns TRUE/FALSE without throwing.
kolada_available <- function() {
  tryCatch({
    resp <- httr2::request("https://api.kolada.se/v3/kpi") |>
      httr2::req_url_query(limit = 1) |>
      httr2::req_user_agent("LabAPI (https://github.com/asetzhumatai/LabAPI)") |>
      httr2::req_timeout(5) |>
      httr2::req_retry(max_tries = 2) |>
      httr2::req_perform()
    status <- httr2::resp_status(resp)
    status >= 200 && status < 300
  }, error = function(e) FALSE)
}


kld_kpis <- function(title = NULL, per_page = 100L, page = 1L) {
  q <- list()
  if (!is.null(title)) q$title <- title
  q$per_page <- per_page
  q$page <- page
  .kld_get("/kpi", query = q, base_url = "https://api.kolada.se/v3")
}

# Municipality metadata
kld_municipalities <- function(title = NULL, region_type = "municipality",
                               per_page = 290L, page = 1L) {
  q <- list()
  if (!is.null(title)) q$title <- title
  if (!is.null(region_type)) q$region_type <- region_type
  q$per_page <- per_page
  q$page <- page
  .kld_get("/municipality", query = q, base_url = "https://api.kolada.se/v3")
}

# Values (v2 only, because v3 doesn’t expose time series yet)
kld_values <- function(kpi, municipality, year) {
  stopifnot(!is.null(kpi), !is.null(municipality), !is.null(year))

  combos <- expand.grid(year = year, municipality = municipality, stringsAsFactors = FALSE)

  dfs <- apply(combos, 1, function(row) {
    path <- paste0("/data/kpi/", kpi, "/municipality/", row[["municipality"]], "/year/", row[["year"]])
    .kld_get(path, base_url = "https://api.kolada.se/v2")
  })

  dplyr::bind_rows(dfs)
}






