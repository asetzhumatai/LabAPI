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

#' Retrieve KPI metadata
#'
#' Queries the Kolada v3 API for KPI metadata. Can filter by title and control pagination.
#'
#' @param title Optional character, search string for KPI titles.
#' @param per_page Integer, number of records per page (default 100).
#' @param page Integer, page number to fetch (default 1).
#'
#' @return A tibble with KPI metadata (id, title, description, etc).
#' @export
#'
#' @examples
#' \dontrun{
#' kld_kpis(per_page = 5)
#' kld_kpis(title = "Population")
#' }
kld_kpis <- function(title = NULL, per_page = 100L, page = 1L) {
  q <- list()
  if (!is.null(title)) q$title <- title
  q$per_page <- per_page
  q$page <- page
  .kld_get("/kpi", query = q, base_url = "https://api.kolada.se/v3")
}

#' Retrieve municipality metadata
#'
#' Queries the Kolada v3 API for municipalities and regions. Can filter by title or region type.
#'
#' @param title Optional character, search string for municipality/region titles.
#' @param region_type Character, one of "municipality" or "region" (default = "municipality").
#' @param per_page Integer, number of records per page (default 290).
#' @param page Integer, page number to fetch (default 1).
#'
#' @return A tibble with municipality metadata (id, title, type, etc).
#' @export
#'
#' @examples
#' \dontrun{
#' kld_municipalities(per_page = 5)
#' }
kld_municipalities <- function(title = NULL, region_type = "municipality",
                               per_page = 290L, page = 1L) {
  q <- list()
  if (!is.null(title)) q$title <- title
  if (!is.null(region_type)) q$region_type <- region_type
  q$per_page <- per_page
  q$page <- page
  .kld_get("/municipality", query = q, base_url = "https://api.kolada.se/v3")
}

#' Retrieve KPI values
#'
#' Queries the Kolada v2 API for values for a given KPI, municipality (or multiple), and year(s).
#'
#' @param kpi Character, KPI id (e.g. "N00951").
#' @param municipality Character vector of municipality ids (e.g. "0180" for Stockholm).
#' @param year Integer or integer vector of years to fetch.
#'
#' @return A tibble with KPI values (includes kpi id, municipality id, year, and value).
#' @export
#'
#' @examples
#' \dontrun{
#' kld_values("N00951", "0180", 2020)
#' kld_values("N00951", c("0180","0181"), 2018:2020)
#' }
kld_values <- function(kpi, municipality, year) {
  stopifnot(!is.null(kpi), !is.null(municipality), !is.null(year))

  combos <- expand.grid(year = year, municipality = municipality, stringsAsFactors = FALSE)

  dfs <- apply(combos, 1, function(row) {
    path <- paste0("/data/kpi/", kpi, "/municipality/", row[["municipality"]], "/year/", row[["year"]])
    .kld_get(path, base_url = "https://api.kolada.se/v2")
  })

  dplyr::bind_rows(dfs)
}






