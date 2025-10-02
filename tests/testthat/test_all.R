test_that("Kolada availability check runs", {
  ok <- kolada_available()
  expect_type(ok, "logical")
})

test_that("kld_kpis returns a tibble", {
  skip_if_not(kolada_available(), "Kolada API not available")
  df <- kld_kpis(per_page = 3L)
  expect_s3_class(df, "tbl_df")
  expect_true(any(grepl("id", names(df), ignore.case = TRUE)))
})

test_that("kld_municipalities returns a tibble", {
  skip_if_not(kolada_available(), "Kolada API not available")
  df <- kld_municipalities(per_page = 5L)
  expect_s3_class(df, "tbl_df")
  expect_true(any(grepl("id", names(df), ignore.case = TRUE)))
})

test_that("kld_values works for a known KPI & municipality", {
  skip_if_not(kolada_available(), "Kolada API not available")
  # Use a KPI that exists (population: N00951) and Stockholm (0180)
  df <- kld_values(kpi = "N00951", municipality = "0180", year = 2020)
  expect_s3_class(df, "tbl_df")
  expect_true(any(grepl("kpi|municipality", names(df), ignore.case = TRUE)))
})

test_that("kld_values can handle a big query", {
  skip_if_not(kolada_available(), "Kolada API not available")

  munis <- kld_municipalities(per_page = 250L)
  expect_s3_class(munis, "tbl_df")
  id_col <- names(munis)[which(grepl("id|code", names(munis), ignore.case = TRUE))[1]]
  skip_if(is.na(id_col), "Couldn't detect municipality id column")

  m_ids <- unique(munis[[id_col]])
  m_ids <- m_ids[!is.na(m_ids)]
  skip_if(length(m_ids) < 40, "Not enough municipalities returned to stress the query")

  m_ids <- m_ids[1:40]

  # One KPI, 3 years, 40 municipalities → expect a lot of rows
  df <- kld_values(kpi = "N00951", municipality = m_ids, year = 2018:2020)

  expect_s3_class(df, "tbl_df")
  expect_gt(nrow(df), 100)
})

test_that("kld_values handles invalid input gracefully", {
  skip_if_not(kolada_available(), "Kolada API not available")
  df <- tryCatch(
    kld_values(kpi = "FAKE_ID_XXXX", municipality = "0180", year = 2020),
    error = function(e) tibble::tibble()
  )
  expect_s3_class(df, "tbl_df")
  expect_equal(nrow(df), 0)
})
