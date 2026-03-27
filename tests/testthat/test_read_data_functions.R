# =============================================================================
# Unit tests for data-loading / preprocessing functions (read_data.R)
#
# These tests ensure that:
#   • parquet files are read correctly
#   • cleaned column names are applied
#   • computed columns (start_year, subject, unit, etc.) are correct
#   • renaming logic behaves as expected
#   • behaviour is stable if upstream parquet schemas change
# =============================================================================

# -----------------------------------------------------------------------------
# Helper: write a small parquet file for testing without touching real data
# -----------------------------------------------------------------------------
write_test_parquet <- function(df) {
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, tmp)
  tmp
}

# =============================================================================
# 1) Tests for read_pupil_teacher_numbers()
# =============================================================================

test_that("read_pupil_teacher_numbers() cleans, rounds, and creates start_year", {
  # Dummy input parquet
  input <- tibble::tibble(
    academic_year = c("2020/21", "2021/22"),
    pupil_numbers = c(1234.8, 5678.2),
    teacher_numbers = c(400.4, 999.6),
    historic = c("Yes", "Yes"),
    phase = "Primary"
  )

  file <- write_test_parquet(input)

  # Run function
  out <- read_pupil_teacher_numbers(file)

  # ---- Structure checks ----
  expect_s3_class(out, "data.frame") # check it returns a tibble/data frame
  expect_equal(nrow(out), 2) # check the number of rows are identical to input

  # ---- Column creation ----
  expect_true("start_year" %in% names(out)) # check it creates expected columns

  # Check computed values are numeric and rounded
  expect_equal(out$start_year, c(2020, 2021))
  expect_equal(out$pupil_numbers, c(1235, 5678))
  expect_equal(out$teacher_numbers, c(400, 1000))
})

# =============================================================================
# 2) Tests for read_pgitt_need_timeseries()
# =============================================================================

test_that("read_pgitt_need_timeseries() renames and derives subject correctly", {
  input <- tibble::tibble(
    time_period = c("2019/20", "2020/21", "2020/21"),
    subject_filter_group = c("Primary", "Secondary", "Secondary"),
    subject = c("English", "Maths", "Biology")
  )

  file <- write_test_parquet(input)

  out <- read_pgitt_need_timeseries(file)

  # Structure
  expect_s3_class(out, "data.frame")
  expect_true(all(c("phase", "subject") %in% names(out)))

  # Start year extracted from first 4 chars
  expect_equal(out$start_year, c(2019, 2020, 2020))

  # Primary → subject forced to "Total"
  expect_equal(out$subject[1], "Total")

  # Secondary → keep original
  expect_equal(out$subject[2], "Maths")
  expect_equal(out$subject[3], "Biology")
})

# =============================================================================
# 3) Tests for read_drivers_data()
# =============================================================================

test_that("read_drivers_data() cleans names and returns all rows intact", {
  input <- tibble::tibble(
    "Driver Name" = c("Demand", "Leavers"),
    "Value Number" = c(100, 200)
  )

  file <- write_test_parquet(input)

  out <- read_drivers_data(file)

  expect_s3_class(out, "data.frame")
  expect_true(all(c("driver_name", "value_number") %in% names(out)))
  expect_equal(out$value_number, c(100, 200))
})

# =============================================================================
# 4) Tests for read_flows_data()
# =============================================================================

test_that("read_flows_data() derives units, year, and version correctly", {
  input <- tibble::tibble(
    year = c("2024/25", "2025/26"),
    type = c("total leaver rate", "entrant count")
  )

  file <- write_test_parquet(input)

  out <- read_flows_data(file)

  # Structure
  expect_s3_class(out, "data.frame")

  # Unit assignment logic
  expect_equal(out$unit, c("%", "FTE"))

  # Extract numeric year
  expect_equal(out$year, c(2024, 2025))

  # Version column injected
  expect_true("version" %in% names(out))
  expect_equal(out$version, c("Last year", "Last year"))
})
