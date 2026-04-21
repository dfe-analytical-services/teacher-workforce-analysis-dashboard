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

# Write a small parquet file for testing without touching real data

write_test_parquet <- function(df) {
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(df, tmp)
  tmp
}

# Expect an error about missing columns

expect_missing_cols_error <- function(expr) {
  expect_error(
    expr,
    regexp = "Missing required columns",
    class = "error"
  )
}

# 1 - Tests for read_pupil_teacher_numbers() ------------------------------------------------------------------------

# Does the function return a dataframe with the correct number of rows,
# 'start year' is created and values are numeric and rounded?

test_that("read_pupil_teacher_numbers() cleans, rounds, and creates start_year", {
  # Dummy input parquet
  input <- tibble::tibble(
    academic_year = c("2020/21", "2021/22"),
    pupil_numbers = c(1234.8, 5678.2),
    teacher_numbers = c(400.4, 999.6),
    projection = c("Yes", "Yes"),
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

# Does the function throw an error when required columns are missing?

test_that("read_pupil_teacher_numbers throws error when required columns missing", {
  # Minimal dummy df WITHOUT teacher_numbers
  bad_df <- tibble::tibble(
    academic_year = "2020/21",
    pupil_numbers = 100,
    projection = FALSE,
    phase = "Primary"
  )

  # Write temp parquet file
  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(bad_df, tmp)

  expect_missing_cols_error(
    read_pupil_teacher_numbers(tmp)
  )
})


# 2. Tests for read_pgitt_need_timeseries() -----------------------------------------------------------------------

# Does the function return a dataframe, have the column names expected, extract start year,
# call subject = total for primary otherwise keep original subject values?

test_that("read_pgitt_need_timeseries() renames and derives subject correctly", {
  input <- tibble::tibble(
    time_period = c("2019/20", "2020/21", "2020/21"),
    subject_filter_group = c("Primary", "Secondary", "Secondary"),
    subject = c("English", "Maths", "Biology"),
    pgitt_trainee_need_count = c("6", "6", "6"),
    difference_to_previous_year_percent = c("6", "6", "6")
  )

  file <- write_test_parquet(input)

  out <- read_pgitt_need_timeseries(file)

  # Structure
  expect_s3_class(out, "data.frame") # check it returns a dataframe
  expect_true(all(c("phase", "subject") %in% names(out))) # check certain columns are present

  # Start year extracted from first 4 chars
  expect_equal(out$start_year, c(2019, 2020, 2020))

  # Primary → subject forced to "Total"
  expect_equal(out$subject[1], "Total")

  # Secondary → keep original
  expect_equal(out$subject[2], "Maths")
  expect_equal(out$subject[3], "Biology")
})

# Does the function throw an error when required columns are missing?

test_that("read_pgitt_need_timeseries throws error when required columns missing", {
  bad_df <- tibble::tibble(
    time_period = "2020/21",
    subject = "English",
    pgitt_trainee_need_count = 200,
    difference_to_previous_year_percent = 0.1
    # education_phase missing
  )

  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(bad_df, tmp)

  expect_missing_cols_error(
    read_pgitt_need_timeseries(tmp)
  )
})

# 3. Tests for read_drivers_data() --------------------------------------------------------------------------------

# Does the function have clean column names and round values to 1dp?

test_that("read_drivers_data() cleans names and returns all rows intact", {
  input <- tibble::tibble(
    "Driver" = c("Entrants", "Leavers"),
    "Value" = c(100.123, 200.876),
    "Phase" = c("Secondary", "Secondary"),
    "Subject" = c("Biology", "Biology")
  )

  file <- write_test_parquet(input)

  out <- read_drivers_data(file)

  expect_s3_class(out, "data.frame") # check it returns a df
  expect_true(all(c("driver", "value", "phase", "subject") %in% names(out))) # check columns are there
  expect_equal(out$value, c(100.1, 200.9)) # check values are rounded to 1dp
})

# Does the function throw an error when a column is missing?

test_that("read_drivers_data throws error when required columns missing", {
  bad_df <- tibble::tibble(
    value = 3.5,
    phase = "Secondary",
    subject = "Maths"
    # driver missing
  )

  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(bad_df, tmp)

  expect_missing_cols_error(
    read_drivers_data(tmp)
  )
})

# =============================================================================
# 4) Tests for read_flows_data()
# =============================================================================

# Does the function return a df and get units, years and version correctly?

test_that("read_flows_data() derives units, year, and version correctly", {
  input <- tibble::tibble(
    Year = c("2024/25", "2025/26"),
    Type = c("Total leaver rate", "Newly qualified entrants"),
    Phase = c("Secondary", "Secondary"),
    Subject = c("Biology", "Biology"),
    Value = c(0.006, 6),
    Historic_or_trajectory = c("Trajectory", "Trajectory"),
    Publication_year = c("2026", "2026")
  )

  file <- write_test_parquet(input)

  out <- read_flows_data(file)

  # Structure
  expect_s3_class(out, "data.frame")

  # Unit assignment logic
  expect_equal(out$unit, c("%", "FTE"))

  # Extract numeric year
  expect_equal(out$start_year, c(2024, 2025))

  # Version column injected
  expect_true("version" %in% names(out))
  expect_equal(out$version, c("Last year", "Last year"))
})

# Does the function throw an error when a column is missing?

test_that("read_flows_data throws error when required columns missing", {
  bad_df <- tibble::tibble(
    Phase = "Secondary",
    Subject = "Physics",
    Type = "Total leaver rate",
    Year = "2020/21",
    Value = 0.1,
    Unit = "%",
    Publication_year = 2025
    # historic_or_trajectory missing
  )

  tmp <- tempfile(fileext = ".parquet")
  arrow::write_parquet(bad_df, tmp)

  expect_missing_cols_error(
    read_flows_data(tmp)
  )
})
