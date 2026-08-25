# =============================================================================
# Unit tests for data-loading / preprocessing functions (read_data.R)
#
# These tests ensure that:
#   • parquet files are read correctly
#   • column names are standardised using clean_names()
#   • required columns are present
#   • academic_year values conform to yyyy/yy format (where applicable)
#   • computed columns (start_year, subject, unit, etc.) are correct
#   • data-quality checks and warnings are triggered as expected
#   • behaviour is stable if upstream parquet schemas change
# =============================================================================

# Helper function used by multiple tests.
# Writes a temporary parquet file so test data can be read via the
# production read_*() functions without touching real data files.

write_test_parquet <- function(df) {
  tmp <- tempfile(fileext = ".parquet")
  write_parquet(df, tmp)
  tmp
}


# Helper assertion used by multiple tests.
# Checks that functions fail with the expected
# "Missing required columns" error message.

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

test_that("read_pupil_teacher_numbers() cleans names, rounds .5s up to 0 dp, and derives start_year", {
  # Dummy input parquet
  input <- tibble::tibble(
    `Academic year` = c("2020/21", "2021/22"),
    `Pupil numbers` = c(1234.8, 5678.5),
    `Teacher numbers` = c(400.5, 999.2),
    Projection = c("Yes", "Yes"),
    Phase = "Primary"
  )

  file <- write_test_parquet(input)

  # Run function
  out <- read_pupil_teacher_numbers(file)

  # Structure checks
  expect_s3_class(out, "data.frame") # check it returns a tibble/data frame
  expect_equal(nrow(out), 2) # check the number of rows are identical to input

  # Column name cleaning checks
  expect_true(
    all(
      c(
        "academic_year",
        "pupil_numbers",
        "teacher_numbers",
        "projection",
        "phase"
      ) %in%
        names(out)
    )
  )

  # Start year column check
  expect_true("start_year" %in% names(out)) # check it creates expected columns
  expect_equal(out$start_year, c(2020L, 2021L)) # check correct

  # Rounding checks
  # 1234.8 -> 1235
  # 5678.5 -> 5679 (.5 rounds up)
  expect_equal(out$pupil_numbers, c(1235, 5679))

  # Rounding checks
  # 400.5 -> 401 (.5 rounds up)
  # 999.2 -> 999
  expect_equal(out$teacher_numbers, c(401, 999))
})

# Does the function throw an error when required columns are missing?

test_that("read_pupil_teacher_numbers throws error when required columns missing", {
  # Minimal dummy df WITHOUT teacher_numbers
  bad_df <- tibble::tibble(
    academic_year = "2020/21",
    pupil_numbers = 100,
    projection = "No",
    phase = "Primary"
  )

  # Write temp parquet file
  tmp <- write_test_parquet(bad_df)

  expect_missing_cols_error(
    read_pupil_teacher_numbers(tmp)
  )
})

# Does the function warn when academic year is not in yyyy/yy format?

test_that("read_pupil_teacher_numbers warns for invalid academic_year format", {
  bad_df <- tibble::tibble(
    academic_year = c("2024-25", "202425"),
    pupil_numbers = c(100, 200),
    teacher_numbers = c(10, 20),
    projection = "Yes",
    phase = "Primary"
  )

  file <- write_test_parquet(bad_df)

  expect_warning(
    read_pupil_teacher_numbers(file),
    regexp = "Invalid academic_year format"
  )
})

# Does the function warn when values are missing?

test_that("read_pupil_teacher_numbers warns when values are missing", {
  input <- tibble::tibble(
    academic_year = "2024/25",
    pupil_numbers = NA_real_,
    teacher_numbers = 100,
    projection = "Yes",
    phase = "Primary"
  )

  file <- write_test_parquet(input)

  expect_warning(
    read_pupil_teacher_numbers(file),
    regexp = "Missing values detected"
  )
})

# Does the function warn when values are negative?

test_that("read_pupil_teacher_numbers warns when values are negative", {
  input <- tibble::tibble(
    academic_year = "2024/25",
    pupil_numbers = -1,
    teacher_numbers = 100,
    projection = "Yes",
    phase = "Primary"
  )

  file <- write_test_parquet(input)

  expect_warning(
    read_pupil_teacher_numbers(file),
    regexp = "Negative values detected"
  )
})


# 2 - Tests for read_pgitt_need_timeseries() -----------------------------------------------------------------------

# Does the function return a dataframe, have the column names expected, extract start year,
# call subject = total for primary otherwise keep original subject values?

test_that("read_pgitt_need_timeseries() cleans names, renames phase, rounds .5s up to the right dp, and derives start_year", {
  input <- tibble::tibble(
    `Time period` = c("201920", "202021", "202021"),
    `Education phase` = c("Primary", "Secondary", "Secondary"),
    Subject = c("English", "Maths", "Biology"),
    `PGITT trainee need count` = c(100.5, 200.2, 300.9),
    `Difference to previous year count` = c(51.5, 61.1, 71.7),
    `Difference to previous year percent` = c(10.05, 10.15, 10.25)
  )

  file <- write_test_parquet(input)

  out <- read_pgitt_need_timeseries(file)

  # Structure
  expect_s3_class(out, "data.frame") # check it returns a dataframe
  expect_equal(nrow(out), 3) # check the data has the correct number of rows

  # Column cleaning & renaming
  expect_true(
    all(
      c(
        "subject",
        "phase",
        "pgitt_trainee_need_count",
        "difference_to_previous_year_count",
        "difference_to_previous_year_percent"
      ) %in%
        names(out)
    )
  )

  # Created column checks
  expect_true("start_year" %in% names(out))
  expect_true("academic_year" %in% names(out))

  # start_year derived from first 4 characters
  expect_equal(out$start_year, c(2019L, 2020L, 2020L))

  # Rounding checks
  # 100.5 -> 101 (.5 rounds up)
  # 200.2 -> 200
  # 300.9 -> 301
  expect_equal(
    out$pgitt_trainee_need_count,
    c(101, 200, 301)
  )

  # 51.5 -> 52 (.5 rounds up)
  # 61.1 -> 61
  # 71.7 -> 72
  expect_equal(
    out$difference_to_previous_year_count,
    c(52, 61, 72)
  )

  # 10.05 -> 10.1
  # 10.15 -> 10.2
  # 10.25 -> 10.3
  expect_equal(
    out$difference_to_previous_year_percent,
    c(10.1, 10.2, 10.3)
  )

  # academic_year formatting checks
  expect_equal(
    out$academic_year,
    c("2019/20", "2020/21", "2020/21")
  )

  # Subject values preserved
  expect_equal(out$subject, c("English", "Maths", "Biology"))

  # Phase correctly renamed
  expect_equal(out$phase, c("Primary", "Secondary", "Secondary"))
})

# Does the function throw an error when required columns are missing?

test_that("read_pgitt_need_timeseries throws error when required columns missing", {
  bad_df <- tibble::tibble(
    # education_phase and difference_to_previous_year_count missing
    time_period = "202021",
    subject = "English",
    pgitt_trainee_need_count = 200,
    difference_to_previous_year_percent = 0.1
  )

  tmp <- write_test_parquet(bad_df)

  expect_missing_cols_error(
    read_pgitt_need_timeseries(tmp)
  )
})

# Does the function warn when values are missing?

test_that("read_pgitt_need_timeseries warns when trainee need count contains missing values", {
  input <- tibble::tibble(
    time_period = "202425",
    subject = "Maths",
    education_phase = "Secondary",
    pgitt_trainee_need_count = NA_real_,
    difference_to_previous_year_count = 1,
    difference_to_previous_year_percent = 0.1
  )

  file <- write_test_parquet(input)

  expect_warning(
    read_pgitt_need_timeseries(file),
    regexp = "Missing values detected"
  )
})

# Does the function warn when values are negative?

test_that("read_pgitt_need_timeseries warns when trainee need count is negative", {
  input <- tibble::tibble(
    time_period = "202425",
    subject = "Maths",
    education_phase = "Secondary",
    pgitt_trainee_need_count = -1,
    difference_to_previous_year_count = 1,
    difference_to_previous_year_percent = 0.1
  )

  file <- write_test_parquet(input)

  expect_warning(
    read_pgitt_need_timeseries(file),
    regexp = "Negative values detected"
  )
})

# 3 - Tests for read_drivers_data() --------------------------------------------------------------------------------

# Does the function have clean column names and round values to 1dp?

test_that("read_drivers_data() cleans names, preserves rows and rounds .5s to 1 decimal place", {
  input <- tibble::tibble(
    "Driver" = c("Entrants", "Leavers"),
    "Value" = c(100.05, 200.25),
    "Phase" = c("Secondary", "Secondary"),
    "Subject" = c("Biology", "Biology")
  )

  file <- write_test_parquet(input)

  # Run function
  out <- read_drivers_data(file)

  # Structure checks
  expect_s3_class(out, "data.frame") # check it returns a df
  expect_equal(nrow(out), 2)

  # Column name cleaning

  expect_true(
    all(
      c("driver", "value", "phase", "subject") %in% names(out)
    )
  )

  # Rounding checks
  # 100.05 -> 100.1
  # 200.25 -> 200.3 (.05 rounds up at 1 dp)
  expect_equal(out$value, c(100.1, 200.3))
  expect_true(is.numeric(out$value))

  # Other columns preserved
  expect_equal(out$driver, c("Entrants", "Leavers"))
  expect_equal(out$phase, c("Secondary", "Secondary"))
  expect_equal(out$subject, c("Biology", "Biology"))
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


# Does the function warn when values are missing?

test_that("read_drivers_data warns when value contains missing values", {
  input <- tibble(
    driver = "Entrants",
    value = NA_real_,
    phase = "Secondary",
    subject = "Maths"
  )

  file <- write_test_parquet(input)

  expect_warning(
    read_drivers_data(file),
    regexp = "Missing values detected"
  )
})

# 4 - Tests for read_flows_*_publication_data() -------------------------------------------------------------------

# Create a small list of functions to test

flow_readers <- list(
  `2025` = read_flows_2025_publication_data,
  `2026` = read_flows_2026_publication_data
)

# Does the function have clean column names, derives start_year and drops rows with NA for value?

test_that("flow publication readers clean names, derive start_year and drop NA values", {
  input <- tibble(
    Phase = c("Secondary", "Secondary", "Secondary"),
    Subject = c("Biology", "Biology", "Biology"),
    Type = c("NQE trajectory", "NQE trajectory", "NQE trajectory"),
    Academic_year = c("2024/25", "2025/26", "2026/27"),
    Value = c(1.2, 1.3, NA),
    Unit = c("FTE", "FTE", "FTE"),
    Historic_or_trajectory = "Trajectory",
    Publication_year = 2025
  )

  file <- write_test_parquet(input)

  for (reader in flow_readers) {
    out <- reader(file)

    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 2)
    expect_true("start_year" %in% names(out))
    expect_equal(out$start_year, c(2024L, 2025L))
  }
})

# Does the function throw an error when a column is missing?

test_that("flow publication readers throw an error when required columns are missing", {
  bad_df <- tibble(
    phase = "Secondary",
    subject = "Physics",
    value = 1.5,
    academic_year = "2024/25",
    unit = "FTE",
    publication_year = 2025
  )

  file <- write_test_parquet(bad_df)

  for (reader in flow_readers) {
    expect_missing_cols_error(
      reader(file)
    )
  }
})

# Does the function round values correctly by flow type?
# Check that .5 values are rounded up rather than using banker's rounding

test_that("flow publication readers round values correctly by type", {
  input <- tibble(
    Phase = c("Secondary", "Secondary"),
    Subject = c("Maths", "Maths"),
    Type = c("Leaver rate", "Entrant"),
    Academic_year = c("2024/25", "2024/25"),
    Value = c(0.0545, 10.5),
    Unit = c("%", "FTE"),
    Historic_or_trajectory = "Trajectory",
    Publication_year = 2025
  )

  file <- write_test_parquet(input)

  # Leaver rate rounded to 3 dp:
  # 0.0545 -> 0.055
  # (would be 0.054 under banker's rounding)
  expect_equal(
    read_flows_2025_publication_data(file)$value,
    c(0.055, 11)
  )

  # Entrant count rounded to 0 dp:
  # 10.5 -> 11
  # (would be 10 under banker's rounding)
  expect_equal(
    read_flows_2026_publication_data(file)$value,
    c(0.055, 11)
  )
})

# Does the function warn when academic year is not in yyyy/yy format?

test_that("flow publication readers warns for invalid academic_year format", {
  bad_df <- tibble(
    phase = "Secondary",
    subject = "Physics",
    type = "Entrant",
    academic_year = "202425",
    value = 10,
    unit = "FTE",
    historic_or_trajectory = "Trajectory",
    publication_year = 2025
  )

  file <- write_test_parquet(bad_df)

  for (reader in flow_readers) {
    expect_warning(
      reader(file),
      regexp = "Invalid academic_year format"
    )
  }
})

# Does the function warn when values are negative?

test_that("flow publication readers warn when value contains negative values", {
  input <- tibble(
    phase = "Secondary",
    subject = "Physics",
    type = "Entrant",
    academic_year = "2024/25",
    value = -1,
    unit = "FTE",
    historic_or_trajectory = "Trajectory",
    publication_year = 2025
  )

  file <- write_test_parquet(input)

  for (reader in flow_readers) {
    expect_warning(
      reader(file),
      regexp = "Negative values detected in value column"
    )
  }
})
