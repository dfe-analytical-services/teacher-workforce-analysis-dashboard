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
  write_parquet(df, tmp)
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

test_that("read_pupil_teacher_numbers() cleans names, rounds .5s up, and derives start_year", {
  # Dummy input parquet
  input <- tibble::tibble(
    `Academic year` = c("2020/21", "2021/22"),
    `Pupil numbers` = c(1234.8, 5678.2),
    `Teacher numbers` = c(400.5, 999.6),
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
      ) %in% names(out)
    )
  )

  # Start year column check
  expect_true("start_year" %in% names(out)) # check it creates expected columns
  expect_equal(out$start_year, c(2020L, 2021L)) # check correct

  # Rounding check
  expect_equal(out$pupil_numbers, c(1235, 5678))
  expect_equal(out$teacher_numbers, c(401, 1000))
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
  tmp <- write_test_parquet(bad_df)

  expect_missing_cols_error(
    read_pupil_teacher_numbers(tmp)
  )
})


# 2 - Tests for read_pgitt_need_timeseries() -----------------------------------------------------------------------

# Does the function return a dataframe, have the column names expected, extract start year,
# call subject = total for primary otherwise keep original subject values?

test_that("read_pgitt_need_timeseries() cleans names, renames phase and derives start_year", {
  input <- tibble::tibble(
    `Time period` = c("2019/20", "2020/21", "2020/21"),
    `Education phase` = c("Primary", "Secondary", "Secondary"),
    Subject = c("English", "Maths", "Biology"),
    `PGITT trainee need count` = c("6", "6", "6"),
    `Difference to previous year count` = c("6", "6", "6"),
    `Difference to previous year percent` = c("6", "6", "6")
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
      ) %in% names(out)
    )
  )


  # Created column checks
  expect_true("start_year" %in% names(out))
  expect_true("academic_year" %in% names(out))

  # start_year derived from first 4 characters
  expect_equal(out$start_year, c(2019L, 2020L, 2020L))


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
    time_period = "2020/21",
    subject = "English",
    pgitt_trainee_need_count = 200,
    difference_to_previous_year_percent = 0.1
  )

  tmp <- write_test_parquet(bad_df)

  expect_missing_cols_error(
    read_pgitt_need_timeseries(tmp)
  )
})

# 3 - Tests for read_drivers_data() --------------------------------------------------------------------------------

# Does the function have clean column names and round values to 1dp?

test_that("read_drivers_data() cleans names, preserves rows and rounds values to 1 decimal place", {
  input <- tibble::tibble(
    "Driver" = c("Entrants", "Leavers"),
    "Value" = c(100.123, 200.876),
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
  expect_equal(out$value, c(100.1, 200.9))
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


# 4 - Tests for read_flows_*_publication_data() -------------------------------------------------------------------

## read_flows_2025_publication_data checks

# Does the function have clean column names, derives start_year and drops rows with NA for value?

test_that(
  "read_flows_2025_publication_data cleans names, derives start_year, and drops NA values",
  {
    input <- tibble(
      Phase = c("Secondary", "Secondary", "Secondary"),
      Subject = c("Biology", "Biology", "Biology"),
      Type = c("NQE trajectory", "NQE trajectory", "NQE trajectory"),
      Academic_year = c("2024/25", "2025/26", "2026/27"),
      Value = c(1.2, 1.3, NA), # third row should be dropped
      Unit = c("FTE", "FTE", "FTE"),
      Historic_or_trajectory = "Trajectory",
      Publication_year = 2025
    )

    file <- write_test_parquet(input)

    out <- read_flows_2025_publication_data(file)

    # Structure checks
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 2) # NA row removed

    # Required columns checks
    expect_true(
      all(
        c(
          "phase",
          "subject",
          "type",
          "academic_year",
          "value",
          "unit",
          "historic_or_trajectory",
          "publication_year"
        ) %in% names(out)
      )
    )

    # Derived column check
    expect_true("start_year" %in% names(out))
    expect_equal(out$start_year, c(2024L, 2025L))
  }
)

# Does the function throw an error when a column is missing?

test_that(
  "read_flows_2025_publication_data throws an error when required columns are missing",
  {
    bad_df <- tibble(
      phase = "Secondary",
      subject = "Physics",
      value = 1.5,
      academic_year = "2024/25",
      unit = "FTE",
      publication_year = 2025
      # missing type and historic_or_trajectory
    )

    tmp <- write_test_parquet(bad_df)

    expect_missing_cols_error(
      read_flows_2025_publication_data(tmp)
    )
  }
)

## read_flows_2026_publication_data checks

# Does the function have clean column names, derives start_year and drops rows with NA for value?

test_that(
  "read_flows_2026_publication_data cleans names, derives start_year, and drops NA values",
  {
    input <- tibble(
      Phase = c("Secondary", "Secondary", "Secondary"),
      Subject = c("Maths", "Maths", "Maths"),
      Type = c("NQE trajectory", "NQE trajectory", "NQE trajectory"),
      Academic_year = c("2025/26", "2026/27", "2027/28"),
      Value = c(2.1, 2.2, NA), # third row should be dropped
      Unit = c("FTE", "FTE", "FTE"),
      Historic_or_trajectory = "Trajectory",
      Publication_year = 2026
    )

    file <- write_test_parquet(input)

    out <- read_flows_2026_publication_data(file)

    # Structure checks
    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 2)

    # Required columns checks
    expect_true(
      all(
        c(
          "phase",
          "subject",
          "type",
          "academic_year",
          "value",
          "unit",
          "historic_or_trajectory",
          "publication_year"
        ) %in% names(out)
      )
    )

    # Derived column check
    expect_equal(out$start_year, c(2025L, 2026L))
  }
)

# Does the function throw an error when a column is missing?

test_that(
  "read_flows_2026_publication_data throws an error when required columns are missing",
  {
    bad_df <- tibble(
      phase = "Secondary",
      subject = "Physics",
      value = 1.5,
      academic_year = "2025/26",
      unit = "FTE",
      publication_year = 2026
      # missing type and historic_or_trajectory
    )

    tmp <- write_test_parquet(bad_df)

    expect_missing_cols_error(
      read_flows_2026_publication_data(tmp)
    )
  }
)
