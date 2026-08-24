# -----------------------------------------------------------------------------------------------------------------
# Unit tests for pupil–teacher summary helper functions
#
# These tests ensure that:
#   • pupil and teacher changes between 2024 and 2027 are calculated correctly
#   • percentage changes are computed as expected
#   • summary text is generated with correct wording and formatting
#   • increase and decrease scenarios are handled correctly
#   • percentage formatting uses 1 decimal place and rounds .5 values up
#   • errors are raised when required year data is missing
#
# Functions under test:
#   • calc_pt_change_24_to_27()
#   • build_pupil_teacher_summary()
# -----------------------------------------------------------------------------------------------------------------

# Dummy data with increases in both pupils and teachers
test_df_increase <- tibble::tibble(
  start_year = c(2024, 2027),
  pupil_numbers = c(100000, 105000),
  teacher_numbers = c(5000, 5200)
)


# Test: calculation of absolute and percentage changes ------------------------------------------------------------

test_that("calc_pt_change_24_to_27 computes correct differences and percentages", {
  # Run calculation helper
  result <- calc_pt_change_24_to_27(test_df_increase)

  # Extract the 2027 row where differences are defined
  df_27 <- result[result$start_year == 2027, ]

  # Check absolute differences
  # 105000 - 100000 = 5000
  expect_equal(df_27$pupil_diff, 5000)
  # 5200 - 5000 = 200
  expect_equal(df_27$teacher_diff, 200)

  # Check percentage differences
  # (5000 / 100000) * 100 = 5
  expect_equal(df_27$pupil_pct, 5)
  # (200 / 5000) * 100 = 4
  expect_equal(df_27$teacher_pct, 4)
})


# Test: summary text generation for increases ---------------------------------------------------------------------

test_that("build_pupil_teacher_summary creates correct text for increases", {
  # Create summary text using increasing data
  summary_text <- build_pupil_teacher_summary(
    calc_pt_change_24_to_27(test_df_increase)
  )

  # Verify correct directional language and formatting
  expect_match(summary_text, "5,000 more pupils")
  expect_match(summary_text, "200 higher")
  expect_match(summary_text, "5.0%")
  expect_match(summary_text, "4.0%")

  # Verify correct comparison years are referenced
  expect_match(summary_text, "in 2027/28 compared to 2024/25")
})


# Test: summary text generation for decreases ---------------------------------------------------------------------

test_that("build_pupil_teacher_summary handles decreases correctly", {
  # Input data with decreases in pupils and teachers
  test_df_decrease <- tibble::tibble(
    start_year = c(2024, 2027),
    # 95000 - 100000 = -5000
    # ((95000 - 100000)/ 100000) * 100 = -5%
    pupil_numbers = c(100000, 95000),
    # 4800 - 5000 = -200
    # ((4800 - 5000)/5000) * 100 = -4%
    teacher_numbers = c(5000, 4800)
  )

  # Create summary text for decreasing scenario
  summary_text <- build_pupil_teacher_summary(
    calc_pt_change_24_to_27(test_df_decrease)
  )

  # Check directional wording switches appropriately
  expect_match(summary_text, "5,000 fewer pupils")
  expect_match(summary_text, "200 lower")

  # Check negative percentage formatting
  expect_match(summary_text, "-5.0%")
  expect_match(summary_text, "-4.0%")
})


# Test: percentage formatting uses 1 decimal place and avoids banker's rounding  ----------------------------------

test_that(
  "build_pupil_teacher_summary rounds percentages to 1 dp and does not use banker's rounding",
  {
    # Input data that produces percentage changes ending in .05.
    # Expected outputs are 2.3% (not 2.2%) and 4.7% (not 4.6%),
    # confirming that .5 values are rounded up rather than using
    # banker's rounding.
    test_df_rounding <- tibble::tibble(
      start_year = c(2024, 2027),
      # 102250 - 100000 = 2250
      # (2250 / 100000) * 100 = 2.25%
      pupil_numbers = c(100000, 102250), # 2.25%
      # 10465 - 10000 = 465
      # (465 / 10000) * 100 = 4.65%
      teacher_numbers = c(10000, 10465) # 4.65%
    )

    summary_text <- build_pupil_teacher_summary(
      calc_pt_change_24_to_27(test_df_rounding)
    )

    # .5 values should round up to 1 dp
    expect_match(summary_text, "2.3%")
    expect_match(summary_text, "4.7%")

    # Check banker's rounding has not been applied
    expect_no_match(summary_text, "2.2%")
    expect_no_match(summary_text, "4.6%")
  }
)


# Test: error handling when required comparison year is missing ---------------------------------------------------

test_that("calc_pt_change_24_to_27 errors if 2027 data is missing", {
  # Input data missing the 2027 comparison year
  df_missing <- tibble::tibble(
    start_year = 2024,
    pupil_numbers = 100000,
    teacher_numbers = 5000
  )

  # Calculation should fail because data for 2027 is missing
  expect_error(
    build_pupil_teacher_summary(
      calc_pt_change_24_to_27(df_missing)
    )
  )
})
