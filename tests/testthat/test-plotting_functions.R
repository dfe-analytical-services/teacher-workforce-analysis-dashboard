# -----------------------------------------------------------------------------------------------------------------
# Unit tests for plotting functions (plotting.R)
#
# These tests ensure that:
#   • the function runs without error
#   • the output is a ggplot2 object
#   • the data used in the plot contains expected columns
#   • key layers exist (e.g. geom types)
#   • for interactive ggiraph plots: expected class attributes exist
# -----------------------------------------------------------------------------------------------------------------

# Tests for plot_pupil_teacher_timeseries() -----------------------------------------------------------------------

test_that("plot_pupil_teacher_timeseries works and returns an interactive ggplot object", {
  # Create minimal, valid input data
  # Contains just enough rows/columns for the function to run without error
  df <- data.frame(
    start_year = 2020:2025,
    academic_year = c(
      "2020/21",
      "2021/22",
      "2022/23",
      "2023/24",
      "2024/25",
      "2025/26"
    ),
    pupil_numbers = c(400, 420, 430, 440, 450, 460),
    teacher_numbers = c(20, 21, 22, 23, 24, 25),
    phase = "Primary",
    projection = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
  )

  # Call the plotting function
  p <- plot_pupil_teacher_timeseries(df, phase = "Primary")

  # Basic structural tests
  # The returned object should be a ggplot
  expect_s3_class(p, "ggplot")

  # Make sure the expected input columns exist (sanity check)
  expect_true("start_year" %in% names(df))

  # Check for interactive ggiraph layers
  # Extract all layer classes (e.g. GeomSegmentInteractive, GeomPointInteractive)
  layer_classes <- unlist(lapply(p$layers, function(x) class(x$geom)))

  # Should contain interactive segments (lines)
  expect_true(any(grepl("GeomInteractiveSegment", layer_classes)))
})


# Tests for plot_pgitt_need_timeseries() --------------------------------------------------------------------------

test_that("plot_pgitt_need_timeseries works and returns an interactive ggplot", {
  # Representative minimal dataset
  df <- data.frame(
    start_year = 2020:2025,
    phase = "Secondary",
    subject = rep("Biology", 6),
    pgitt_trainee_need = c(200, 210, 220, 230, 240, 250)
  )

  # Call plot function
  p <- plot_pgitt_need_timeseries(df)

  # Structural tests
  expect_s3_class(p, "ggplot")

  # Check for interactive columns
  layer_classes <- unlist(lapply(p$layers, function(x) class(x$geom)))

  # Should contain GeomInteractiveCol
  expect_true(any(grepl("GeomInteractiveCol", layer_classes)))
})


# Tests for plot_drivers_waterfall() ------------------------------------------------------------------------------

test_that("plot_drivers_waterfall works and returns an interactive ggplot", {
  # Minimal but realistic waterfall input
  df <- data.frame(
    driver = c(
      "2025/26 PGITT need",
      "Demand growth YOY",
      "Leavers",
      "NTSF",
      "2026/27 PGITT need"
    ),
    value = c(3000, -200, 100, -50, 2850)
  )

  # Call plot function
  p <- plot_drivers_waterfall(df)

  # Structural tests
  expect_s3_class(p, "ggplot")

  # Check for interactive rectangle layers (the waterfall bars)
  layer_classes <- unlist(lapply(p$layers, function(x) class(x$geom)))

  # Should contain GeomRectInteractive
  expect_true(any(grepl("GeomInteractiveRect", layer_classes)))
})


# Tests for plot_flow_trajectories() ------------------------------------------------------------------------------

test_that("plot_flow_trajectories works and returns an interactive ggplot", {
  # Create minimal dataset for flow trajectories
  df <- data.frame(
    year = 2020:2025,
    phase = "Secondary",
    subject = "Physics",
    type = "Total leaver rate",
    value = c(0.08, 0.09, 0.10, 0.11, 0.12, 0.13),
    version = c(
      "Last year",
      "Last year",
      "Last year",
      "This year (dummy data)",
      "This year (dummy data)",
      "This year (dummy data)"
    )
  )

  # Call plot function
  p <- plot_flow_trajectories(df)

  # Structural tests
  expect_s3_class(p, "ggplot")

  # Check for expected ggiraph layers
  layer_classes <- unlist(lapply(p$layers, function(x) class(x$geom)))

  # Should contain interactive segments (lines)
  expect_true(any(grepl("GeomInteractiveSegment", layer_classes)))

  # Should contain interactive points
  expect_true(any(grepl("GeomInteractivePoint", layer_classes)))
})
