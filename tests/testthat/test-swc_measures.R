# =============================================================================
# Unit tests for the School workforce census (SWC) measures tab helpers
#   • swc_percent_accuracy()
#   • build_swc_measure_title()
#   • plot_swc_measures()
# =============================================================================

# Minimal long-format dataset matching the output of read_swc_*()

make_swc_df <- function(measure = "Qualified teacher entrant rate") {
  tidyr::expand_grid(
    breakdown = factor(c("Total", "Newly qualified entrants")),
    start_year = 2020:2024
  ) |>
    dplyr::mutate(
      measure = measure,
      academic_year = paste0(
        start_year,
        "/",
        substr(start_year + 1, 3, 4)
      ),
      value = ifelse(breakdown == "Total", 0.1, 0.05)
    )
}


# Tests for swc_percent_accuracy() --------------------------------------------------------------------------------

test_that("swc_percent_accuracy() uses 2 dp for vacancy rates and 1 dp otherwise", {
  expect_equal(swc_percent_accuracy(swc_measure_names[["vacancies"]]), 0.01)
  expect_equal(swc_percent_accuracy(swc_measure_names[["entrants"]]), 0.1)
  expect_equal(swc_percent_accuracy(swc_measure_names[["leavers"]]), 0.1)
})


# Tests for build_swc_measure_title() -----------------------------------------------------------------------------

test_that("build_swc_measure_title() describes the measure and year range", {
  expect_equal(
    build_swc_measure_title(make_swc_df()),
    "Qualified teacher entrant rate, 2020/21 to 2024/25"
  )
})

test_that("build_swc_measure_title() errors when required columns are missing", {
  expect_error(
    build_swc_measure_title(data.frame(measure = "x")),
    regexp = "missing required columns"
  )
})


# Tests for plot_swc_measures() -----------------------------------------------------------------------------------

test_that("plot_swc_measures() returns an interactive ggplot with a line per breakdown", {
  p <- plot_swc_measures(make_swc_df())

  expect_s3_class(p, "ggplot")

  layer_classes <- unlist(lapply(p$layers, function(x) class(x$geom)))

  # Interactive hover lines, plus standard lines and points
  expect_true(any(grepl("GeomInteractiveVline", layer_classes)))
  expect_true(any(grepl("GeomLine", layer_classes)))
  expect_true(any(grepl("GeomPoint", layer_classes)))

  # Title is built from the data
  expect_equal(
    p$labels$title,
    "Qualified teacher entrant rate, 2020/21 to 2024/25"
  )

  # The plot can be fully built
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_swc_measures() ignores unused factor levels", {
  df <- make_swc_df()
  df$breakdown <- factor(
    as.character(df$breakdown),
    levels = c("Total", "Newly qualified entrants", "Unused level")
  )

  p <- plot_swc_measures(df)

  expect_no_error(ggplot2::ggplot_build(p))
})


# Tests for ratio measures (pupil to teacher ratios) --------------------------------------------------------------

test_that("swc_is_ratio() identifies the pupil to teacher ratio measures", {
  expect_true(swc_is_ratio(swc_measure_names[["ptr_qualified"]]))
  expect_true(swc_is_ratio(swc_measure_names[["ptr_all"]]))
  expect_false(swc_is_ratio(swc_measure_names[["vacancies"]]))
  expect_false(swc_is_ratio(swc_measure_names[["entrants"]]))
  expect_false(swc_is_ratio(swc_measure_names[["leavers"]]))
})

test_that("swc_value_labeller() formats rates as percentages and ratios as numbers", {
  expect_equal(
    swc_value_labeller(swc_measure_names[["entrants"]])(0.092),
    "9.2%"
  )
  expect_equal(
    swc_value_labeller(swc_measure_names[["vacancies"]])(0.0028),
    "0.28%"
  )
  expect_equal(
    swc_value_labeller(swc_measure_names[["ptr_qualified"]])(17.34),
    "17.3"
  )
})

test_that("plot_swc_measures() handles ratio measures and states the SWC source", {
  df <- make_swc_df(swc_measure_names[["ptr_qualified"]]) |>
    dplyr::mutate(value = ifelse(breakdown == "Total", 21.3, 17.3))

  p <- plot_swc_measures(df)

  expect_no_error(ggplot2::ggplot_build(p))

  # Y axis is a plain number scale rather than a percentage
  y_labels <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  expect_false(any(grepl("%", y_labels)))

  # Source is stated on every SWC chart, including rates
  expect_match(p$labels$caption, "school workforce census", ignore.case = TRUE)
  expect_match(
    plot_swc_measures(make_swc_df())$labels$caption,
    "school workforce census",
    ignore.case = TRUE
  )
})


# Tests for value types, axis titles and counts -------------------------------------------------------------------

test_that("swc_value_type() classifies every SWC measure", {
  for (m in swc_measure_names) {
    expect_true(swc_value_type(m) %in% c("rate", "ratio"))
  }
  expect_equal(swc_value_type(swc_measure_names[["ptr_all"]]), "ratio")
  expect_equal(swc_value_type(swc_measure_names[["leavers"]]), "rate")

  expect_equal(swc_value_type(swc_quality_measure_names[["occasional"]]), "count")
  expect_equal(swc_value_type(swc_quality_measure_names[["in_service"]]), "rate")
  expect_equal(swc_value_type(swc_quality_measure_names[["qts"]]), "rate")
  expect_equal(swc_value_type(swc_subject_measure_names[["specialism"]]), "rate")
})

test_that("swc_value_digits() and swc_value_labeller() handle counts", {
  occasional <- swc_quality_measure_names[["occasional"]]

  expect_equal(swc_value_digits(occasional), 0)
  expect_equal(swc_value_digits(swc_measure_names[["vacancies"]]), 2)
  expect_equal(swc_value_digits(swc_measure_names[["leavers"]]), 1)
  expect_equal(swc_value_digits(swc_measure_names[["ptr_all"]]), 1)

  expect_equal(swc_value_labeller(occasional)(16021), "16,021")
})

test_that("axis titles reflect the measure", {
  expect_equal(
    swc_x_axis_title(swc_quality_measure_names[["in_service"]]),
    "Academic year of qualification"
  )
  expect_equal(swc_x_axis_title(swc_measure_names[["leavers"]]), "Academic year")

  expect_equal(
    swc_y_axis_title(swc_measure_names[["leavers"]]),
    "Qualified teacher leaver rate (%)"
  )
  expect_equal(
    swc_y_axis_title(swc_quality_measure_names[["occasional"]]),
    "Headcount of occasional teachers"
  )
})

test_that("build_swc_measure_title() can name the selected breakdown", {
  expect_equal(
    build_swc_measure_title(
      make_swc_df(swc_subject_measure_names[["specialism"]]),
      breakdown_label = "Biology"
    ),
    "Specialism rate: Biology, 2020/21 to 2024/25"
  )
})

test_that("plot_swc_measures() handles counts and a named breakdown", {
  df <- make_swc_df(swc_quality_measure_names[["occasional"]]) |>
    dplyr::mutate(value = ifelse(breakdown == "Total", 16021, 5269))

  p <- plot_swc_measures(df, title_breakdown = "Total")

  expect_no_error(ggplot2::ggplot_build(p))
  expect_match(p$labels$title, "Headcount of occasional teachers: Total")

  y_labels <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
  expect_false(any(grepl("%", y_labels)))
})

test_that("in service chart labels the x axis as the year of qualification", {
  df <- make_swc_df(swc_quality_measure_names[["in_service"]]) |>
    dplyr::mutate(value = ifelse(breakdown == "Total", 0.86, 0.7))

  expect_equal(
    plot_swc_measures(df)$labels$x,
    "Academic year of qualification"
  )
})
