# =============================================================================
# Tests for swc_tab_server(), the shared server logic for the Sufficiency
# metrics, Quality metrics and Subject level metrics tabs.
#
# Each test registers the tab outputs in a minimal server so they can be
# tested without the rest of the app.
# =============================================================================

# Set a Shiny input by (character) name

set_input <- function(session, name, value) {
  do.call(session$setInputs, stats::setNames(list(value), name))
}


test_that("each tab's outputs and downloads work for every measure", {
  tabs <- list(
    swc = list(data = swc_data, measures = choices_swc_measure),
    quality = list(data = swc_quality_data, measures = choices_quality_measure),
    itt = list(data = itt_data, measures = choices_itt_measure)
  )

  for (prefix in names(tabs)) {
    local({
      tab_prefix <- prefix
      tab <- tabs[[prefix]]

      server_fn <- function(input, output, session) {
        swc_tab_server(
          input,
          output,
          session,
          prefix = tab_prefix,
          data = tab$data,
          teacher_type_filter = tab_prefix == "swc"
        )
      }

      shiny::testServer(server_fn, {
        # The teacher type filter is only used by the pupil to teacher ratio
        if (tab_prefix == "swc") {
          set_input(
            session,
            "filter_swc_teacher_type",
            choices_swc_teacher_type[1]
          )
        }

        for (m in tab$measures) {
          set_input(session, paste0("filter_", tab_prefix, "_measure"), m)

          # Plot and table render without error
          expect_no_error(output[[paste0(tab_prefix, "_timeseries_plot")]])
          expect_no_error(output[[paste0("table_", tab_prefix, "_measures")]])

          # CSV, XLSX and JPEG downloads are created
          for (type in c(
            "CSV (Up to 1 MB)",
            "XLSX (Up to 1 MB)",
            "JPEG (Up to 500 KB)"
          )) {
            set_input(session, paste0("file_type_", tab_prefix), type)
            path <- output[[paste0("download_", tab_prefix, "_measures")]]
            expect_true(file.exists(path))
            expect_gt(file.size(path), 0)
          }
        }
      })
    })
  }
})


test_that("the subject tab shows the selected subject alongside 'All subjects'", {
  server_fn <- function(input, output, session) {
    swc_tab_server(
      input,
      output,
      session,
      prefix = "subject",
      data = swc_subject_data,
      breakdown_filter = TRUE,
      comparator = "All subjects"
    )
  }

  shiny::testServer(server_fn, {
    session$setInputs(
      filter_subject_measure = choices_subject_measure[1],
      filter_subject_breakdown = "Biology",
      file_type_subject = "CSV (Up to 1 MB)"
    )

    expect_no_error(output$subject_timeseries_plot)
    expect_no_error(output$table_subject_measures)

    csv <- utils::read.csv(output$download_subject_measures, check.names = FALSE)

    # Selected subject first, then the comparator
    expect_equal(
      names(csv),
      c("Measure", "Academic year", "Biology (%)", "All subjects (%)")
    )
    expect_equal(nrow(csv), 15)

    # Percentages, not proportions
    expect_true(all(csv$`Biology (%)` > 1))

    # Selecting 'All subjects' shows a single line
    session$setInputs(filter_subject_breakdown = "All subjects")
    csv_all <- utils::read.csv(
      output$download_subject_measures,
      check.names = FALSE
    )
    expect_equal(
      names(csv_all),
      c("Measure", "Academic year", "All subjects (%)")
    )
  })
})


test_that("the occasional teachers download holds whole-number headcounts, not percentages", {
  server_fn <- function(input, output, session) {
    swc_tab_server(
      input,
      output,
      session,
      prefix = "quality",
      data = swc_quality_data
    )
  }

  shiny::testServer(server_fn, {
    session$setInputs(
      filter_quality_measure = swc_quality_measure_names[["occasional"]],
      file_type_quality = "CSV (Up to 1 MB)"
    )

    csv <- utils::read.csv(output$download_quality_measures, check.names = FALSE)

    expect_equal(
      names(csv),
      c(
        "Measure", "Academic year", "Total",
        "State-funded nursery and primary", "State-funded secondary"
      )
    )
    expect_true(all(csv$Total == round(csv$Total)))
    expect_true(all(csv$Total > 1000))
  })
})


test_that("the pupil to teacher ratio is one measure, with a teacher type filter choosing the dataset", {
  server_fn <- function(input, output, session) {
    swc_tab_server(
      input,
      output,
      session,
      prefix = "swc",
      data = swc_data,
      teacher_type_filter = TRUE
    )
  }

  shiny::testServer(server_fn, {
    session$setInputs(
      filter_swc_measure = swc_ptr_label,
      filter_swc_teacher_type = "Qualified teachers only",
      file_type_swc = "CSV (Up to 1 MB)"
    )

    csv_qualified <- utils::read.csv(output$download_swc_measures, check.names = FALSE)
    expect_equal(unique(csv_qualified$Measure), swc_measure_names[["ptr_qualified"]])

    session$setInputs(filter_swc_teacher_type = "Qualified and unqualified teachers")

    csv_all <- utils::read.csv(output$download_swc_measures, check.names = FALSE)
    expect_equal(unique(csv_all$Measure), swc_measure_names[["ptr_all"]])

    # Including unqualified teachers gives a lower ratio
    expect_true(all(
      csv_all$`State-funded secondary` < csv_qualified$`State-funded secondary`,
      na.rm = TRUE
    ))
  })
})


test_that("the PGITT metrics download names the ITT data and shows whole percentages", {
  server_fn <- function(input, output, session) {
    swc_tab_server(
      input,
      output,
      session,
      prefix = "itt",
      data = itt_data,
      file_prefix = "itt",
      data_date = itt_data_date
    )
  }

  shiny::testServer(server_fn, {
    session$setInputs(
      filter_itt_measure = itt_measure_names[["need_met"]],
      file_type_itt = "CSV (Up to 1 MB)"
    )

    path <- output$download_itt_measures
    csv <- utils::read.csv(path, check.names = FALSE)

    expect_equal(
      names(csv),
      c("Measure", "Academic year", "Primary (%)", "Secondary (%)")
    )
    expect_equal(csv$`Secondary (%)`, round(csv$`Secondary (%)`))
    expect_true(any(csv$`Primary (%)` > 100))

    session$setInputs(filter_itt_measure = itt_measure_names[["trainees"]])
    csv_trainees <- utils::read.csv(output$download_itt_measures, check.names = FALSE)
    expect_equal(names(csv_trainees), c("Measure", "Academic year", "Primary", "Secondary"))
  })
})


test_that("swc_resolve_measure() only uses the teacher type for the pupil to teacher ratio", {
  expect_equal(
    swc_resolve_measure(swc_ptr_label, "Qualified teachers only"),
    swc_measure_names[["ptr_qualified"]]
  )
  expect_equal(
    swc_resolve_measure(swc_ptr_label, "Qualified and unqualified teachers"),
    swc_measure_names[["ptr_all"]]
  )

  # Other measures are returned unchanged, whatever the teacher type
  expect_equal(
    swc_resolve_measure(swc_measure_names[["vacancies"]], "Qualified teachers only"),
    swc_measure_names[["vacancies"]]
  )
  expect_equal(
    swc_resolve_measure(swc_measure_names[["vacancies"]]),
    swc_measure_names[["vacancies"]]
  )

  expect_error(swc_resolve_measure(swc_ptr_label), "teacher_type")
  expect_error(swc_resolve_measure(swc_ptr_label, "Unknown"), "teacher_type")
})
