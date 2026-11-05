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
    quality = list(data = swc_quality_data, measures = choices_quality_measure)
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
          data = tab$data
        )
      }

      shiny::testServer(server_fn, {
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
