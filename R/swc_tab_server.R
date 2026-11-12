# -----------------------------------------------------------------------------
# Server logic shared by the tabs which show school workforce census (SWC)
# measures over time: Sufficiency metrics, Quality metrics and
# Subject level metrics.
#
# Each tab has the same layout (chart, table and download views with a measure
# filter), so the server logic is written once here and registered per tab with
# a prefix. The prefix is used to build the input / output IDs, which must
# match those created by swc_tab_panel() in R/ui_panels/swc_tab_ui.R:
#
#   filter_<prefix>_measure       measure filter
#   filter_<prefix>_teacher_type  optional teacher type filter (pupil to teacher ratio)
#   filter_<prefix>_breakdown     optional breakdown filter (e.g. subject)
#   file_type_<prefix>            download file type
#   <prefix>_timeseries_plot      chart
#   <prefix>_title_table_ui       table title
#   table_<prefix>_measures       table
#   download_button_ui_<prefix>   download button
#   download_<prefix>_measures    download handler
# -----------------------------------------------------------------------------

# Registers the outputs for one SWC tab
#
# param: input, output, session The Shiny server arguments
# param: prefix Character string used to build input / output IDs
# param: data Long-format SWC data, as returned by read_swc_*(), with columns
#        measure, breakdown, academic_year, start_year, value
# param: breakdown_filter If TRUE the tab has a filter for a single breakdown
#        (e.g. subject), and only that breakdown is shown
# param: comparator Optional breakdown which is always shown alongside the
#        selected breakdown for comparison (e.g. "All subjects")
# param: teacher_type_filter If TRUE the tab has a teacher type filter, used to
#        choose between the pupil to teacher ratio datasets (qualified teachers
#        only, or qualified and unqualified teachers)

swc_tab_server <- function(
  input,
  output,
  session,
  prefix,
  data,
  breakdown_filter = FALSE,
  comparator = NULL,
  teacher_type_filter = FALSE
) {
  measure_id <- paste0("filter_", prefix, "_measure")
  teacher_type_id <- paste0("filter_", prefix, "_teacher_type")
  breakdown_id <- paste0("filter_", prefix, "_breakdown")
  file_type_id <- paste0("file_type_", prefix)

  # Turn a label into something safe to use in a file name
  slug <- function(x) {
    gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(x)))
  }

  # Selected breakdown, e.g. subject (NULL if the tab has no breakdown filter)

  selected_breakdown <- reactive({
    if (!breakdown_filter) {
      return(NULL)
    }
    req(input[[breakdown_id]])
    input[[breakdown_id]]
  })

  # Selected measure, as named in the data. The pupil to teacher ratio is one
  # option in the measure filter, with the teacher type filter choosing which
  # of the two ratio datasets is used

  selected_measure <- reactive({
    measure_in <- input[[measure_id]]
    req(measure_in)

    if (teacher_type_filter && identical(measure_in, swc_ptr_label)) {
      req(input[[teacher_type_id]])
      return(swc_resolve_measure(measure_in, input[[teacher_type_id]]))
    }

    measure_in
  })

  # Data
  # Reactive SWC data filtered by the selected measure (and breakdown)

  filtered <- reactive({
    measure_in <- selected_measure()

    df <- dplyr::filter(data, measure == measure_in) %>%
      # Order breakdowns as they appear in the data. Combined datasets share
      # factor levels across measures, which can otherwise change the order
      dplyr::mutate(
        breakdown = factor(as.character(breakdown), levels = unique(as.character(breakdown)))
      )

    if (breakdown_filter) {
      # Selected breakdown first, so it takes the first colour on the chart
      keep <- unique(c(selected_breakdown(), comparator))

      df <- df %>%
        dplyr::filter(breakdown %in% keep) %>%
        dplyr::mutate(breakdown = factor(as.character(breakdown), levels = keep))
    }

    validate(
      need(nrow(df) > 0, paste0("No rows for measure = ", measure_in))
    )

    df
  })

  measure_selected <- reactive({
    unique(as.character(filtered()$measure))
  })

  # Plot builder that can upscale text when graph is downloaded
  # Keep the look on-screen exactly as is; only enlarge text and add white background if for_download=TRUE

  build_plot <- function(df, for_download = FALSE) {
    p <- plot_swc_measures(df, title_breakdown = selected_breakdown())

    if (for_download) {
      p <- p +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(size = 30),
          axis.title.y = ggplot2::element_text(size = 30),
          axis.text.x = ggplot2::element_text(size = 28),
          axis.text.y = ggplot2::element_text(size = 28),
          legend.text = ggplot2::element_text(size = 28),
          plot.title = ggplot2::element_text(size = 40, face = "bold"),
          plot.caption = ggplot2::element_text(size = 24),

          # Set white background for downloads - prevents issue
          # with devices not rendering the transparent bg properly
          plot.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
    }
    p
  }

  # Graph: SWC measures plot for app (interactive via ggiraph)

  output[[paste0(prefix, "_timeseries_plot")]] <- ggiraph::renderGirafe({
    df <- filtered()

    # remove brief error message that appears before graph fully renders
    req(df, nrow(df) > 0)

    p <- build_plot(df, for_download = FALSE)

    ggiraph::girafe(
      ggobj = p,
      width_svg = 12,
      height_svg = 6,
      options = list(
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(css = "stroke-dasharray:5,5;stroke:black;stroke-width:2px;"),
        ggiraph::opts_hover_key(css = "stroke-dasharray:4,4;"),
        ggiraph::opts_sizing(rescale = TRUE, width = 1),
        ggiraph::opts_toolbar(saveaspng = FALSE, hidden = "saveaspng")
      )
    )
  })

  # Reactive title for the selected measure
  # Used to keep chart and table titles consistent

  title <- reactive({
    build_swc_measure_title(filtered(), selected_breakdown())
  })

  # Render the reactive title as GOV.UK-styled heading text
  # uiOutput() is used in the tab UI to allow this to update dynamically

  output[[paste0(prefix, "_title_table_ui")]] <- renderUI({
    heading_text(title(), level = 3, size = "s")
  })

  # Table: SWC measures table for app (interactive via govReactable)
  # One row per academic year and one column per breakdown

  table_data <- reactive({
    filtered() %>%
      dplyr::arrange(start_year, breakdown) %>%
      dplyr::select(
        `Academic year` = academic_year,
        breakdown,
        value
      ) %>%
      tidyr::pivot_wider(names_from = breakdown, values_from = value)
  })

  output[[paste0("table_", prefix, "_measures")]] <- renderGovReactable({
    df <- table_data()

    value_cols <- setdiff(names(df), "Academic year")

    # Rates are shown as percentages, ratios and counts as numbers
    value_type <- swc_value_type(measure_selected())
    value_digits <- swc_value_digits(measure_selected())

    govReactable(
      df,
      right_col = value_cols,
      pagination = FALSE,
      searchable = FALSE,
      filterable = FALSE,
      striped = FALSE,
      highlight = TRUE,
      resizable = TRUE,
      columns = stats::setNames(
        lapply(value_cols, function(x) {
          reactable::colDef(
            format = reactable::colFormat(
              digits = value_digits,
              percent = value_type == "rate",
              separators = value_type == "count"
            )
          )
        }),
        value_cols
      )
    )
  })

  # Create download dataset (matches table, with rates as percentages and
  # ratios and counts left as numbers)

  download_table <- reactive({
    value_type <- swc_value_type(measure_selected())
    value_digits <- swc_value_digits(measure_selected())
    is_rate <- value_type == "rate"

    filtered() %>%
      dplyr::arrange(start_year, breakdown) %>%
      dplyr::mutate(
        value = if (is_rate) {
          dfeR::round_five_up(value * 100, dp = value_digits)
        } else {
          dfeR::round_five_up(value, dp = value_digits)
        },
        breakdown = if (is_rate) {
          paste0(breakdown, " (%)")
        } else {
          as.character(breakdown)
        }
      ) %>%
      dplyr::select(
        Measure = measure,
        `Academic year` = academic_year,
        breakdown,
        value
      ) %>%
      tidyr::pivot_wider(names_from = breakdown, values_from = value)
  })

  # Create download chart (static ggplot for export)

  download_chart <- reactive({
    build_plot(filtered(), for_download = TRUE)
  })

  # Download button UI

  download_id <- paste0("download_", prefix, "_measures")

  output[[paste0("download_button_ui_", prefix)]] <- renderUI({
    shinyGovstyle::download_button(
      download_id, # must match downloadHandler id below
      "Download Chart Data",
      file_type = tolower(sub(" .*", "", input[[file_type_id]])), # extract leading token
      file_size = NULL
    )
  })

  # Download handler (CSV/XLSX/JPEG)

  output[[download_id]] <- downloadHandler(
    filename = function() {
      # make file name which adds the selected measure (and breakdown)
      file_name <- paste(
        c(
          "swc",
          slug(selected_measure()),
          if (breakdown_filter) slug(selected_breakdown()),
          swc_data_date
        ),
        collapse = "_"
      )

      extension <- if (input[[file_type_id]] == "CSV (Up to 1 MB)") {
        ".csv"
      } else if (input[[file_type_id]] == "XLSX (Up to 1 MB)") {
        ".xlsx"
      } else {
        ".jpeg"
      }
      paste0(file_name, extension)
    },
    content = function(file) {
      if (input[[file_type_id]] == "CSV (Up to 1 MB)") {
        utils::write.csv(
          download_table(),
          file,
          row.names = FALSE
        )
      } else if (input[[file_type_id]] == "XLSX (Up to 1 MB)") {
        # Notify because Excel can take a little while to generate
        pop_up <- showNotification("Generating download file", duration = NULL)
        on.exit(removeNotification(pop_up), add = TRUE)
        openxlsx::write.xlsx(
          download_table(),
          file,
          colWidths = "Auto"
        )
      } else {
        # JPEG: save static ggplot.
        tmp_file <- tempfile(paste0(
          "swc_",
          prefix,
          "_",
          swc_data_date,
          ".jpeg"
        ))
        ggplot2::ggsave(
          filename = tmp_file,
          plot = download_chart(),
          device = "jpeg",
          width = 10,
          height = 6,
          dpi = 300
        )
        file.copy(tmp_file, file, overwrite = TRUE)
      }
    }
  )

  invisible(NULL)
}
