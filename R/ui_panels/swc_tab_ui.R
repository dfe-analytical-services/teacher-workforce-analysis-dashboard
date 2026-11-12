# # ---------------------------------------------------------------------------------------------------------------
#
# USER INTERFACE COMPONENTS FOR: tabs showing school workforce census (SWC) measures over time
# (Sufficiency metrics, Quality metrics and Subject level metrics), and the PGITT metrics tab,
# which uses the same layout with data from the ITT census and ITT performance profiles
#
# Each tab has the same layout: chart / table / download views on the left, and a measure filter
# with an "About this graph" box on the right, followed by a definitions box.
# The matching server logic is swc_tab_server() in R/swc_tab_server.R. The prefix is used to build
# the input / output IDs, so must be the same in both.
#
# # ---------------------------------------------------------------------------------------------------------------

# Builds one SWC tab
#
# param: prefix Character string used to build input / output IDs
# param: title Tab title, also used as the section heading
# param: intro Introductory text (gov_text elements) shown under the heading
# param: measure_choices Character vector of measures for the measure filter
# param: about Text (gov_text elements) for the "About this graph" box
# param: definitions Text (gov_text elements) for the "Definitions" box
# param: breakdown_choices Optional character vector of choices for a second
#        filter (e.g. subjects). If NULL no second filter is shown
# param: breakdown_label Label for the second filter
# param: teacher_type_choices Optional character vector of choices for a filter
#        for whether unqualified teachers are included. It is only shown when the
#        pupil to teacher ratio (swc_ptr_label) is selected in the measure filter.
#        If NULL no teacher type filter is added
# param: data_source Name of the data source(s), shown in the source text on the
#        tab. Defaults to the school workforce census
# param: data_updated Date the data was last updated, in d/m/yyyy format

swc_tab_panel <- function(
  prefix,
  title,
  intro,
  measure_choices,
  about,
  definitions,
  breakdown_choices = NULL,
  breakdown_label = NULL,
  teacher_type_choices = NULL,
  data_source = "school workforce census (SWC)",
  data_updated = swc_data_updated
) {
  has_second_filter <- !is.null(breakdown_choices) || !is.null(teacher_type_choices)

  tabPanel(
    id = paste0(prefix, "_measures"),
    title,
    gov_main_layout(
      heading_text(title, level = 2, size = "m"),
      gov_text(
        strong(paste0("Data source: ", data_source, "."))
      ),
      intro,
      bslib::layout_columns(
        col_widths = bslib::breakpoints(md = c(12, 12), lg = c(8, 4)),

        # Left column with chart/table/download
        create_output_tabs(
          paste0(prefix, "_measures"),

          # Mini tab 1 - chart
          chart_output = div(
            style = "margin-top: 3rem;",
            ggiraph::girafeOutput(
              outputId = paste0(prefix, "_timeseries_plot"),
              width = NULL,
              height = NULL
            )
          ),

          # Mini tab 2 - table
          table_output = div(
            uiOutput(paste0(prefix, "_title_table_ui")),
            gov_text(
              tags$small(paste0("Source: ", data_source, "."))
            ),
            govReactableOutput(
              paste0("table_", prefix, "_measures"),
              caption = "",
              caption_size = "s",
              heading_level = "h3"
            )
          ),

          # Mini tab 3 - download
          download_output = tagList(
            radioButtons(
              inputId = paste0("file_type_", prefix),
              label = "Choose download file format",
              choices = c(
                "CSV (Up to 1 MB)",
                "XLSX (Up to 1 MB)",
                "JPEG (Up to 500 KB)"
              ),
              selected = "CSV (Up to 1 MB)"
            ),
            uiOutput(paste0("download_button_ui_", prefix))
          )
        ),

        # Right column: sidebar with filter(s) and about this graph
        div(
          class = "sidebar",
          style = "top: 12px; overflow: visible;",

          # Filters
          bslib::card(
            bslib::card_header(
              gov_text(if (has_second_filter) "Filters" else "Filter")
            ),
            bslib::card_body(
              shinyGovstyle::select_Input(
                inputId = paste0("filter_", prefix, "_measure"),
                label = "Select a measure:",
                select_text = measure_choices,
                select_value = measure_choices
              ),
              if (!is.null(teacher_type_choices)) {
                conditionalPanel(
                  condition = sprintf(
                    "input.filter_%s_measure == '%s'",
                    prefix,
                    swc_ptr_label
                  ),
                  shinyGovstyle::select_Input(
                    inputId = paste0("filter_", prefix, "_teacher_type"),
                    label = "Select which teachers to include:",
                    select_text = teacher_type_choices,
                    select_value = teacher_type_choices
                  )
                )
              },
              if (!is.null(breakdown_choices)) {
                shinyGovstyle::select_Input(
                  inputId = paste0("filter_", prefix, "_breakdown"),
                  label = breakdown_label,
                  select_text = breakdown_choices,
                  select_value = breakdown_choices
                )
              }
            )
          ),

          # About this graph text box
          bslib::card(
            bslib::card_header(gov_text("About this graph")),
            bslib::card_body(
              gov_text(
                paste0("The data on this tab comes from the ", data_source, ".")
              ),
              about
            )
          )
        )
      ),

      # Text box below graph with definitions
      bslib::card(
        bslib::card_header(gov_text("Definitions")),
        bslib::card_body(
          definitions,
          gov_text(paste0("Data source: ", data_source, ".")),
          gov_text("Data last updated: ", data_updated)
        )
      )
    )
  )
}
