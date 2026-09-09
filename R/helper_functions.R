# --------------------------------------------------------------------------------------
# This is the helper file, filled with lots of helpful functions!
#
# It is commonly used as an R script to store custom functions used through the
# app to keep the rest of the app code easier to read.
# --------------------------------------------------------------------------------------

# --------------------------------------------------------------------------------------
# Calculate change in pupil and teacher numbers between 2024/25 and 2027/28
# --------------------------------------------------------------------------------------

# Filters the input dataset to 2024 and 2027, orders by year, and
# calculates absolute and percentage changes in pupil and teacher numbers.
#
# param: df A data frame containing pupil and teacher numbers by start year.
#   Must include columns:
#   - start_year
#   - pupil_numbers
#   - teacher_numbers
#   - phase (e.g. "Primary"/"Secondary"), carried through unchanged so
#     downstream summary text can reference it.
#
# return: A data frame with additional columns:
#   - pupil_diff
#   - pupil_pct
#   - teacher_diff
#   - teacher_pct
#
# example:
# calc_pt_change_24_to_27(df)
# --------------------------------------------------------------------------------------

calc_pt_change_24_to_27 <- function(df) {
  # Error if 2024 or 2027 data is missing

  required_years <- c(2024, 2027)
  years_present <- sort(unique(df$start_year))

  if (!all(required_years %in% years_present)) {
    missing_years <- setdiff(required_years, years_present)

    stop(
      "Missing required start_year(s): ",
      paste(missing_years, collapse = ", "),
      ". Data for both 2024 and 2027 is required.",
      call. = FALSE
    )
  }

  df %>%
    # Keep only start years of interest
    dplyr::filter(start_year %in% c(2024, 2027)) %>%
    # Ensure correct ordering for lag calculations
    dplyr::arrange(start_year) %>%
    # Calculate absolute and percentage changes
    dplyr::mutate(
      pupil_diff = pupil_numbers - dplyr::lag(pupil_numbers),
      pupil_pct = (pupil_diff / dplyr::lag(pupil_numbers)) * 100,
      teacher_diff = teacher_numbers - dplyr::lag(teacher_numbers),
      teacher_pct = (teacher_diff / dplyr::lag(teacher_numbers)) * 100
    )
}

# --------------------------------------------------------------------------------------
# Build summary text for change in pupil and teacher numbers between 2024/25 and 2027/28
# --------------------------------------------------------------------------------------

# Takes a data frame of changes (as produced by `calc_pt_change_24_to_27`)
# and returns a human-readable summary sentence describing projected
# changes between 2024/25 and 2027/28.
#
# param: df_change A data frame containing change metrics for 2024 and 2027.
#   Must include a `phase` column (e.g. "Primary"/"Secondary"), which is
#   used to describe the pupils as "primary pupils" or "secondary pupils"
#   in the summary sentence.
#
# return: A single character string suitable for display in a Shiny text output.
#
# example:
# build_pupil_teacher_summary(df_change)
# --------------------------------------------------------------------------------------

build_pupil_teacher_summary <- function(df_change) {
  # Extract the 2027 row (where differences are defined)
  df_27 <- df_change[df_change$start_year == 2027, ]

  # Determine directional wording for changes
  pupil_dir <- if (df_27$pupil_diff > 0) "more" else "fewer"
  teacher_dir <- if (df_27$teacher_diff > 0) "higher" else "lower"

  # Add in phase e.g. gives "primary pupils"/"secondary pupils"
  pupil_label <- paste(tolower(unique(df_27$phase)[1]), "pupils")

  # Construct summary sentence
  paste0(
    "DfE projects that there will be ",
    scales::label_comma()(abs(df_27$pupil_diff)),
    " ",
    pupil_dir,
    " ",
    pupil_label,
    " (",
    sprintf(
      "%.1f%%",
      dfeR::round_five_up(df_27$pupil_pct, dp = 1)
    ),
    ") ",
    "and teacher demand will be ",
    scales::label_comma()(abs(df_27$teacher_diff)),
    " ",
    teacher_dir,
    " (",
    sprintf(
      "%.1f%%",
      dfeR::round_five_up(df_27$teacher_pct, dp = 1)
    ),
    ") ",
    "in 2027/28 compared to 2024/25."
  )
}

# --------------------------------------------------------------------------------------
# Build dynamic title for PGITT trainee need time series outputs
# --------------------------------------------------------------------------------------

# Takes a filtered PGITT trainee need data frame (by phase and subject)
# and returns a human-readable title describing the selected phase/subject
# and academic year range covered by the data.
#
# Generates the title displayed in the chart and above the table
# on the PGITT trainee need time series tab
#
# param: df A data frame containing PGITT trainee need data with the
#             following fields:
#             - phase
#             - subject
#             - start_year
#
# return: A single character string suitable for use as a plot title
#         or table caption in a Shiny app.
# --------------------------------------------------------------------------------------

build_pgitt_need_ts_title <- function(df) {
  phase_val <- unique(df$phase)[1]
  subject_val <- unique(df$subject)[1]

  min_year <- min(df$start_year, na.rm = TRUE)
  max_year <- max(df$start_year, na.rm = TRUE)

  title_prefix <- dplyr::case_when(
    phase_val == "Primary" ~ "Primary",
    phase_val == "Secondary" & subject_val == "Total" ~ "Secondary",
    phase_val == "Secondary" & subject_val != "Total" ~ subject_val,
    TRUE ~ subject_val
  )

  paste0(
    title_prefix,
    " PGITT trainee need ",
    min_year,
    "/",
    sprintf("%02d", (min_year + 1) %% 100),
    " to ",
    max_year,
    "/",
    sprintf("%02d", (max_year + 1) %% 100)
  )
}

# --------------------------------------------------------------------------------------
# Build dynamic title for drivers analysis table 1
# --------------------------------------------------------------------------------------
#
# Takes a filtered drivers analysis data frame (by phase and subject)
# and returns a human-readable title describing the selected phase/subject
# and academic year range covered by the data.
#
# Generates the title displayed above the first table on the Drivers of Change tab
#
# param: df A drivers analysis data frame with the
#            following fields:
#            - phase (e.g. "Primary", "Secondary")
#            - subject (e.g. "Total", "Maths")
#            - start_year (numeric or character, e.g. 2025)
#
# return: A single character string suitable for use as a plot title
#         or table caption in a Shiny app.
# --------------------------------------------------------------------------------------

build_drivers_table_title <- function(df) {
  phase_val <- unique(df$phase)[1]
  subject_val <- unique(df$subject)[1]

  title_prefix <- dplyr::case_when(
    phase_val == "Primary" ~ "Primary",
    phase_val == "Secondary" & subject_val == "Total" ~ "Secondary",
    phase_val == "Secondary" & subject_val != "Total" ~ subject_val,
    TRUE ~ subject_val
  )

  paste0(
    title_prefix,
    " PGITT trainee need: 2026/27 vs 2025/26"
  )
}

# --------------------------------------------------------------------------------------
# Build dynamic title for flow trajectories outputs
# --------------------------------------------------------------------------------------
#
# Takes a filtered flow trajectories data frame and returns a character
# string describing the selected phase / subject and flow type.
#
# Generates the title displayed in the chart and above the table
# on the Flow trajectories tab
#
# param: df A data frame containing at least the following columns:
#           - phase
#           - subject
#           - type
#
# return: A single character string suitable for use as a plot title
#
# --------------------------------------------------------------------------------------

build_flow_traj_title <- function(df) {
  # Defensive checks
  required_cols <- c("phase", "subject", "type")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      "build_flow_traj_title(): data frame is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Extract unique values
  phase_val <- unique(df$phase)[1]
  subject_val <- unique(df$subject)[1]
  type_val <- unique(df$type)[1]

  # Build title prefix
  title_prefix <- dplyr::case_when(
    phase_val == "Primary" ~ "Primary",
    phase_val == "Secondary" & subject_val == "Total" ~ "Secondary",
    phase_val == "Secondary" & subject_val != "Total" ~ subject_val,
    TRUE ~ subject_val
  )

  # Final title
  paste0(
    title_prefix,
    " ",
    tolower(type_val),
    " trajectory"
  )
}

# --------------------------------------------------------------------------------------
# Create tabbed output panel with optional chart, table, and download tabs
# --------------------------------------------------------------------------------------
#
# Generates a Shiny `tabsetPanel` containing up to three tabs:
# "Chart", "Table", and "Download".
#
# Tabs are only included when the corresponding output object is supplied.
# This allows a consistent tabbed layout to be reused across app outputs
# while supporting views that may not require a table or download section.
#
# Additional spacing is applied above the contents of the "Table" and
# "Download" tabs to ensure consistent visual presentation.
#
# param: id A character string used to create a unique tabset panel ID.
#
# param: chart_output A Shiny UI output object to display in the "Chart" tab.
#        This argument is required.
#
# param: table_output An optional Shiny UI output object to display in the
#        "Table" tab. If NULL, the tab is omitted.
#
# param: download_output An optional Shiny UI output object to display in the
#        "Download" tab. If NULL, the tab is omitted.
#
# return: A Shiny `tabsetPanel` containing the supplied tabs.
#
# example:
# create_output_tabs(
#   id = "pgitt_need",
#   chart_output = plotOutput("need_plot"),
#   table_output = reactableOutput("need_table"),
#   download_output = downloadButton("download_data")
# )
#
# --------------------------------------------------------------------------------------

create_output_tabs <- function(
  id,
  chart_output,
  table_output = NULL,
  download_output = NULL
) {
  tabs <- Filter(
    Negate(is.null),
    list(
      if (!is.null(chart_output)) tabPanel("Chart", chart_output),
      if (!is.null(table_output)) {
        tabPanel(
          "Table",
          div(style = "margin-top: 20px;", table_output)
        )
      },
      if (!is.null(download_output)) {
        tabPanel(
          "Download",
          div(style = "margin-top: 40px;", download_output)
        )
      }
    )
  )

  do.call(tabsetPanel, c(list(id = paste0("main_tabs_", id)), tabs))
}

# --------------------------------------------------------------------------------------
# Styling overrides to give a wide, but capped, page width
# --------------------------------------------------------------------------------------
#
# Experimental: a local adaptation of `shinyGovstyle::full_width_overrides()`
# that caps the page at 1800px rather than allowing it to stretch to 100% of
# the viewport width.
#
# This was added after updating the dashboard to align with the latest GDS
# styling, including the addition of a GOV.UK service navigation bar and the
# removal of the left-hand navigation panel. Without a width cap, these
# changes made the page excessively wide on large monitors.
#
# It carries the same caveats as the original function: it is not well tested
# and may cause unexpected styling issues when used alongside components from
# other packages, so use with care.
#
# Returns: HTML containing CSS styling overrides.
#
# Example:
# max_width_overrides()

max_width_overrides <- function() {
  shiny::tags$head(
    shiny::tags$style(
      shiny::HTML(
        # Overall overrides
        ".container-fluid { padding: 0; }",
        # Match GOV.UK Frontend's own gutter values: 15px (mobile) and 30px
        # (desktop). Without this, text would start flush against the viewport
        # edge in a full-width layout, which GOV.UK Frontend normally avoids
        # via its max-width container and auto margins.
        # Cap at 1800px and centre with auto margins once the viewport
        # exceeds that width, rather than stretching to 100% indefinitely.
        # padding-left/-right are set equally so the gutter is symmetric.
        ".govuk-width-container { max-width: 1800px; margin-left: auto; margin-right: auto; padding-left: 15px; padding-right: 15px; }",
        paste0(
          "@media (min-width: 641px) {",
          " .govuk-width-container { padding-left: 30px; padding-right: 30px; } }"
        ),
        ".govuk-grid-row { margin-left: 0; margin-right: 0; }",
        "[class*='govuk-grid-column-'] { padding: 0; }",
        ".govuk-main-wrapper { padding-top: 20px; }",

        # Cookie banner overrides
        ".govuk-button-group { margin-right: 0px; }",

        # Footer overrides
        ".govuk-footer { padding: 2rem; }",
        "html { background-color: #f3f2f1; }",

        # Left content overrides
        ".govuk-contents-box { margin-left: 0; margin-right: 0; }",
        ".govuk-contents-box { padding: 10px; width: fit-content !important; }"
      )
    )
  )
}
