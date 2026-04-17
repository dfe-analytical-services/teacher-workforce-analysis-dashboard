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
#
# return: A data frame with additional columns:
#   - pupil_diff
#   - pupil_pct
#   - teacher_diff
#   - teacher_pct
#
# example:
# calc_pt_change_24_to_27(df)

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
#
# return: A single character string suitable for display in a Shiny text output.
#
# example:
# build_pupil_teacher_summary(df_change)

build_pupil_teacher_summary <- function(df_change) {
  # Extract the 2027 row (where differences are defined)
  df_27 <- df_change[df_change$start_year == 2027, ]

  # Determine directional wording for changes
  pupil_dir <- if (df_27$pupil_diff > 0) "more" else "fewer"
  teacher_dir <- if (df_27$teacher_diff > 0) "higher" else "lower"

  # Construct summary sentence
  glue::glue(
    "DfE project there will be ",
    scales::label_comma()(abs(df_27$pupil_diff)),
    " ",
    pupil_dir,
    " pupils (",
    scales::label_number(accuracy = 0.1, suffix = "%")(df_27$pupil_pct),
    ") ",
    "and teacher demand to be ",
    scales::label_comma()(abs(df_27$teacher_diff)),
    " ",
    teacher_dir,
    " (",
    scales::label_number(accuracy = 0.1, suffix = "%")(df_27$teacher_pct),
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
# The title is designed for use across multiple outputs, including
# ggplot chart titles and GOV.UK Reactable table captions, ensuring
# consistency between visual and tabular views.
#
# param: df A data frame containing PGITT trainee need data with the
#             following fields:
#             - phase
#             - subject
#             - start_year
#
# return: A single character string suitable for use as a plot title
#         or table caption in a Shiny app.


build_pgitt_need_ts_title <- function(df) {
  phase_selected <- unique(df$phase)
  subject_selected <- unique(df$subject)

  phase_val <- phase_selected[1]
  subject_val <- subject_selected[1]

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
# The title is designed for use across multiple outputs, including
# ggplot chart titles and GOV.UK Reactable table captions, ensuring
# consistency between visual and tabular views.
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
  phase_selected <- unique(df$phase)
  subject_selected <- unique(df$subject)

  phase_val <- if (length(phase_selected) == 1) {
    phase_selected
  } else {
    phase_selected[1]
  }

  subject_val <- if (length(subject_selected) == 1) {
    subject_selected
  } else {
    subject_selected[1]
  }

  title_prefix <- dplyr::case_when(
    phase_val == "Primary" ~ "Primary",
    phase_val == "Secondary" && subject_val == "Total" ~ "Secondary",
    phase_val == "Secondary" && subject_val != "Total" ~ subject_val,
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
# Designed for use in downloaded plots, chart titles, or captions to ensure
# consistent naming across outputs.
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
  # Defensive checks ----------------------------------------------------------
  required_cols <- c("phase", "subject", "type")
  missing_cols <- setdiff(required_cols, names(df))

  if (length(missing_cols) > 0) {
    stop(
      "build_flow_traj_title(): data frame is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  # Extract unique values -----------------------------------------------------
  phase_selected <- unique(df$phase)
  subject_selected <- unique(df$subject)
  type_selected <- unique(df$type)

  # Pick a single value if filters return more than one ------------------------
  phase_val <- if (length(phase_selected) == 1) {
    phase_selected
  } else {
    phase_selected[1]
  }

  subject_val <- if (length(subject_selected) == 1) {
    subject_selected
  } else {
    subject_selected[1]
  }

  type_val <- if (length(type_selected) == 1) {
    type_selected
  } else {
    type_selected[1]
  }

  # Build title prefix --------------------------------------------------------
  title_prefix <- dplyr::case_when(
    phase_val == "Primary" ~ "Primary",
    phase_val == "Secondary" && subject_val == "Total" ~ "Secondary",
    phase_val == "Secondary" && subject_val != "Total" ~ subject_val,
    TRUE ~ subject_val
  )

  # Final title ---------------------------------------------------------------
  paste0(
    title_prefix,
    " ",
    tolower(type_val),
    " trajectory"
  )
}

# # FROM TEMPLATE -------------------------------------------------------------------------------------------------

# Value box function ----------------------------------------------------------
# fontsize: can be small, medium or large
value_box <- function(
  value,
  subtitle,
  icon = NULL,
  color = "blue",
  width = 4,
  href = NULL,
  fontsize = "medium"
) {
  validate_color(color)
  if (!is.null(icon)) tagAssert(icon, type = "i")

  box_content <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      p(value, id = paste0("vboxhead-", fontsize)),
      p(subtitle, id = paste0("vboxdetail-", fontsize))
    ),
    if (!is.null(icon)) div(class = "icon-large", icon)
  )

  if (!is.null(href)) {
    box_content <- a(href = href, box_content)
  }

  div(
    class = if (!is.null(width)) paste0("col-sm-", width),
    box_content
  )
}

# Valid colours for value box -------------------------------------------------
valid_colors <- c("blue", "dark-blue", "green", "orange", "purple", "white")

# Validate that only valid colours are used -----------------------------------
validate_color <- function(color) {
  if (color %in% valid_colors) {
    return(TRUE)
  }

  stop(
    "Invalid color: ",
    color,
    ". Valid colors are: ",
    paste(valid_colors, collapse = ", "),
    "."
  )
}

# GSS colours -----------------------------------------------------------------
# Current GSS colours for use in charts. These are taken from the current
# guidance here:
# https://analysisfunction.civilservice.gov.uk/policy-store/data-visualisation-colours-in-charts/
# Note the advice on trying to keep to a maximum of 4 series in a single plot
# AF colours package guidance here: https://best-practice-and-impact.github.io/afcolours/
suppressMessages(
  gss_colour_pallette <- afcolours::af_colours(
    "categorical",
    colour_format = "hex",
    n = 4
  )
)

#' Create a Tabset Panel with Optional Tabs
#'
#' This function generates a `tabsetPanel` containing up to three tabs: "Chart",
#' "Table", and "Download".
#' Only non-NULL inputs will result in corresponding tabs being displayed.
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

#' Standardise internal links ---------------------------------------------
#'
#' This function generates a link to an internal tabPanel (target_link),
#' with the link text specified in "link_text"
#' The following is required in the server.R script
#'
#'   # navigation link within text --------------------------------------------
#' observeEvent(input$nav_link, {
#'   shiny::updateTabsetPanel(session, "navlistPanel", selected = input$nav_link)
#' })
#'
#' The target location could be changed to a different UI element by
#' changing the "navlistPanel" element of the server code

in_line_nav_link <- function(link_text, target_link) {
  HTML(paste0(
    "<a href='#' onclick=\"Shiny.setInputValue('nav_link', '",
    target_link,
    "', {priority: 'event'});\">",
    link_text,
    "</a>"
  ))
}
