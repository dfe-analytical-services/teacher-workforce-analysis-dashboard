# -----------------------------------------------------------------------------
# This is the server file.
#
# Use it to create interactive elements like tables, charts and text for your
# app.
#
# Anything you create in the server file won't appear in your app until you call
# it in the UI file. This server script gives examples of plots and value boxes
#
# There are many other elements you can add in too, and you can play around with
# their reactivity. The "outputs" section of the shiny cheatsheet has a few
# examples of render calls you can use:
# https://shiny.rstudio.com/images/shiny-cheatsheet.pdf
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  # Bookmarking ---------------------------------------------------------------
  # The template uses bookmarking to store input choices in the url. You can
  # exclude specific inputs (for example extra info created for a datatable
  # or plotly chart) using the list below, but it will need updating to match
  # any entries in your own dashboard's bookmarking url that you don't want
  # including.
  setBookmarkExclude(c(
    "cookies",
    "link_to_app_content_tab",
    "tabBenchmark_rows_current",
    "tabBenchmark_rows_all",
    "tabBenchmark_columns_selected",
    "tabBenchmark_cell_clicked",
    "tabBenchmark_cells_selected",
    "tabBenchmark_search",
    "tabBenchmark_rows_selected",
    "tabBenchmark_row_last_clicked",
    "tabBenchmark_state",
    "plotly_relayout-A",
    "plotly_click-A",
    "plotly_hover-A",
    "plotly_afterplot-A",
    ".clientValue-default-plotlyCrosstalkOpts"
  ))

  observe({
    # Trigger this observer every time an input changes
    reactiveValuesToList(input)
    session$doBookmark()
  })

  onBookmarked(function(url) {
    updateQueryString(url)
  })

  observe({
    if (input$navlistPanel == "Example tab 1") {
      change_window_title(
        session,
        paste0(
          site_title,
          " - ",
          input$selectPhase,
          ", ",
          input$selectArea
        )
      )
    } else {
      change_window_title(
        session,
        paste0(
          site_title,
          " - ",
          input$navlistPanel
        )
      )
    }
  })

  # Cookies logic -------------------------------------------------------------
  output$cookies_status <- dfeshiny::cookies_banner_server(
    input_cookies = shiny::reactive(input$cookies),
    parent_session = session,
    google_analytics_key = google_analytics_key
  )

  dfeshiny::cookies_panel_server(
    input_cookies = shiny::reactive(input$cookies),
    google_analytics_key = google_analytics_key
  )


  # User guide ------------------------------------------------------------------------------------------------------

  # Data sources and updates table for teacher demand and PGITT need tab

  output$data_sources_updates <- renderGovReactable({
    parent_pub_link <- sprintf(
      '<a href="%s" target="_blank" rel="noopener noreferrer">%s</a>',
      parent_publication,
      parent_pub_name
    )

    df <- tibble::tribble(
      ~Tab,
      ~`Data from`,
      ~File,
      ~`Data last updated`,
      "Teacher demand trajectories",
      parent_pub_link,
      "Supporting information data file ‘Calculation of 2026-27 postgraduate initial teacher training (PGITT) trainee need and related data’",
      "23/4/2026",
      "PGITT trainee need time series",
      parent_pub_link,
      "Featured table ‘PGITT trainee need time series by phase and subject’",
      "23/4/2026",
      "Drivers of change in PGITT trainee need",
      parent_pub_link,
      "Supporting information data file ‘Calculation of drivers of 2026-27 postgraduate ITT trainee need’",
      "23/4/2026",
      "Flow trajectories",
      parent_pub_link,
      "Supporting information data files ‘Calculation of 2026-27 postgraduate initial teacher training (PGITT) ",
      "trainee need and related data’ from this year’s publication (includes data from last year’s publication).",
      "23/4/2026"
    )

    govReactable(
      df,
      pagination = FALSE,
      searchable = FALSE,
      sortable = FALSE,
      highlight = TRUE,
      striped = FALSE,
      defaultColDef = reactable::colDef(html = TRUE)
    )
  })


  # Teacher demand trajectories tab ------------------------------------------------------------------------------------

  # Dataset: pupil/teacher timeseries ----------------------------------------------
  # Reactive data filtered by phase selection

  pt_data_filtered <- reactive({
    req(input$filter_phase)

    df <- dplyr::filter(
      pupil_teacher_numbers,
      tolower(phase) == tolower(input$filter_phase) # case insensitive matching
    )

    validate(
      need(nrow(df) > 0, paste0("No rows for phase = ", input$filter_phase)) # give error message if no data found
    )

    df
  })

  # Graph: pupil/teacher timeseries plot

  # create a function that builds the ggplot graph

  build_pupil_teacher_plot <- function(
    df,
    phase,
    for_download = FALSE,
    axis_lock = NULL
  ) {
    # dynamic labels (used inside plot function already, but keeping here is fine)
    phase_title <- if (tolower(phase) == "primary") "Primary" else "Secondary"
    left_title <- paste0(phase_title, " pupils")
    right_title <- paste0(phase_title, " teachers")

    # Let plot_pupil_teacher_timeseries() handle y scales & sec.axis,
    # including the manual lock (breaks and matching).
    p <- plot_pupil_teacher_timeseries(
      df,
      phase = phase_title,
      axis_lock = axis_lock
    )

    # Make axis text larger for downloads
    if (for_download) {
      p <- p +
        theme(
          axis.title.x = element_text(size = 34),
          axis.title.y = element_text(size = 34),
          axis.text.x = element_text(size = 32),
          axis.text.y = element_text(size = 32),
          legend.text = element_text(size = 28),

          # Set white background for downloads - prevents issue
          # with devices not rendering the transparent bg properly
          plot.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
    }
    p
  }

  # create the ggiraph plot to display on the app

  output$pupil_teacher_plot <- ggiraph::renderGirafe({
    df <- pt_data_filtered() # filtered data

    # Decide axis lock by phase
    phase_in <- input$filter_phase
    lock <- if (tolower(phase_in) == "primary") primary_lock else secondary_lock

    p <- build_pupil_teacher_plot(
      df,
      phase = phase_in,
      for_download = FALSE,
      axis_lock = lock
    )

    ggiraph::girafe(
      ggobj = p,
      width_svg = 12,
      height_svg = 6,
      options = list(
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(css = "stroke-width:2px;"),
        # Keep dashed appearance in legend keys on hover as well:
        ggiraph::opts_hover_key(
          css = "stroke-dasharray:8,6; stroke-width:1.6px;"
        ),
        ggiraph::opts_sizing(rescale = TRUE, width = 1),
        ggiraph::opts_toolbar(saveaspng = FALSE, hidden = "saveaspng")
      )
    )
  })

  # Table: pupil/teacher numbers

  output$pupil_teacher_table <- renderGovReactable({
    df <- pt_data_filtered() %>%
      dplyr::select(
        Phase = phase,
        `Academic year` = academic_year,
        `Pupil numbers (FTE)` = pupil_numbers,
        `Teacher numbers (FTE)` = teacher_numbers,
        Projection = projection
      )

    govReactable(
      df,
      pagination = FALSE,
      searchable = FALSE,
      filterable = FALSE,
      right_col = c("Pupil numbers (FTE)", "Teacher numbers (FTE)"),
      striped = FALSE,
      highlight = TRUE,
      defaultColDef = reactable::colDef(
        format = reactable::colFormat(
          separators = TRUE,
          digits = 0
        )
      )
    )
  })

  # Create download dataset for pupil teacher data

  download_table_pupil_teacher_data <- reactive({
    pt_data_filtered() %>%
      dplyr::mutate(
        `Academic year` = academic_year,
        Projection = projection
      ) %>%
      dplyr::select(
        Phase = phase,
        `Academic year`,
        `Pupil numbers (FTE)` = pupil_numbers,
        `Teacher numbers (FTE)` = teacher_numbers,
        Projection
      )
  })

  # Create download chart for pupil teacher data

  download_chart_pupil_teacher_data <- reactive({
    phase_in <- input$filter_phase
    lock <- if (tolower(phase_in) == "primary") primary_lock else secondary_lock

    build_pupil_teacher_plot(
      pt_data_filtered(),
      phase = phase_in,
      for_download = TRUE,
      axis_lock = lock
    )
  })

  # Create the download button UI

  output$download_button_ui_pupil_teacher <- renderUI({
    shinyGovstyle::download_button(
      "download_pupil_teacher", # this has to link to the id in the download action code
      "Download Chart Data",
      file_type = tolower(sub(" .*", "", input$file_type_pupil_teacher)), # this extracts file types
      file_size = NULL
    )
  })

  # Define action when user clicks download button

  output$download_pupil_teacher <- downloadHandler(
    filename = function() {
      phase_clean <- tolower(input$filter_phase)
      raw_name <- paste0(
        "twm_pupil_teacher_numbers_",
        phase_clean,
        "_",
        Sys.Date()
      )
      extension <- if (input$file_type_pupil_teacher == "CSV (Up to X.XX MB)") {
        ".csv"
      } else if (input$file_type_pupil_teacher == "XLSX (Up to X.XX MB)") {
        ".xlsx"
      } else {
        ".jpeg"
      }
      paste0(raw_name, extension)
    },
    ## Generate downloaded file
    content = function(file) {
      if (input$file_type_pupil_teacher == "CSV (Up to X.XX MB)") {
        write.csv(download_table_pupil_teacher_data(), file, row.names = FALSE)
      } else if (input$file_type_pupil_teacher == "XLSX (Up to X.XX MB)") {
        # Added a basic pop up notification as the Excel file can take time to generate
        pop_up <- showNotification("Generating download file", duration = NULL)
        openxlsx::write.xlsx(
          download_table_pupil_teacher_data(),
          file,
          colWidths = "Auto"
        )
        on.exit(removeNotification(pop_up), add = TRUE)
      } else {
        file.copy(
          ggplot2::ggsave(
            filename = tempfile(paste0(
              "twm_pupil_teacher_numbers_chart_",
              Sys.Date(),
              ".jpeg"
            )),
            plot = download_chart_pupil_teacher_data(),
            device = "jpeg",
            width = 12,
            height = 6,
            dpi = 300
          ),
          file
        )
      }
    }
  )

  ## Section for pupil/teacher number blue summary box
  ## See helper functions script for calc_pt_change_24_to_27() + build_pupil_teacher_summary() functions

  # Reactive: Calculate pupil and teacher changes between 2024/25 and 2027/28

  pt_change_24_to_27 <- reactive({
    # Ensure filtered pupil/teacher data is available before proceeding
    req(pt_data_filtered())

    # Apply helper function to calculate absolute and percentage changes
    # between 2023/24 and 2027/28
    calc_pt_change_24_to_27(pt_data_filtered())
  })

  # Reactive: Build summary text describing projected changes

  pupil_teacher_summary <- reactive({
    # Generate a human-readable summary sentence based on the calculated changes
    build_pupil_teacher_summary(pt_change_24_to_27())
  })

  # Output: Render pupil and teacher summary text in the UI

  output$pt_summary_box <- renderText({
    # Display the summary sentence in the summary box
    pupil_teacher_summary()
  })


  # PGITT trainee need time series tab ----------------------------------------------------------------------------

  # Data
  # Reactive PGITT trainee need time series data filtered by phase and/or subject selection

  pgitt_need_filtered <- reactive({
    req(input$filter_phase_pgitt_need)

    if (input$filter_phase_pgitt_need == "Primary") {
      dplyr::filter(
        pgitt_need_timeseries,
        phase == "Primary"
      )
    } else if (input$filter_phase_pgitt_need == "Secondary") {
      req(input$filter_subject_pgitt_need)

      dplyr::filter(
        pgitt_need_timeseries,
        phase == "Secondary",
        subject == input$filter_subject_pgitt_need
      )
    } else if (input$filter_phase_pgitt_need == "Total") {
      dplyr::filter(
        pgitt_need_timeseries,
        phase == "Total"
      )
    }
  })


  # Reactive title describing selected phase/subject and year range
  # for PGITT trainee need outputs

  pgitt_need_ts_title <- reactive({
    build_pgitt_need_ts_title(pgitt_need_filtered())
  })


  # Builder that can upscale text when graph is downloaded
  # Keep the look on-screen exactly as-is; only enlarge text and add white background if for_download=TRUE.
  build_pgitt_need_plot <- function(df, for_download = FALSE) {
    p <- plot_pgitt_need_timeseries(df)

    if (for_download) {
      p <- p +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(size = 30),
          axis.title.y = ggplot2::element_text(size = 30),
          axis.text.x = ggplot2::element_text(size = 28),
          axis.text.y = ggplot2::element_text(size = 28),
          plot.title = ggplot2::element_text(
            size = 40,
            face = "bold"
          ),
          # Set white background for downloads - prevents issue
          # with devices not rendering the transparent bg properly
          plot.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
    }
    p
  }

  # Graph: PGITT trainee need timeseries plot for app (interactive via ggiraph)

  output$pgitt_need_timeseries_plot <- ggiraph::renderGirafe({
    df <- pgitt_need_filtered()
    p <- build_pgitt_need_plot(df, for_download = FALSE)

    ggiraph::girafe(
      ggobj = p,
      width_svg = 12,
      height_svg = 6,
      options = list(
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(css = "stroke-width:2px;"),
        ggiraph::opts_hover_key(css = "stroke-dasharray:4,4;"),
        ggiraph::opts_sizing(rescale = TRUE, width = 1),
        ggiraph::opts_toolbar(saveaspng = FALSE, hidden = "saveaspng")
      )
    )
  })

  # Table: PGITT trainee need timeseries table for app (interactive via reactable)

  output$pgitt_need_timeseries_table <- renderGovReactable({
    df <- pgitt_need_filtered() %>%
      dplyr::select(
        `Academic year` = academic_year,
        Phase = phase,
        Subject = subject,
        `PGITT trainee need` = pgitt_trainee_need,
        `Difference in need to previous year` = difference_to_previous_year,
        `Percentage change in need to previous year` =
          percentage_difference_to_previous_year
      )

    govReactable(
      df,
      pagination = FALSE,
      searchable = FALSE,
      filterable = FALSE,
      striped = FALSE,
      highlight = TRUE,
      resizable = TRUE,
      right_col = c(
        "PGITT trainee need",
        "Difference in need to previous year",
        "Percentage change in need to previous year"
      ),
      defaultColDef = reactable::colDef(
        format = reactable::colFormat(
          separators = TRUE,
          digits = 0
        )
      )
    )
  })


  # Update table so that it has reactable caption

  output$pgitt_need_timeseries_table_ui <- renderUI({
    govReactableOutput(
      "pgitt_need_timeseries_table",
      caption = pgitt_need_ts_title(),
      caption_size = "1",
      heading_level = "h3"
    )
  })


  # Create download dataset (matches table)

  download_table_pgitt_need_data <- reactive({
    df <- pgitt_need_filtered() %>%
      dplyr::select(
        `Academic year`,
        Phase = phase,
        Subject = subject,
        `PGITT trainee need` = pgitt_trainee_need,
        `Difference in need to previous year` = difference_to_previous_year,
        `Percentage difference in need to previous year` =
          percentage_difference_to_previous_year
      )
    # Drop column subject from dataset if phase is primary
    if (nrow(df) > 0 && all(df$Phase == "Primary")) {
      df <- dplyr::select(df, -Subject)
    }
    df
  })

  # Create download chart (static ggplot for export)

  download_chart_pgitt_need_data <- reactive({
    build_pgitt_need_plot(pgitt_need_filtered(), for_download = TRUE)
  })

  # Download button UI

  output$download_button_ui_pgitt_need <- renderUI({
    shinyGovstyle::download_button(
      "download_pgitt_need", # must match downloadHandler id below
      "Download Chart Data",
      file_type = tolower(sub(" .*", "", input$file_type_pgitt_need)), # extract leading token
      file_size = NULL
    )
  })

  # Download handler (CSV/XLSX/JPEG)

  output$download_pgitt_need <- downloadHandler(
    filename = function() {
      raw_name <- paste0("twm_pgitt_need_timeseries_", Sys.Date())

      # Keep mapping identical to your earlier block for consistency
      extension <- if (input$file_type_pgitt_need == "CSV (Up to X.XX MB)") {
        ".csv"
      } else if (input$file_type_pgitt_need == "XLSX (Up to X.XX MB)") {
        ".xlsx"
      } else {
        ".jpeg"
      }
      paste0(raw_name, extension)
    },
    content = function(file) {
      if (input$file_type_pgitt_need == "CSV (Up to X.XX MB)") {
        utils::write.csv(
          download_table_pgitt_need_data(),
          file,
          row.names = FALSE
        )
      } else if (input$file_type_pgitt_need == "XLSX (Up to X.XX MB)") {
        # Optional: notify because Excel can take a little while to generate
        pop_up <- showNotification("Generating download file", duration = NULL)
        on.exit(removeNotification(pop_up), add = TRUE)
        openxlsx::write.xlsx(
          download_table_pgitt_need_data(),
          file,
          colWidths = "Auto"
        )
      } else {
        # JPEG: save static ggplot.
        tmp_file <- tempfile(paste0(
          "twm_pgitt_need_timeseries_chart_",
          Sys.Date(),
          ".jpeg"
        ))
        ggplot2::ggsave(
          filename = tmp_file,
          plot = download_chart_pgitt_need_data(),
          device = "jpeg",
          width = 10,
          height = 6,
          dpi = 300
        )
        file.copy(tmp_file, file, overwrite = TRUE)
      }
    }
  )

  # Drivers of change in PGITT trainee need tab ---------------------------------------------------------------------

  # Data
  # Reactive drivers analysis data filtered by phase and/or subject selection

  drivers_filtered <- reactive({
    df <- drivers_data %>%
      filter(phase == input$filter_phase_drivers)

    # If primary force subject = Total
    if (input$filter_phase_drivers == "Primary") {
      df <- df %>% filter(subject == "Total")
    } else {
      df <- df %>% filter(subject == input$filter_subject_drivers)
    }

    df
  })


  # Build a reactive title describing the selected school phase/subject
  # and academic year comparison for the drivers table 1 header


  drivers_title <- reactive({
    build_drivers_table_title(drivers_filtered())
  })


  # Export the reactive title as plain text for use in table 1 header

  output$drivers_title <- renderText({
    drivers_title()
  })


  # Render the text in heading_text format

  output$drivers_table_1_heading <- renderUI({
    heading_text(drivers_title(), level = 4, size = "s")
  })


  # Plot builder for drivers analysis which adds title & larger text for downloads

  build_drivers_waterfall_plot <- function(df, for_download = FALSE) {
    p <- plot_drivers_waterfall(df) # your existing ggplot builder

    if (for_download) {
      # Increase size of existing data labels
      p$layers <- lapply(p$layers, function(layer) {
        if (inherits(layer$geom, "GeomText")) {
          layer$aes_params$size <- 8
        }
        layer
      })

      # Increase text size
      p <- p +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 38, face = "bold"),
          axis.title.y = ggplot2::element_text(size = 30),
          axis.text.y = ggplot2::element_text(size = 26),

          # Tighten spacing between wrapped lines and reduce top margin
          axis.text.x = ggplot2::element_text(
            size = 28,
            lineheight = 0.35,
            margin = ggplot2::margin(t = 5)
          ),

          # Set white background for downloads - prevents issue
          # with devices not rendering the transparent bg properly
          plot.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )
    }

    p
  }

  # Graph: Drivers analysis waterfall plot for app (interactive via ggiraph)

  output$drivers_waterfall_plot <- renderGirafe({
    req(drivers_filtered())

    p <- build_drivers_waterfall_plot(drivers_filtered(), for_download = FALSE)

    ggiraph::girafe(
      ggobj = p,
      width_svg = 12,
      height_svg = 6,
      options = list(
        ggiraph::opts_sizing(rescale = TRUE),
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_toolbar(saveaspng = FALSE, hidden = "saveaspng"),
        ggiraph::opts_hover(
          css = "stroke: pink; stroke-width: 2.5px; filter: drop-shadow(0 0 4px white);"
        ),
        ggiraph::opts_hover_inv(css = "opacity:1;"), # keep others unchanged on hover
        ggiraph::opts_tooltip(
          css = "
          max-width: 260px;
          white-space: normal;
          background-color: rgba(0,0,0,0.88);
          color: #fff;
          padding: 6px 10px;
          border-radius: 6px;
          border: 0;
          box-shadow: 0 2px 6px rgba(0,0,0,0.25);
          "
        )
      )
    )
  })

  # Table 1: Drivers analysis with last year's PGITT need, this year's PGITT need, overall difference (interactive via reactable)

  output$table_pgitt_need_diff <- renderGovReactable({
    df_wide <- drivers_filtered() %>%
      dplyr::select(driver, value) %>%
      dplyr::filter(
        driver %in%
          (c(
            "2025/26 PGITT need",
            "2026/27 PGITT need",
            "Overall difference"
          ))
      ) %>%
      tidyr::pivot_wider(
        names_from = driver,
        values_from = value
      )
    right_num <- reactable::colDef(
      align = "right",
      format = reactable::colFormat(separators = TRUE)
    )

    govReactable(
      df_wide,
      pagination = FALSE,
      searchable = FALSE,
      filterable = FALSE,
      right_col = c("2025/26 PGITT need", "2026/27 PGITT need", "Overall difference"),
      highlight = TRUE,
      defaultColDef = reactable::colDef(
        format = reactable::colFormat(
          separators = TRUE,
          digits = 0
        )
      )
    )
  })

  # Table 2: Drivers analysis with drivers breakdown (interactive via reactable)

  output$table_drivers_breakdown <- renderGovReactable({
    df <- drivers_filtered() %>%
      dplyr::filter(
        !driver %in% c(
          "2025/26 PGITT need",
          "2026/27 PGITT need",
          "Overall difference"
        )
      ) %>%
      dplyr::select(
        Phase = phase,
        Subject = subject,
        Driver = driver,
        Value = value
      )

    govReactable(
      df,
      pagination = FALSE,
      searchable = FALSE,
      filterable = FALSE,
      highlight = TRUE,
      right_col = c("Value"),
      defaultColDef = reactable::colDef(
        format = reactable::colFormat(
          separators = TRUE,
          digits = 0
        )
      )
    )
  })


  # Create download dataset (matches filtered table so all data in the two tables in the app)
  # Spell out acronyms in download table
  # Capitalise column names
  # Reorder columns to match app

  download_table_drivers_data <- reactive({
    drivers_filtered() %>%
      dplyr::mutate(
        driver = dplyr::recode(
          driver,
          "2025/26 PGITT need" = "2025/26 PGITT trainee need",
          "Demand growth YOY" = "Demand growth year-on-year",
          "NTSF" = "New to state-funded sector entrants",
          "NQEs from other sources" = "Newly qualified entrants from other sources",
          "ITT-NQE conversion rate" = "Initial teacher training - newly qualified entrant conversion rate",
          "2026/27 PGITT need" = "2026/27 PGITT trainee need"
        )
      ) %>%
      dplyr::rename_with(~ tools::toTitleCase(.x)) %>%
      dplyr::select(Phase, Subject, everything())
  })

  # Create download chart (static ggplot for export with title and larger text)

  download_drivers_waterfall_plot <- reactive({
    build_drivers_waterfall_plot(drivers_filtered(), for_download = TRUE)
  })

  # Download button UI (
  output$download_button_ui_drivers <- renderUI({
    shinyGovstyle::download_button(
      "download_drivers",
      "Download Chart Data",
      file_type = tolower(sub(" .*", "", input$file_type_drivers)),
      file_size = NULL
    )
  })

  # Download handler (CSV / XLSX / JPEG) --------------------------------
  output$download_drivers <- downloadHandler(
    filename = function() {
      raw_name <- paste0("twm_drivers_", Sys.Date())
      extension <- if (input$file_type_drivers == "CSV (Up to X.XX MB)") {
        ".csv"
      } else if (input$file_type_drivers == "XLSX (Up to X.XX MB)") {
        ".xlsx"
      } else {
        ".jpeg"
      }
      paste0(raw_name, extension)
    },
    content = function(file) {
      if (input$file_type_drivers == "CSV (Up to X.XX MB)") {
        utils::write.csv(download_table_drivers_data(), file, row.names = FALSE)
      } else if (input$file_type_drivers == "XLSX (Up to X.XX MB)") {
        pop_up <- showNotification("Generating download file", duration = NULL)
        on.exit(removeNotification(pop_up), add = TRUE)
        openxlsx::write.xlsx(
          download_table_drivers_data(),
          file,
          colWidths = "Auto"
        )
      } else {
        # JPEG: save static ggplot (interactive tooltips are not present in static export)
        tmp_file <- tempfile(paste0("twm_drivers_chart_", Sys.Date(), ".jpeg"))
        ggplot2::ggsave(
          filename = tmp_file,
          plot = download_drivers_waterfall_plot(),
          device = "jpeg",
          width = 10,
          height = 6,
          dpi = 300
        )
        file.copy(tmp_file, file, overwrite = TRUE)
      }
    }
  )

  # Flow trajectories tab -----------------------------------------------------------------------------------------

  # Prevent subject = total being included from subject dropdown if secondary selected

  observeEvent(input$filter_phase_flow, {
    if (input$filter_phase_flow == "Secondary") {
      # Remove "Total" from the subject choices
      new_choices <- choices_flow_subject[choices_flow_subject != "Total"]

      updateSelectizeInput(
        session,
        "filter_subject_flow",
        choices = new_choices,
        selected = new_choices[1] # pick the first valid choice
      )
    } else {
      # Phase == Primary → always set subject = "Total"
      updateSelectizeInput(
        session,
        "filter_subject_flow",
        choices = c("Total"), # only choice
        selected = "Total"
      )
    }
  })

  # Data
  # Reactive drivers analysis data filtered by phase and/or subject selection

  flow_filtered <- reactive({
    df <- flow_data %>%
      filter(phase == input$filter_phase_flow)

    # If primary → force subject = Total
    if (input$filter_phase_flow == "Primary") {
      df <- df %>% filter(subject == "Total")
    } else {
      df <- df %>% filter(subject == input$filter_subject_flow)
    }

    # filter by selected flow type

    df <- dplyr::filter(df, type == input$filter_flow_type)
  })

  # Plot builder that can upscale text when graph is downloaded
  # Keep the look on-screen exactly as is; only enlarge axis test and add title if for_download=TRUE

  build_flow_trajectory_plot <- function(df, for_download = FALSE) {
    p <- plot_flow_trajectories(df)

    if (for_download) {
      p <- p +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(size = 30),
          axis.title.y = ggplot2::element_text(size = 30),
          axis.text.x = ggplot2::element_text(size = 28),
          axis.text.y = ggplot2::element_text(size = 28),
          legend.text = element_text(size = 28),

          # Set white background for downloads - prevents issue
          # with devices not rendering the transparent bg properly
          plot.background = ggplot2::element_rect(fill = "white", colour = NA),
          panel.background = ggplot2::element_rect(fill = "white", colour = NA)
        )

      # Add reactive title

      p <- p + ggplot2::labs(title = build_flow_traj_title(df))

      # Increase plot title text size

      p <- p +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 40,
            face = "bold"
          )
        )
    }
    p
  }

  # Graph: Flow trajectories plot for app (interactive via ggiraph)

  output$flow_timeseries_plot <- ggiraph::renderGirafe({
    df <- flow_filtered()

    # remove brief error message that appears before graph fully renders
    req(df, nrow(df) > 0)

    p <- plot_flow_trajectories(df)

    ggiraph::girafe(
      ggobj = p,
      width_svg = 12,
      height_svg = 6,
      options = list(
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(css = "stroke-width:2px;"),
        ggiraph::opts_hover_key(css = "stroke-dasharray:4,4;"),
        ggiraph::opts_sizing(rescale = TRUE, width = 1),
        ggiraph::opts_toolbar(saveaspng = FALSE, hidden = "saveaspng")
      )
    )
  })


  # Reactive title for flow trajectories based on current filters
  # Used to keep chart and table titles consistent

  flow_traj_title <- reactive({
    build_flow_traj_title(flow_filtered())
  })


  # Render the reactive title as GOV.UK–styled body text
  # uiOutput() is used in the twm_tab UI to allow this to update dynamically

  # For table

  output$flow_traj_title_table_ui <- renderUI({
    gov_text(strong(flow_traj_title()))
  })


  # Table: Flow trajectories table for app (interactive via reactable)
  # Abbrev NQEs and NTSFs to help fit in table

  output$table_flow_trajectories <- renderGovReactable({
    df <- flow_filtered() %>%
      filter(publication_year == 2026) %>%
      dplyr::mutate(
        Type = dplyr::case_when(
          type == "Newly qualified entrants" ~ "NQEs",
          type == "New to state-funded sector entrants" ~ "NTSF entrants",
          TRUE ~ type
        )
      ) %>%
      dplyr::select(
        Phase = phase,
        Subject = subject,
        `Academic year` = academic_year,
        Type,
        DUMMY = value,
        Unit = unit,
        `Historic or trajectory` = historic_or_trajectory
      )
    # conditional value formatting depending on whether leaver rates or non-leaver rates chosen
    leaver_types <- c(
      "Total leaver rate",
      "55+ leaver rate",
      "Under 55 leaver rate"
    )

    is_leaver_table <- nrow(df) > 0 && all(df$Type %in% leaver_types)


    value_formatter <- if (is_leaver_table) {
      reactable::colFormat(digits = 1, percent = TRUE)
    } else {
      reactable::colFormat(separators = TRUE, digits = 0)
    }

    govReactable(
      df,
      pagination = FALSE,
      searchable = FALSE,
      filterable = FALSE,
      highlight = TRUE,
      defaultColDef = reactable::colDef(
        format = value_formatter
      )
    )
  })


  # Create download dataset (matches table)

  download_table_flow_trajectories <- reactive({
    df <- flow_filtered() %>%
      dplyr::filter(publication_year == 2026) %>%
      dplyr::select(
        Phase = phase,
        Subject = subject,
        `Academic year` = academic_year,
        Type,
        DUMMY = value,
        Unit = unit,
        `Historic or trajectory` = historic_or_trajectory
      )

    # Drop column subject from dataset if primary selected
    if (nrow(df) > 0 && all(df$Phase == "Primary")) {
      df <- dplyr::select(df, -Subject)
    }

    # Format entrants (FTE) to 0 dp and leaver rates (%) to 1 dp
    leaver_types <- c(
      "Total leaver rate",
      "55+ leaver rate",
      "Under 55 leaver rate"
    )
    is_leaver_table <- nrow(df) > 0 && all(df$Type %in% leaver_types)

    df <- df %>%
      dplyr::mutate(
        DUMMY = if (is_leaver_table) {
          round(DUMMY * 100, 1) # 0.056 → 5.6
        } else {
          round(DUMMY, 0) # 1234 → 1234
        }
      )

    df
  })

  # Create download chart (static ggplot for export)

  download_chart_flow_trajectories <- reactive({
    build_flow_trajectory_plot(flow_filtered(), for_download = TRUE)
  })

  # Download button UI

  output$download_button_ui_flows <- renderUI({
    shinyGovstyle::download_button(
      "download_flow_data",
      "Download Chart Data",
      file_type = tolower(sub(" .*", "", input$file_type_flows)),
      file_size = NULL
    )
  })

  # Download handler(CSV/XLSX/JPEG)

  output$download_flow_data <- downloadHandler(
    filename = function() {
      raw_name <- paste0("twm_flow_trajectories_", Sys.Date())

      # Keep mapping identical to your earlier block for consistency
      extension <- if (input$file_type_flows == "CSV (Up to X.XX MB)") {
        ".csv"
      } else if (input$file_type_flows == "XLSX (Up to X.XX MB)") {
        ".xlsx"
      } else {
        ".jpeg"
      }
      paste0(raw_name, extension)
    },
    content = function(file) {
      if (input$file_type_flows == "CSV (Up to X.XX MB)") {
        utils::write.csv(
          download_table_flow_trajectories(),
          file,
          row.names = FALSE
        )
      } else if (input$file_type_flows == "XLSX (Up to X.XX MB)") {
        # Optional: notify because Excel can take a little while to generate
        pop_up <- showNotification("Generating download file", duration = NULL)
        on.exit(removeNotification(pop_up), add = TRUE)
        openxlsx::write.xlsx(
          download_table_flow_trajectories(),
          file,
          colWidths = "Auto"
        )
      } else {
        # JPEG: save static ggplot.
        tmp_file <- tempfile(paste0(
          "twm_flow_trajectories_chart_",
          Sys.Date(),
          ".jpeg"
        ))
        ggplot2::ggsave(
          filename = tmp_file,
          plot = download_chart_flow_trajectories(),
          device = "jpeg",
          width = 10,
          height = 6,
          dpi = 300
        )
        file.copy(tmp_file, file, overwrite = TRUE)
      }
    }
  )


  # Dashboard navigation --------------------------------------------------------------------------------------------

  # Adding content navigation for Teacher demand trajectories and PGITT trainee need section

  # Teacher demand trajectories link

  observeEvent(input$link_to_teacher_demand_traj, {
    updateTabsetPanel(session, "twm_tabsetpanels", selected = "Teacher demand trajectories")
  })

  # PGITT trainee need calculation link

  observeEvent(input$link_to_pgitt_need_calc, {
    updateTabsetPanel(session, "twm_tabsetpanels", selected = "PGITT trainee need calculation")
  })

  # PGITT trainee need time series link

  observeEvent(input$link_to_pgitt_need_ts, {
    updateTabsetPanel(session, "twm_tabsetpanels", selected = "PGITT trainee need time series")
  })

  # Drivers analysis link

  observeEvent(input$link_to_drivers_change, {
    updateTabsetPanel(session, "twm_tabsetpanels", selected = "Drivers of change in PGITT trainee need")
  })

  # Flow trajectories link

  observeEvent(input$link_to_flow_traj, {
    updateTabsetPanel(session, "twm_tabsetpanels", selected = "Flow trajectories")
  })

  # User guide link

  observeEvent(input$link_to_user_guide, {
    updateTabsetPanel(session, "navlistPanel", selected = "User guide")
  })

  # Support and feedback link

  observeEvent(input$link_to_support, {
    updateTabsetPanel(session, "navlistPanel", selected = "support_panel_ui")
  })


  # footer links -----------------------
  shiny::observeEvent(input$accessibility_statement, {
    shiny::updateTabsetPanel(session, "navlistPanel", selected = "a11y_panel")
  })

  shiny::observeEvent(input$use_of_cookies, {
    shiny::updateTabsetPanel(
      session,
      "navlistPanel",
      selected = "cookies_panel_ui"
    )
  })

  shiny::observeEvent(input$support_and_feedback, {
    shiny::updateTabsetPanel(
      session,
      "navlistPanel",
      selected = "support_panel_ui"
    )
  })

  shiny::observeEvent(input$privacy_notice, {
    showModal(modalDialog(
      external_link(
        "https://www.gov.uk/government/organisations/department-for-education/about/personal-information-charter", # nolint
        "Privacy notice",
        add_warning = FALSE
      ),
      easyClose = TRUE,
      footer = NULL
    ))

    # JavaScript to auto-click the link and close the modal
    shinyjs::runjs(
      "
      setTimeout(function() {
        var link = document.querySelector('.modal a');
        if (link) {
          link.click();
          setTimeout(function() {
            $('.modal').modal('hide');
          }, 20); // Extra delay to avoid any race conditions
        }
      }, 400);
    "
    )
  })

  shiny::observeEvent(input$external_link, {
    showModal(modalDialog(
      external_link(
        "https://shiny.posit.co/",
        "External Link",
        add_warning = FALSE
      ),
      easyClose = TRUE,
      footer = NULL
    ))

    # JavaScript to auto-click the link and close the modal
    shinyjs::runjs(
      "
      setTimeout(function() {
        var link = document.querySelector('.modal a');
        if (link) {
          link.click();
          setTimeout(function() {
            $('.modal').modal('hide');
          }, 20); // Extra delay to avoid any race conditions
        }
      }, 400);
    "
    )
  })
}
