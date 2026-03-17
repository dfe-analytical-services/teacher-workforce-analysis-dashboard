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
    "cookies", "link_to_app_content_tab",
    "tabBenchmark_rows_current", "tabBenchmark_rows_all",
    "tabBenchmark_columns_selected", "tabBenchmark_cell_clicked",
    "tabBenchmark_cells_selected", "tabBenchmark_search",
    "tabBenchmark_rows_selected", "tabBenchmark_row_last_clicked",
    "tabBenchmark_state",
    "plotly_relayout-A",
    "plotly_click-A", "plotly_hover-A", "plotly_afterplot-A",
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
          site_title, " - ",
          input$selectPhase, ", ",
          input$selectArea
        )
      )
    } else {
      change_window_title(
        session,
        paste0(
          site_title, " - ",
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

  # Graph: pupil/teacher timeseries plot ----------------------------------------------

  # create a function that builds the ggplot graph

  build_pupil_teacher_plot <- function(df, phase, for_download = FALSE, axis_lock = NULL) {
    # dynamic labels (used inside plot function already, but keeping here is fine)
    phase_title <- if (tolower(phase) == "primary") "Primary" else "Secondary"
    left_title <- paste0(phase_title, " pupils")
    right_title <- paste0(phase_title, " teachers")

    # -- IMPORTANT --
    # We now let plot_pupil_teacher_timeseries() handle y scales & sec.axis,
    # including the manual lock (breaks and matching).
    p <- plot_pupil_teacher_timeseries(df, phase = phase_title, axis_lock = axis_lock)

    # Make axis text larger for downloads
    if (for_download) {
      p <- p +
        theme(
          axis.title.x = element_text(size = 24),
          axis.title.y = element_text(size = 24),
          axis.text.x  = element_text(size = 22),
          axis.text.y  = element_text(size = 22)
        )
    }
    p
  }

  # axis locks for dual axis primary and secondary graphs

  primary_lock <- list(
    p0           = 3600000,
    p_max        = 4800000,
    pup_step     = 200000,
    t0           = 180000,
    t_max        = 240000,
    teach_step   = 10000,
    force_limits = TRUE
  )


  secondary_lock <- list(
    p0           = 1800000,
    p_max        = 3800000,
    pup_step     = 200000,
    t0           = 180000,
    t_max        = 380000,
    teach_step   = 20000,
    force_limits = TRUE
  )



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
        ggiraph::opts_hover_key(css = "stroke-dasharray:8,6; stroke-width:1.6px;"),
        ggiraph::opts_sizing(rescale = TRUE, width = 1),
        ggiraph::opts_toolbar(saveaspng = FALSE)
      )
    )
  })



  # Table: pupil/teacher numbers ----------------------------------------------------------

  output$tablePupilTeacher <- renderReactable({
    df <- pt_data_filtered() %>%
      dplyr::mutate(
        `Academic year` = paste0(start_year, "/", sprintf("%02d", (start_year + 1) %% 100)),
        `Pupil numbers` = pupil_numbers,
        `Teacher numbers` = teacher_numbers,
        Projection = dplyr::if_else(start_year > 2024, "Yes", "No")
      ) %>%
      dplyr::select(
        Phase = phase,
        `Academic year`,
        `Pupil numbers`,
        `Teacher numbers`,
        Projection
      )

    reactable::reactable(
      df,
      defaultPageSize = 10,
      pagination = FALSE,
      searchable = FALSE,
      filterable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        `Pupil numbers` = reactable::colDef(
          align = "right", headerStyle = list(textAlign = "right"),
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        `Teacher numbers` = reactable::colDef(
          align = "right", headerStyle = list(textAlign = "right"),
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        Phase = reactable::colDef(align = "right", headerStyle = list(textAlign = "right")),
        `Academic year` = reactable::colDef(align = "right", headerStyle = list(textAlign = "right")),
        Projection = reactable::colDef(align = "right", headerStyle = list(textAlign = "right"))
      ),
      defaultColDef = reactable::colDef(headerClass = "bar-sort-header")
    )
  })

  # Create download dataset for pupil teacher data

  download_table_pupil_teacher_data <- reactive({
    pt_data_filtered() %>%
      dplyr::mutate(
        `Academic year` = paste0(
          start_year, "/", sprintf("%02d", (start_year + 1) %% 100)
        ),
        Projection = dplyr::if_else(start_year > 2024, "Yes", "No")
      ) %>%
      dplyr::select(
        Phase = phase,
        `Academic year`,
        `Pupil numbers` = pupil_numbers,
        `Teacher numbers` = teacher_numbers,
        Projection
      )
  })

  # Create download chart for pupil teacher data


  download_chart_pupil_teacher_data <- reactive({
    build_pupil_teacher_plot(
      pt_data_filtered(),
      input$filter_phase,
      for_download = TRUE
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
      raw_name <- paste0("twm_pupil_teacher_numbers_", Sys.Date())
      extension <- if (input$file_type_pupil_teacher == "CSV (Up to X.XX MB)") {
        ".csv"
      } else if (input$file_type_pupil_teacher == "XLSX (Up to X.XX MB)") {
        ".xlsx"
      } else {
        ".jpeg"
      }
      paste0(raw_name, extension)
    },
    ## Generate downloaded file ---------------------------------------------
    content = function(file) {
      if (input$file_type_pupil_teacher == "CSV (Up to X.XX MB)") {
        write.csv(download_table_pupil_teacher_data(), file, row.names = FALSE)
      } else if (input$file_type_pupil_teacher == "XLSX (Up to X.XX MB)") {
        # Added a basic pop up notification as the Excel file can take time to generate
        pop_up <- showNotification("Generating download file", duration = NULL)
        openxlsx::write.xlsx(download_table_pupil_teacher_data(), file, colWidths = "Auto")
        on.exit(removeNotification(pop_up), add = TRUE)
      } else {
        file.copy(
          ggplot2::ggsave(
            filename = tempfile(paste0("twm_pupil_teacher_numbers_chart_", Sys.Date(), ".jpeg")),
            plot = download_chart_pupil_teacher_data(), device = "jpeg",
            width = 10, height = 6, dpi = 300
          ),
          file
        )
      }
    }
  )

  ############################ pgitt trainee need time series tab

  # Data
  # Reactive pgitt trainee need time series data filtered by phase selection

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


  output$pgitt_need_timeseries_plot <- ggiraph::renderGirafe({
    df <- pgitt_need_filtered()
    p <- plot_pgitt_need_timeseries(df)

    ggiraph::girafe(
      ggobj = p,
      width_svg = 12, height_svg = 6,
      options = list(
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(css = "stroke-width:2px;"),
        ggiraph::opts_hover_key(css = "stroke-dasharray:4,4;"),
        ggiraph::opts_sizing(rescale = TRUE, width = 1),
        ggiraph::opts_toolbar(saveaspng = FALSE)
      )
    )
  })


  output$tablePgittNeedTimeseries <- reactable::renderReactable({
    df <- pgitt_need_filtered() %>%
      mutate(
        `Academic year` = paste0(start_year, "/", sprintf("%02d", (start_year + 1) %% 100)),
        Phase = phase,
        Subject = subject,
        `PGITT trainee need` = pgitt_trainee_need,
        `Difference in need to previous year` = difference_to_previous_year,
        `Percentage difference in need to previous year` = percentage_difference_to_previous_year
      ) %>%
      dplyr::select(
        `Academic year`, Phase, Subject, `PGITT trainee need`,
        `Difference in need to previous year`, `Percentage difference in need to previous year`
      )

    # highlight if primary selected so can remove subject column from table

    is_primary_phase <- nrow(df) > 0 && all(df$Phase == "Primary")

    reactable::reactable(
      df,
      defaultPageSize = 10,
      pagination = FALSE,
      searchable = FALSE,
      filterable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      resizable = TRUE,
      columns = list(
        `Academic year` = reactable::colDef(name = "Academic<br>year", html = TRUE, align = "right", width = 120),
        Phase = reactable::colDef(align = "right", width = 120),
        Subject = reactable::colDef(show = !is_primary_phase, align = "right", width = 120),
        `PGITT trainee need` = reactable::colDef(
          align = "right",
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        `Difference in need to previous year` = reactable::colDef(
          name = "Difference in<br>need to<br>previous year",
          html = TRUE,
          align = "right",
          format = reactable::colFormat(separators = TRUE, digits = 0)
        ),
        `Percentage difference in need to previous year` = reactable::colDef(
          name = "Percentage difference<br>in need to<br>previous year",
          html = TRUE,
          align = "right",
          format = reactable::colFormat(suffix = "%", digits = 1)
        )
      ),
      defaultColDef = reactable::colDef(
        headerClass = "bar-sort-header"
        # (header wrapping handled by the CSS above)
      )
    )
  })



  drivers_filtered <- reactive({
    df <- drivers_data %>%
      filter(phase == input$filter_phase_drivers)

    # If primary → force subject = Total
    if (input$filter_phase_drivers == "Primary") {
      df <- df %>% filter(subject == "Total")
    } else {
      df <- df %>% filter(subject == input$filter_subject_drivers)
    }

    df
  })


  output$drivers_waterfall_plot <- renderGirafe({
    req(drivers_filtered())
    girafe(
      ggobj = plot_drivers_waterfall(drivers_filtered()),
      width_svg = 12,
      height_svg = 7,
      options = list(
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_toolbar(saveaspng = FALSE),
        ggiraph::opts_hover(css = "stroke: pink; stroke-width: 2.5px; filter: drop-shadow(0 0 4px white);"),
        ggiraph::opts_hover_inv(css = "opacity:1;") # keep others unchanged on hover
      )
    )
  })

  output$table_pgitt_need_diff <- reactable::renderReactable({
    df_wide <- drivers_filtered() %>%
      dplyr::select(driver, value) %>%
      dplyr::filter(driver %in% (c("Last year's need", "This year's need", "Overall difference"))) %>%
      tidyr::pivot_wider(names_from = driver, values_from = value)

    reactable::reactable(
      df_wide,
      defaultPageSize = 10,
      pagination = FALSE,
      searchable = FALSE,
      filterable = FALSE,
      striped = FALSE,
      highlight = TRUE,
      columns = list(
        `Last year's need` = reactable::colDef(
          align = "right", headerStyle = list(textAlign = "right"),
          format = reactable::colFormat(separators = TRUE)
        ),
        `This year's need` = reactable::colDef(
          align = "right", headerStyle = list(textAlign = "right"),
          format = reactable::colFormat(separators = TRUE)
        ),
        `Overall difference` = reactable::colDef(
          align = "right", headerStyle = list(textAlign = "right"),
          format = reactable::colFormat(separators = TRUE)
        )
      ),
      defaultColDef = reactable::colDef(headerClass = "bar-sort-header")
    )
  })

  output$table_drivers_breakdown <- reactable::renderReactable({
    df <- drivers_filtered() %>%
      dplyr::mutate(
        Driver  = driver,
        Value   = value,
        Phase   = phase,
        Subject = subject
      ) %>%
      dplyr::select(Phase, Subject, Driver, Value) %>%
      dplyr::filter(!(Driver %in% (c("Last year's need", "This year's need", "Overall difference"))))

    # highlight if primary selected so can remove subject column from table

    is_primary_phase <- nrow(df) > 0 && all(df$Phase == "Primary")

    reactable::reactable(
      df,
      defaultPageSize = 10,
      pagination = FALSE, # If FALSE, defaultPageSize is ignored, but it's okay to leave
      searchable = FALSE,
      filterable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        Phase = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        Subject = reactable::colDef(
          show = !is_primary_phase,
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        Driver = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        Value = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        )
      ),
      defaultColDef = reactable::colDef(headerClass = "bar-sort-header")
    )
  })

  output$table_drivers <- reactable::renderReactable({
    df <- drivers_filtered() %>%
      dplyr::mutate(
        Driver  = driver,
        Value   = value,
        Phase   = phase,
        Subject = subject
      ) %>%
      dplyr::select(Phase, Subject, Driver, Value)

    reactable::reactable(
      df,
      defaultPageSize = 10,
      pagination = FALSE, # If FALSE, defaultPageSize is ignored, but it's okay to leave
      searchable = FALSE,
      filterable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        Phase = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        Subject = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        Driver = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        Value = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        )
      ),
      defaultColDef = reactable::colDef(headerClass = "bar-sort-header")
    )
  })


  # flow trajectories --------------------------------------------------------

  # prevent subject = total being excluded from subject dropdown if secondary selected

  observeEvent(input$filter_phase_flow, {
    if (input$filter_phase_flow == "Secondary") {
      # Remove "Total" from the subject choices
      new_choices <- choices_flow_subject[choices_flow_subject != "Total"]

      updateSelectizeInput(
        session,
        "filter_subject_flow",
        choices  = new_choices,
        selected = new_choices[1] # pick the first valid choice
      )
    } else {
      # Phase == Primary → always set subject = "Total"
      updateSelectizeInput(
        session,
        "filter_subject_flow",
        choices  = c("Total"), # only choice
        selected = "Total"
      )
    }
  })

  # filter dataset based on input

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


  output$flow_timeseries_plot <- ggiraph::renderGirafe({
    df <- flow_filtered()

    # remove brief error message that appears before graph fully renders
    req(df, nrow(df) > 0)

    p <- plot_flow_trajectories(df)

    ggiraph::girafe(
      ggobj = p,
      width_svg = 12, height_svg = 6,
      options = list(
        ggiraph::opts_selection(type = "none"),
        ggiraph::opts_hover(css = "stroke-width:2px;"),
        ggiraph::opts_hover_key(css = "stroke-dasharray:4,4;"),
        ggiraph::opts_sizing(rescale = TRUE, width = 1),
        ggiraph::opts_toolbar(saveaspng = FALSE)
      )
    )
  })

  output$table_flow_trajectories <- reactable::renderReactable({
    df <- flow_filtered() %>%
      filter(version == "This year (dummy data)") %>%
      dplyr::mutate(
        Type = type,
        `DUMMY value` = value,
        Unit = unit,
        Phase = phase,
        Subject = subject,
        `Historic or trajectory` = historic_or_trajectory,
        `Academic year` = academic_year
      ) %>%
      dplyr::select(Phase, Subject, `Academic year`, Type, `DUMMY value`, Unit, `Historic or trajectory`)

    # highlight if primary selected so can remove subject column from table

    is_primary_phase <- nrow(df) > 0 && all(df$Phase == "Primary")

    # conditional value formatting depending on whether leaver rates or non-leaver rates chosen

    leaver_types <- c("Total leaver rate", "55+ leaver rate", "Under 55 leaver rate")

    is_leaver_table <- all(df$Type %in% leaver_types)

    if (is_leaver_table) {
      value_formatter <- reactable::colFormat(
        digits = 1,
        percent = TRUE # converts 0.056 -> 5.6%
      )
    } else {
      value_formatter <- reactable::colFormat(
        separators = TRUE, # adds 1,234 formatting
        digits = 0
      )
    }

    reactable::reactable(
      df,
      defaultPageSize = 10,
      pagination = FALSE, # If FALSE, defaultPageSize is ignored, but it's okay to leave
      searchable = FALSE,
      filterable = FALSE,
      striped = TRUE,
      highlight = TRUE,
      columns = list(
        Phase = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        Subject = reactable::colDef(
          show = !is_primary_phase, # hide when phase is primary
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        `Academic year` = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        Type = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        ),
        `DUMMY value` = reactable::colDef(
          align = "right",
          format = value_formatter,
          headerStyle = list(textAlign = "right")
        ),
        Unit = reactable::colDef(
          show = !is_leaver_table,
          align = "right",
          format = value_formatter,
          headerStyle = list(textAlign = "right")
        ),
        `Historic or trajectory` = reactable::colDef(
          align = "right",
          headerStyle = list(textAlign = "right")
        )
      ),
      defaultColDef = reactable::colDef(headerClass = "bar-sort-header")
    )
  })

  # Link in the user guide panel back to the main panel -----------------------
  observeEvent(input$link_to_app_content_tab, {
    updateTabsetPanel(session, "navlistPanel", selected = "Example tab 1")
  })

  # Download the underlying data button --------------------------------------
  output$download_data <- downloadHandler(
    filename = "shiny_template_underlying_data.csv",
    content = function(file) {
      write.csv(df_revbal, file)
    }
  )

  # Wrap a plot with a larger spinner
  with_gov_spinner <- function(ui_element, spinner_type = 6, size = 1, color = "#1d70b8") {
    shinycssloaders::withSpinner(
      ui_element,
      type = spinner_type,
      color = color,
      size = size,
      proxy.height = paste0(250 * size, "px")
    )
  }

  # navigation link within text --------------------------------------------
  observeEvent(input$nav_link, {
    shiny::updateTabsetPanel(session, "navlistPanel", selected = input$nav_link)
  })

  # Dynamic label showing custom selections -----------------------------------
  output$dropdown_label <- renderText({
    paste0("Current selections: ", input$selectPhase, ", ", input$selectArea)
  })

  # footer links -----------------------
  shiny::observeEvent(input$accessibility_statement, {
    shiny::updateTabsetPanel(session, "navlistPanel", selected = "a11y_panel")
  })

  shiny::observeEvent(input$use_of_cookies, {
    shiny::updateTabsetPanel(session, "navlistPanel", selected = "cookies_panel_ui")
  })

  shiny::observeEvent(input$support_and_feedback, {
    shiny::updateTabsetPanel(session, "navlistPanel", selected = "support_panel_ui")
  })

  shiny::observeEvent(input$privacy_notice, {
    showModal(modalDialog(
      external_link("https://www.gov.uk/government/organisations/department-for-education/about/personal-information-charter", # nolint
        "Privacy notice",
        add_warning = FALSE
      ),
      easyClose = TRUE,
      footer = NULL
    ))

    # JavaScript to auto-click the link and close the modal
    shinyjs::runjs("
      setTimeout(function() {
        var link = document.querySelector('.modal a');
        if (link) {
          link.click();
          setTimeout(function() {
            $('.modal').modal('hide');
          }, 20); // Extra delay to avoid any race conditions
        }
      }, 400);
    ")
  })

  shiny::observeEvent(input$external_link, {
    showModal(modalDialog(
      external_link("https://shiny.posit.co/",
        "External Link",
        add_warning = FALSE
      ),
      easyClose = TRUE,
      footer = NULL
    ))

    # JavaScript to auto-click the link and close the modal
    shinyjs::runjs("
      setTimeout(function() {
        var link = document.querySelector('.modal a');
        if (link) {
          link.click();
          setTimeout(function() {
            $('.modal').modal('hide');
          }, 20); // Extra delay to avoid any race conditions
        }
      }, 400);
    ")
  })

  # Stop app ------------------------------------------------------------------
  session$onSessionEnded(function() {
    stopApp()
  })
}
