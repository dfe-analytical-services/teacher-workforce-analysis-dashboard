library(shinytest2)


all_inputs <- c(
  "tabsetpanels",

  # Teacher demand trajectories
  "file_type_pupil_teacher",
  "filter_phase",
  "main_tabs_pupil_teacher",

  # PGITT trainee need time series
  "file_type_pgitt_need",
  "filter_phase_pgitt_need",
  "filter_subject_pgitt_need",
  "main_tabs_pgitt_trainee_need",

  # Drivers of PGITT trainee need changes
  "file_type_drivers",
  "filter_phase_drivers",
  "filter_subject_drivers",
  "main_tabs_drivers_analysis",

  # Flow trajectories
  "file_type_flows",
  "filter_phase_flow",
  "filter_subject_flow",
  "filter_flow_type",
  "main_tabs_flow_trajectories"
)


all_outputs <- c(
  # Teacher demand trajectories tab
  "tablePupilTeacher",
  "download_button_ui_pupil_teacher",
  "pt_summary_box",

  # PGITT trainee need time series tab
  "tablePgittNeedTimeseries",
  "download_button_ui_pgitt_need",

  # Drivers tab
  "table_pgitt_need_diff",
  "table_drivers_breakdown",
  "download_button_ui_drivers",

  # Flow trajectories tab
  "table_flow_trajectories",
  "download_button_ui_flows"
)


test_that("{shinytest2} recording: initial_state", {
  app <- AppDriver$new(
    name = "initial_state",
    load_timeout = 320 * 1000,
    timeout = 320 * 1000,
    wait = TRUE
  )
  app$click("cookies_banner-cookies_reject")
  app$set_inputs(
    navlistPanel = "Teacher demand and PGITT need",
    tabsetpanels = "Teacher demand trajectories",
    filter_phase = "Secondary",
    main_tabs_pupil_teacher = "Table"
  )
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(
    filter_phase = "Primary",
    main_tabs_pupil_teacher = "Chart"
  )
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(
    main_tabs_pupil_teacher = "Download",
    file_type_pupil_teacher = "XLSX (Up to X.XX MB)"
  )
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(file_type_pupil_teacher = "JPEG (Up to XXX KB)")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(tabsetpanels = "PGITT trainee need calculation")
  app$set_inputs(tabsetpanels = "PGITT trainee need time series")
  app$set_inputs(filter_phase_pgitt_need = "Secondary")
  app$set_inputs(filter_subject_pgitt_need = "Computing")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(main_tabs_pgitt_trainee_need = "Table")
  app$set_inputs(filter_phase_pgitt_need = "Primary")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(main_tabs_pgitt_trainee_need = "Download")
  app$set_inputs(file_type_pgitt_need = "XLSX (Up to X.XX MB)")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(tabsetpanels = "Drivers of PGITT trainee need changes")
  app$set_inputs(filter_phase_drivers = "Secondary")
  app$set_inputs(filter_subject_drivers = "Physical Education")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(main_tabs_drivers_analysis = "Table")
  app$set_inputs(filter_subject_drivers = "Religious Education")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(main_tabs_drivers_analysis = "Download")
  app$set_inputs(file_type_drivers = "XLSX (Up to X.XX MB)")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(file_type_drivers = "JPEG (Up to XXX KB)")
  app$set_inputs(main_tabs_drivers_analysis = "Chart")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(main_tabs_drivers_analysis = "Table")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(main_tabs_drivers_analysis = "Download")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(tabsetpanels = "Flow trajectories")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(filter_phase_flow = "Secondary")
  app$set_inputs(filter_subject_flow = "Modern Foreign Languages")
  app$set_inputs(filter_flow_type = "Returners")
  app$set_inputs(filter_subject_flow = "Physics")
  app$set_inputs(filter_flow_type = "Total leaver rate")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(main_tabs_flow_trajectories = "Table")
  app$set_inputs(filter_phase_flow = "Primary")
  app$set_inputs(filter_flow_type = "Returners")
  app$set_inputs(main_tabs_flow_trajectories = "Download")
  app$set_inputs(file_type_flows = "XLSX (Up to X.XX MB)")
  app$set_inputs(file_type_flows = "JPEG (Up to XXX KB)")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(main_tabs_flow_trajectories = "Table")
  app$set_inputs(filter_phase_flow = "Secondary")
  app$set_inputs(filter_subject_flow = "Classics")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(main_tabs_flow_trajectories = "Chart")
  app$set_inputs(filter_subject_flow = "Physics")
  app$expect_values(input = all_inputs, output = all_outputs)

  app$set_inputs(navlistPanel = "a11y_panel")
  app$set_inputs(navlistPanel = "cookies_panel_ui")
  app$set_inputs(navlistPanel = "support_panel_ui")
  app$expect_values(input = all_inputs, output = all_outputs)
})
