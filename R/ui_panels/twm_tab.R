# # ---------------------------------------------------------------------------------------------------------------
#
# USER INTERFACE COMPONENTS FOR: *Teacher demand trajectories and PGITT trainee need* tab
# Includes layout, filters, chart output, and text boxes
#
# # ---------------------------------------------------------------------------------------------------------------

twm_tab_panel <- function() {
  tabPanel(
    "Teacher demand and PGITT need",

    # Dummy data warning
    div(
      style = "margin-bottom: 5px;",
      shinyGovstyle::warning_text(
        inputId = "warn1",
        text = "This dashboard is being developed using dummy data."
      )
    ),
    gov_main_layout(
      gov_row(

        # Header --------------------------------------------------------------
        column(
          width = 12,
          div(id = "main_col", h1("Teacher demand trajectories and PGITT trainee need"))
        ),

        # Tabs ---------------------------------------------------------------
        column(
          width = 12,
          tabsetPanel(
            id = "tabsetpanels",

            #####################################################################

            #   Introduction tab

            #####################################################################

            tabPanel(
              "Introduction",
              h2("Introduction"),

              # Intro to dashboard section
              bslib::card(
                bslib::card_header("About this section"),
                bslib::card_body(
                  p(
                    "This interactive dashboard accompanies the ",
                    strong("Teacher demand and postgraduate trainee need "),
                    "publication, available here: [link]"
                  ), # TO ADD: PUB LINK
                  p(
                    "It is designed to help users understand how PGITT trainee need is estimated using the ",
                    "Department for Education’s Teacher Workforce Model (TWM). The dashboard also highlights ",
                    "the key factors driving changes in PGITT trainee need over time and provides greater ",
                    "transparency around the model’s forecasted inflows and outflows."
                  ),
                  p(
                    "Data is available for state-funded nursery & primary, and secondary schools in England. ",
                    "Where possible, secondary data is also broken down by individual subject."
                  ),
                  p(strong("Last updated:"), "XX/XX/XXXX"), # TO ADD: PUB DATE
                  p(
                    strong("Data sources:"), "Underlying data can be found in XX, XX, and XX tables of the ", # TO ADD TABLE NAMES
                    "latest publication, and for the ", em("Flow trajectories "), "tab, in last year’s publication."
                  )
                )
              ),

              # Disclaimers text box
              bslib::card(
                bslib::card_header("Disclaimers"),
                bslib::card_body(
                  p(
                    "Figures used within the TWM may differ to the School workforce in England publication ",
                    "which includes special schools and PRUs within the state-funded schools sector."
                  ),
                  p(
                    "Leavers are counted in different academic years in the TWM and School workforce publication. ",
                    "In the TWM, teachers that are recorded as being in service in the November 2023 School Workforce Census (SWC), ",
                    "but not within the November 2024 SWC are assumed to be leavers in the 2024/25 academic year. Whereas, ",
                    "in the SWC, these leavers would be counted as leavers in the 2023/24 academic year."
                  ),
                  p(
                    "The TWM includes post-16 pupils in state-funded secondary school settings. ",
                    "Other publications may exclude these pupils, so figures may differ slightly."
                  ),
                  p(
                    "Pupil projections displayed in this dashboard differ slightly to those published in ",
                    "the National pupil projections publication due to slight coverage differences, to ensure that ",
                    "they are consistent with the methodology of the TWM. Finally, they have been adjusted ",
                    "slightly to account for the actual number of pupils captured in October 2025 via school returns."
                  )
                )
              )
            ),


            #####################################################################

            #   Demand trajectories tab

            #####################################################################

            tabPanel(
              "Teacher demand trajectories",
              h2("Historical pupil and teacher numbers, projected pupil numbers and teacher demand trajectories"),
              p(
                "This section shows historic trends in pupil and qualified teacher numbers, ",
                "alongside projections of future pupil numbers and the resulting demand for teachers as ",
                "calculated by the Teacher Workforce Model."
              ),
              bslib::layout_columns(
                col_widths = bslib::breakpoints(md = c(12, 12), lg = c(8, 4)),

                # Left column with chart/table/download
                div(
                  create_output_tabs(
                    "pupil_teacher", # base id (kept consistent with output IDs below)

                    # Mini tab 1 - chart
                    chart_output = div(
                      style = "margin-top: 1.5rem;",
                      tags$p(
                        "Pupil and teacher numbers are shown on separate y axes because they are on very different scales. ",
                        "The axes start above zero to make the trends easier to see."
                      ),
                      ggiraph::girafeOutput(
                        "pupil_teacher_plot",
                        width  = "100%",
                        height = "600px"
                      )
                    ),

                    # Mini tab 2 - table
                    table_output = reactableOutput("tablePupilTeacher"),

                    # Mini tab 3 - download

                    download_output = tagList(
                      radioButtons(
                        inputId = "file_type_pupil_teacher",
                        label = "Choose download file format",
                        choices = c(
                          "CSV (Up to X.XX MB)",
                          "XLSX (Up to X.XX MB)",
                          "JPEG (Up to XXX KB)"
                        ),
                        selected = "CSV (Up to X.XX MB)"
                      ),
                      uiOutput("download_button_ui_pupil_teacher")
                    )
                  )
                ),

                # Right column: sticky sidebar with filter, about this graph text box and reactive text box

                div(
                  class = "sticky-sidebar",
                  style = "position: sticky; top: 12px;",

                  # Filter
                  bslib::card(
                    bslib::card_header("Filter"),
                    bslib::card_body(
                      selectizeInput(
                        "filter_phase",
                        "Select a school phase:",
                        choices = choices_pupil_teacher_phase,
                        multiple = FALSE,
                        selected = "Primary",
                        options = list(
                          dropdownParent = "body"
                        )
                      )
                    )
                  ),

                  # About this graph text box
                  bslib::card(
                    bslib::card_header("About this graph"),
                    bslib::card_body(
                      p(
                        "Historical and projected pupil numbers are presented here alongside previous teacher numbers and future teacher demand. ",
                        "These figures cover state-funded nursery and primary, or state-funded secondary schools in England."
                      ),
                      p("Select a school phase to view its data and hover over the data points to see the value:"),
                      tags$ul(
                        tags$li(
                          "Pupil numbers are shown by the orange line with star markers; with projections shown as the dotted part. ",
                          "Values correspond to the left-hand axis."
                        ),
                        tags$li(
                          "Teacher numbers are shown by the blue line with dot markers, with projected demand being the dotted part. ",
                          "Values correspond to the right-hand axis."
                        )
                      )
                    )
                  )
                )
              ),

              # Reactive text box
              bslib::value_box(
                title = "",
                value = textOutput("pt_summary_box"),
                theme = bslib::value_box_theme(bg = "#1d70b8", fg = "white"),
                max_height = "115px"
              ),

              # Text boxes below graph
              bslib::card(
                style = "margin-top: 1rem;",
                bslib::card_header("Pupil numbers and teacher demand"),
                bslib::card_body(
                  p(strong("Historic trends")),
                  p(
                    "Historically, teacher demand has been influenced by changes in pupil numbers. As ",
                    "pupil numbers increase, schools respond through a combination of recruiting more ",
                    "teachers and allowing pupil–teacher ratios (PTRs) and class sizes to grow. By contrast, ",
                    "during periods of falling pupil numbers, teacher numbers and PTRs have generally fallen."
                  ),
                  p(
                    "The relationship is slightly complicated for secondary by pupil demographics shifting ",
                    "between key stage 3-5; KS3 has larger class sizes than KS4 and 5."
                  ),
                  p(strong("Pupil projections")),
                  p(
                    "Going forward, primary pupil numbers are projected to continue falling, whilst ",
                    "secondary pupil numbers are projected to start falling in the mid-2020s."
                  ),
                  p(strong("Estimating projected teacher demand")),
                  p(
                    "Teacher demand is estimated using pupil projections and assumptions about future ",
                    "PTRs. It is assumed that PTRs move in line with historic patterns: rising when pupil ",
                    "numbers rise and falling when pupil numbers fall. There is no “optimal” PTR; instead, ",
                    "the Teacher Workforce Model reflects how schools have historically responded to ",
                    "demographic change. "
                  ),
                  p(
                    "Using these assumed PTRs and projected pupil numbers, the number of teachers ",
                    "required to deliver those PTRs in future years is calculated as ‘future demand’. "
                  ),
                  p(strong("Conclusions")),
                  p(
                    "These demand trajectories represent teachers needed in service, they are not ",
                    "forecasted outcomes. Actual workforce levels will depend upon recruitment, retention, ",
                    "and movements into and out of the state funded sector (among other factors)."
                  )
                )
              )
            ),

            #####################################################################

            #   PGITT trainee need calculation tab

            #####################################################################

            tabPanel(
              "PGITT trainee need calculation",
              h2("Calculation of Postgraduate Initial Teacher Training (PGITT) trainee need"),
              p(
                "This year, the Teacher Workforce Model has estimated PGITT trainee need for 2026/27 courses. ",
                "This refers to trainees that will be recruited during 2025/26, to start training in September 2026. ",
                "They’ll be newly qualified teachers in 2027/28, entering the teaching workforce in September 2027."
              ),

              # Top box: flow chart

              bslib::card(
                bslib::card_header("PGITT trainee need 2026/27 calculation"),
                bslib::card_body(
                  tags$img(
                    src   = "pgitt_trainee_need_26_27_calculation_flow_chart.svg",
                    alt   = "Schematic of how Postgraduate Initial Teacher Training trainee need for 2026/27 is estimated by the Teacher Workforce Model",
                    style = "max-width:80%; height:auto; display:block;"
                  )
                )
              ),

              # Bottom box: flow chart text guide

              bslib::card(
                bslib::card_header("Estimating PGITT trainee need for 2026/27"),
                bslib::card_body(
                  p(
                    "This diagram shows how the Teacher Workforce Model (TWM) estimates the number of ",
                    "postgraduate initial teacher training (PGITT) trainees needed for 2026/27 for state-funded primary schools ",
                    "and for state-funded secondary schools for each secondary subject. ",
                    "The process happens in two main steps:"
                  ),
                  tags$ul(
                    tags$li("Calculating future teacher demand, and"),
                    tags$li(
                      "Estimating how many postgraduate initial trainees are needed to meet that demand ",
                      "once expected workforce changes are taken into account."
                    )
                  ),
                  p(strong("Step 1: Calculate teacher demand trajectory to 2027/28")),
                  p("Firstly, the model estimates how many teachers we need in future."),
                  tags$ul(
                    tags$li(
                      "The model assumes that the current numbers of teacher numbers from the latest school workforce census (2024/25) ",
                      "are sufficient to meet current demand."
                    ),
                    tags$li(
                      "Using projected pupil numbers, the model makes an assumption that rising pupil numbers increase teacher demand, ",
                      "with part of the demand being met by growth in pupil-to-teacher ratios. ",
                      "The opposite is true when pupil numbers are projected to fall. ",
                      "This reflects historical relationships between these factors."
                    )
                  ),
                  p("This produces a teacher demand trajectory of the number of teachers needed up to and including 2027/28."),
                  p(strong("Step 2: Calculate the number of PGITT trainees needed for 2026/27")),
                  p(
                    "Once teacher demand for 2027/28 is known, the model estimates the number of trainees ",
                    "needed in 2026/27 to meet it."
                  ),

                  # Top-level ordered list (1, 2, 3)
                  tags$ol(
                    # 1.
                    tags$li(
                      "Firstly, the model estimates the teacher entrant need in 2027/28; this is made up of two parts.",
                      # Nested ordered list (a, b)
                      tags$ol(
                        type = "a",
                        # a.
                        tags$li(
                          "The difference between teacher demand for 2027/28 and the size of the workforce in 2026/27. ",
                          "For subjects for which it is estimated there will be a supply deficit, an estimate of 2026/27 ",
                          strong("SUPPLY"),
                          " is used. This ensures that PGITT need is inflated to correct that deficit. ",
                          "For subjects for which a supply surplus is estimated, ",
                          strong("DEMAND"),
                          " is used. To do otherwise would mean PGITT need would be deflated due to the surplus."
                        ),
                        # b.
                        tags$li(
                          "Estimated losses from the workforce in 2027/28 that require replacement. ",
                          "These include both leavers (teachers leaving the sector) and losses due to teachers reducing ",
                          "their individual working hours between years."
                        )
                      )
                    ),

                    # 2.
                    tags$li(
                      "From the teacher entrant need in 2027/28, the model subtracts the teachers expected to enter in 2027/28 ",
                      "through routes other than PGITT. These include returners, teachers new to the state-funded sector, ",
                      "and newly qualified entrants from undergraduate ITT or assessment-only routes. ",
                      "The remainder is the PGITT NQE entrant need for 2027/28 in FTE."
                    ),

                    # 3.
                    tags$li(
                      "Finally, this is converted into the PGITT trainee need for 2026/27 (headcount) by applying an NQE-specific ",
                      "FTE-to-headcount conversion rate (not all NQEs will start in full-time roles) and applying ITT completion and ",
                      "post-training employment rates (to account for trainees who will not complete ITT and those who will not enter ",
                      "service in state-funded schools within four to six months)."
                    )
                  )
                )
              )
            ),

            #####################################################################

            #   PGITT trainee need time series tab

            #####################################################################

            tabPanel(
              "PGITT trainee need time series",
              h2("Postgraduate initial teacher training (PGITT) trainee need time series"),
              p(
                "PGITT trainee need by phase and subject, and how it has changed over ",
                "time from 2021/22 to 2026/27, as estimated by the Teacher Workforce Model."
              ),
              bslib::layout_columns(
                col_widths = bslib::breakpoints(md = c(12, 12), lg = c(8, 4)),

                # Left column with chart/table/download
                div(
                  create_output_tabs(
                    "pgitt_trainee_need",

                    # Mini tab 1 - chart
                    chart_output = div(
                      style = "margin-top: 3rem;",
                      ggiraph::girafeOutput(
                        "pgitt_need_timeseries_plot",
                        width  = "100%",
                        height = "600px"
                      )
                    ),

                    # Mini tab 2 - table
                    table_output = reactableOutput("tablePgittNeedTimeseries"),

                    # Mini tab 3 - download
                    download_output = tagList(
                      radioButtons(
                        inputId = "file_type_pgitt_need",
                        label = "Choose download file format",
                        choices = c(
                          "CSV (Up to X.XX MB)",
                          "XLSX (Up to X.XX MB)",
                          "JPEG (Up to XXX KB)"
                        ),
                        selected = "CSV (Up to X.XX MB)"
                      ),
                      uiOutput("download_button_ui_pgitt_need")
                    )
                  )
                ),

                # Right column: sticky sidebar with filters and about this graph text box

                div(
                  class = "sticky-sidebar",
                  style = "position: sticky; top: 12px; overflow: visible;",

                  # Filters
                  bslib::card(
                    bslib::card_header("Filters"),
                    bslib::card_body(
                      selectizeInput(
                        "filter_phase_pgitt_need", "Select a school phase:",
                        choices = choices_pgitt_need_phase,
                        multiple = FALSE,
                        selected = "Primary",
                        options = list(
                          dropdownParent = "body"
                        )
                      ),
                      conditionalPanel(
                        condition = "input.filter_phase_pgitt_need == 'Secondary'",
                        selectizeInput(
                          "filter_subject_pgitt_need", "Subject",
                          choices = choices_pgitt_need_subject,
                          multiple = FALSE,
                          selected = "Total",
                          options = list(
                            dropdownParent = "body"
                          )
                        )
                      )
                    )
                  ),

                  # About this graph text box
                  bslib::card(
                    bslib::card_header("About this graph"),
                    bslib::card_body(
                      p(
                        "This page shows PGITT trainee need for both primary and secondary ",
                        "and each secondary subject for the 2021/22 to 2026/27 academic years. "
                      ),
                      p(
                        "Select a school phase or secondary subject to view its data ",
                        "and hover over the data points to see the value. "
                      ),
                      p("Footnotes:"),
                      tags$ul(
                        tags$li("PGITT trainee need has been rounded to the nearest 5."),
                        tags$li("‘Others’ includes Child development, Citizenship, Law, Media Studies,
                                Other Social Studies, Other Technology, Politics, Psychology, Sociology,
                                and Social Sciences among others.")
                      )
                    )
                  )
                )
              ),

              # Trends in PGITT trainee need over time text box below chart
              bslib::card(
                style = "margin-top: 1rem;",
                bslib::card_header("Trends in PGITT trainee need over time"),
                bslib::card_body(
                  p(
                    "Overall PGITT need peaked in 2023/24 with the number of trainees needed falling to ",
                    "lower levels for both primary and the majority of secondary subjects."
                  ),
                  p(
                    "This lower PGITT need has been driven by pupil numbers falling more rapidly for primary, ",
                    "and growing less rapidly and levelling out for secondary respectively. "
                  ),
                  p(
                    "Additionally, teacher retention and PGITT recruitment have improved for most subjects ",
                    "making supply forecasts more favourable further helping to reduce PGITT recruitment needs."
                  ),
                  p(
                    "Changes in individual subjects have been driven by individual circumstances, with ",
                    "further information available on the ‘drivers of PGITT need changes’ tab."
                  )
                )
              )
            ),

            #####################################################################

            #   Drivers analysis tab

            #####################################################################

            tabPanel(
              "Drivers of PGITT trainee need changes",
              h2("Drivers analysis of changes in PGITT trainee need this year"),
              p(
                "Comparison of the 2025/26 and 2026/27 PGITT trainee need with ",
                "estimated driver impacts behind these changes."
              ),
              bslib::layout_columns(
                col_widths = bslib::breakpoints(md = c(12, 12), lg = c(8, 4)),

                # Left column with chart/table/download
                div(
                  create_output_tabs(
                    "drivers_analysis",

                    # Mini tab 1 - chart
                    chart_output = div(
                      style = "margin-top: 3rem;",
                      ggiraph::girafeOutput("drivers_waterfall_plot",
                        width  = "100%",
                        height = "600px"
                      )
                    ),

                    # Mini tab 2 - table
                    table_output = div(
                      h4("PGITT trainee need for 2025/26 and 2026/27"),
                      reactable::reactableOutput("table_pgitt_need_diff"),
                      tags$hr(),
                      h4("Drivers of the change"),
                      reactable::reactableOutput("table_drivers_breakdown")
                    ),

                    # Mini tab 3 - download
                    download_output = tagList(
                      radioButtons(
                        inputId = "file_type_drivers",
                        label = "Choose download file format",
                        choices = c(
                          "CSV (Up to X.XX MB)",
                          "XLSX (Up to X.XX MB)",
                          "JPEG (Up to XXX KB)"
                        ),
                        selected = "CSV (Up to X.XX MB)"
                      ),
                      uiOutput("download_button_ui_drivers")
                    )
                  )
                ),

                # Right column: sticky sidebar with filters and about this graph text box
                div(
                  class = "sticky-sidebar",
                  style = "position: sticky; top: 12px; overflow: visible;",

                  # Filters
                  bslib::card(
                    bslib::card_header("Filters"),
                    bslib::card_body(
                      selectizeInput(
                        "filter_phase_drivers", "Select a school phase:",
                        choices = choices_drivers_phase,
                        multiple = FALSE,
                        selected = "Primary",
                        options = list(
                          dropdownParent = "body"
                        )
                      ),
                      conditionalPanel(
                        condition = "input.filter_phase_drivers == 'Secondary'",
                        selectizeInput(
                          "filter_subject_drivers", "Subject",
                          choices = choices_drivers_subject,
                          multiple = FALSE,
                          selected = "Total",
                          options = list(
                            dropdownParent = "body"
                          )
                        )
                      )
                    )
                  ),

                  # About this graph box
                  bslib::card(
                    bslib::card_header("About this graph"),
                    bslib::card_body(
                      p(
                        "This graph shows last year’s PGITT need (left, dark blue bar) and this year’s PGITT need ",
                        "(right, dark blue bar) for the selected school phase and/or secondary subject. In ",
                        "between these two bars are the estimated respective impacts upon the change in PGITT ",
                        "need this year of different drivers."
                      ),
                      p("Orange bars show drivers that reduced PGITT need this year, and green bars show drivers that acted to increased it. "),
                      p(
                        "The figures are the respective impacts upon PGITT need in isolation of other drivers, and ",
                        "not the amount that the driver itself changed. For example, returners did not change by ",
                        "‘x’ this year, rather returners acted in isolation to increase/decrease PGITT need this ",
                        "year by ‘x’ trainees."
                      ),
                      p("As the graph shows, not all drivers have acted upon PGITT need this year in the same direction.")
                    )
                  )
                )
              ),

              # Text box with definitions below chart
              bslib::card(
                bslib::card_header("Definitions"),
                bslib::card_body(
                  tags$ul(
                    tags$li(
                      strong("Sector: "),
                      "State-funded primary or secondary schools in England."
                    ),
                    tags$li(
                      strong("Last year’s PGITT need: "),
                      "The 2025/26 PGITT trainee need as estimated by the Teacher Workforce Model."
                    ),
                    tags$li(
                      strong("Demand growth YOY: "),
                      "Change in teacher demand relating to pupil number change rates based on national pupil projections data. ",
                      em(
                        "Negative values suggest year-on-year (YOY) changes in teacher demand is lower due to projected pupil numbers ",
                        "falling more rapidly or growing less rapidly, acting to reduce PGITT need. ",
                        "By contrast, positive values suggest teacher demand is growing more rapidly ",
                        "YOY due to projected pupil numbers falling less rapidly or growing more rapidly."
                      )
                    ),
                    tags$li(
                      strong("Leavers: "),
                      "Teachers leaving service between years. ",
                      em(
                        "Negative numbers suggest there will be fewer leavers expected that will require replacement, ",
                        "leading to lower PGITT need, whilst a positive number suggests more leavers are expected."
                      )
                    ),
                    tags$li(
                      strong("Working hour losses: "),
                      "Losses of teachers through individual teachers reducing their  working hours between years. ",
                      em(
                        "Negative numbers suggest fewer forecasted working hour losses reducing PGITT need, ",
                        "whilst positive numbers suggest more forecasted working hour losses."
                      )
                    ),
                    tags$li(
                      strong("Returners: "),
                      "Teachers who enter service having been employed as a regular teacher in the sector previously. ",
                      em(
                        "Negative numbers suggest more returners are expected acting to reduce PGITT need, ",
                        "whilst positive numbers suggest fewer expected returners."
                      )
                    ),
                    tags$li(
                      strong("NTSF: new to state-funded sector entrants. "),
                      "Teachers who enter service having not been employed as a regular teacher ",
                      "in the sector previously and are not newly qualified ",
                      "entrants (NQEs). This  includes newly qualified teachers that defer entry into service by ",
                      "4 to 16 months. ",
                      em(
                        "Negative numbers suggest more NTSF entrants are expected leading to ",
                        "lower PGITT need, whilst positive numbers suggest fewer expected NTSFs."
                      )
                    ),
                    tags$li(
                      strong("NQEs from other sources: "),
                      "newly qualified entrants (NQE) sourced from routes other ",
                      "than PGITT courses, including undergraduate ITT, assessment only, Scotland/Wales, ",
                      "and recognition of overseas qualified status. ",
                      em(
                        "Negative numbers suggest more NQEs ",
                        "from other sources are expected acting to reduce PGITT need, whilst positive numbers suggests fewer."
                      )
                    ),
                    tags$li(
                      strong("ITT–NQE conversion rate: "),
                      "this rate is applied to reflect that not all NQEs start in full-time ",
                      "roles, trainees that do not complete ITT, and those that do not immediately enter ",
                      "employment after ITT (i.e. ITT completion and post ITT employment rates). ",
                      em(
                        "Negative numbers suggest a higher conversion rate between trainees and NQEs, acting to reduce ",
                        "PGITT need, the opposite is true for positive numbers. "
                      )
                    ),
                    tags$li(
                      strong("Under-supply adjustment: "),
                      "this accounts for potential supply shortfalls resulting from ",
                      "the two ITT cycles prior to the year for which we are setting PGITT need. These are ITT ",
                      "cycles that have already occurred but are yet to be reflected in the School Workforce ",
                      "Census. If a shortfall is estimated, the model assumes additional teachers will need to ",
                      "be recruited via PGITT to correct it. The model accounts for ITT recruitment, teacher ",
                      "retention, and other recruitment routes (e.g., returners). This holistic assessment ",
                      "means the impact of missing historical PGITT trainee need may be offset by wider ",
                      "recruitment or retention being better than expected. ",
                      em(
                        "Negative numbers reflects that the ",
                        "adjustment is smaller than last year resulting in reduced PGITT need. No bar means no ",
                        "adjustment was needed because there is no supply shortfall expected from the two prior ITT cycles",
                      )
                    ),
                    tags$li(
                      strong("This year’s PGITT need: "),
                      "The 2026/27 PGITT trainee need as estimated by the Teacher Workforce Model."
                    )
                  )
                )
              )
            ),

            #####################################################################

            #   Flow trajectories tab

            #####################################################################

            tabPanel(
              "Flow trajectories",
              h2("Flow trajectories"),
              p(
                "Trajectories of inflows and outflows to the teacher workforce as estimated ",
                "by the Teacher Workforce Model."
              ),
              bslib::layout_columns(
                col_widths = bslib::breakpoints(md = c(12, 12), lg = c(8, 4)),

                # Left column with chart/table/download
                div(
                  create_output_tabs(
                    "flow_trajectories",

                    # Mini tab 1 - chart
                    chart_output = div(
                      style = "margin-top: 3rem;",
                      ggiraph::girafeOutput(
                        outputId = "flow_timeseries_plot",
                        width = "100%",
                        height = "600px"
                      )
                    ),

                    # Mini tab 2 - table
                    table_output = div(
                      tags$p("This table shows the latest data which relates to the [Month] 2026 publication.
                             This was the latest data availability at this point in time but this data may differ to the latest School Workforce Census data."),
                      reactableOutput("table_flow_trajectories")
                    ),

                    # Mini tab 3 - download
                    download_output = tagList(
                      radioButtons(
                        inputId = "file_type_flows",
                        label = "Choose download file format",
                        choices = c(
                          "CSV (Up to X.XX MB)",
                          "XLSX (Up to X.XX MB)",
                          "JPEG (Up to XXX KB)"
                        ),
                        selected = "CSV (Up to X.XX MB)"
                      ),
                      uiOutput("download_button_ui_flows")
                    )
                  )
                ),

                # Right column: sticky sidebar with filters and about this graph
                div(
                  class = "sticky-sidebar",
                  style = "position: sticky; top: 12px; overflow: visible;",

                  # Filters
                  bslib::card(
                    bslib::card_header("Filters"),
                    bslib::card_body(
                      selectizeInput(
                        "filter_phase_flow", "Select a school phase:",
                        choices = choices_flow_phase,
                        width = "100%",
                        multiple = FALSE,
                        selected = "Primary",
                        options = list(
                          dropdownParent = "body"
                        )
                      ),
                      conditionalPanel(
                        condition = "input.filter_phase_flow == 'Secondary'",
                        selectizeInput(
                          "filter_subject_flow", "Select a secondary subject:",
                          choices = choices_flow_subject,
                          width = "100%",
                          multiple = FALSE,
                          selected = "Total",
                          options = list(
                            dropdownParent = "body"
                          )
                        )
                      ),
                      selectizeInput(
                        "filter_flow_type", "Select entrant or leaver flow type:",
                        choices = choices_flow_type,
                        width = "100%",
                        multiple = FALSE,
                        selected = "Total leaver rate",
                        options = list(
                          dropdownParent = "body"
                        )
                      )
                    )
                  ),

                  # About this graph text box
                  bslib::card(
                    bslib::card_header("About this graph"),
                    bslib::card_body(
                      p(
                        "The Teacher Workforce Model uses trajectories of inflows into and outflows from the ",
                        "teacher workforce to estimate the future PGITT trainee need by phase and secondary subject. ",
                        "These trajectories are in part based on historical data."
                      ),
                      p(
                        "Use the filter list to scroll through our trajectories for entrants and leavers ",
                        "to state-funded nursery & primary, and secondary schools."
                      )
                    )
                  )
                )
              ),

              # Text box below graph with definitions
              bslib::card(
                bslib::card_header("Definitions"),
                bslib::card_body(
                  p(strong("Leaver rates")),
                  p(
                    "Under 55 leaver rates reflect the proportion of the total teacher workforce who will leave ",
                    "service between years and are under 55 years of age."
                  ),
                  p(
                    "55+ leaver rates do the same but for those that are aged 55 or over, many of whom will ",
                    "leave service via retirement."
                  ),
                  p(
                    "The total leaver rate reflects the proportion of the total teacher workforce who will leave ",
                    "service (regardless of age). This is the sum of both under 55 leaver and 55+ leaver rates."
                  ),
                  p(
                    "The higher leaver rates are expected to be, the more PGITT newly qualified entrants ",
                    "(NQEs) may be needed to replace leavers, and the higher PGITT trainee need will be (all else being equal)."
                  ),
                  p(strong("Entrant types")),
                  p(
                    "Newly qualified entrants (NQEs) are teachers who gain qualified teacher status and will ",
                    "be recorded as entering service in the English state-funded schools sector (primary and ",
                    "secondary schools only) in the following November school workforce census. "
                  ),
                  p(
                    "New to state funded sector entrants (NTSF) are also teachers who will enter service in ",
                    "the English state-funded schools sector for the first time as recorded within the school ",
                    "workforce census excluding NQEs. This group includes newly qualified teachers that ",
                    "defer entry into the workforce by 4 to 16 months, and those that have only taught in ",
                    "other sectors, e.g. independent schools, Wales, and Scotland."
                  ),
                  p(
                    "Returners are teachers who enter service in the English state-funded schools sector, ",
                    "and are recorded within the school workforce census as having worked in the sector before."
                  ),
                  p(
                    "The more NQEs, deferrers, or returners expected, the lower the PGITT trainee need will ",
                    "be, all else being equal."
                  )
                )
              )
            )
          ) # <-- closes tabsetPanel
        ) # <-- closes column(width = 12)
      ) # <-- closes gov_row
    ) # <-- closes gov_main_layout
  ) # <-- closes tabPanel("Dashboard")
}
