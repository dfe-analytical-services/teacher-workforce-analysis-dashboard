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
          div(
            id = "main_col",
            h1("Teacher demand trajectories and PGITT trainee need")
          )
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
                    a(
                      "Teacher demand and postgraduate trainee need publication.",
                      href = "https://explore-education-statistics.service.gov.uk/find-statistics/teacher-demand-and-postgraduate-trainee-need/2026-27",
                      target = "_blank"
                    )
                  ),
                  p(
                    "It is designed to help users understand how teacher demand trajectories and PGITT ",
                    "trainee need is estimated using the Department for Education’s teacher workforce ",
                    "model (TWM). The dashboard also highlights the key factors driving changes in PGITT ",
                    "trainee need and provides greater transparency around the model’s forecasted inflows and outflows."
                  ),
                  p(
                    "Data is available for state-funded primary (including maintained nurseries ",
                    "attached to schools), and secondary schools in England. Where possible, secondary ",
                    "data is also broken down by individual subject.",
                    "Please see the user guide for details of data sources."
                  ),
                  p(strong("Data last updated:"), "XX/XX/XXXX") # TO ADD: PUB DATE
                )
              ),

              # Disclaimers text box
              bslib::card(
                bslib::card_header("Disclaimers and caveats"),
                bslib::card_body(
                  tags$ul(
                    tags$li(
                      "The inputs to the teacher workforce model are the most timely data available as ",
                      "of February 2026. It has been presented within this part of the dashboard as it ",
                      "was data used to estimate 2026/27 PGITT need.",
                      tags$ul(
                        tags$li(
                          "2026/27 PGITT need will not be retrospectively updated in future. "
                        ),
                        tags$li(
                          "As a consequence, this part of the dashboard will not be updated after publication."
                        ),
                        tags$li(
                          "Therefore, these data may differ slightly to that in subsequent updates to ",
                          "the school workforce census (SWC), ITT census, and ITT performance ",
                          "profiles data."
                        )
                      )
                    ),
                    tags$li(
                      "Note – next year PGITT need will be calculated for 2027/28 and will reflect any data updates."
                    ),
                    tags$li(
                      "Figures used within the TWM may differ to the SWC publication (school workforce in England) ",
                      "which includes special schools and PRUs within the state-funded schools sector."
                    ),
                    tags$li(
                      "This publication uses a different naming convention to the SWC for teachers leaving service.",
                      tags$ul(
                        tags$li(
                          "A teacher that leaves service between the November 2023 and November ",
                          "2024 SWC is classified as being a 2023/24 leaver in the School Workforce ",
                          "publication. By contrast, in the TWM, such leavers are classified as being ",
                          "leavers in the 2024/25 academic year."
                        ),
                        tags$li(
                          "This approach is taken within the TWM for modelling purposes to ensure ",
                          "leavers align with entrants coming in to replace them."
                        ),
                        tags$li(
                          "I.e. teachers leaving at the very end of the 2023/24 academic year would ",
                          "largely be replaced by entrants entering service in September 2024."
                        )
                      )
                    ),
                    tags$li(
                      "Finally, pupil projections in the dashboard differ slightly from those in the ",
                      "national pupil projections release due to:",
                      tags$ul(
                        tags$li(
                          "Coverage differences which are needed to align with the TWM methodology ",
                          "(e.g. post-16 pupils in secondary schools are included within the TWM), and"
                        ),
                        tags$li(
                          "Adjustments reflecting the actual number of pupils captured in October ",
                          "2025 school returns."
                        )
                      )
                    )
                  )
                )
              )
            ),

            #####################################################################

            #   Demand trajectories tab

            #####################################################################

            tabPanel(
              "Teacher demand trajectories",
              h2(
                "Historical pupil and teacher numbers, projected pupil numbers and teacher demand trajectories"
              ),
              p(
                "This section shows historic trends in pupil and teacher numbers (including ",
                "unqualified teachers), alongside projections of both future pupil numbers and the ",
                "resulting demand for teachers as calculated by the teacher workforce model. ",
                "All numbers are in full time equivalent (FTE)."
              ),
              bslib::layout_columns(
                col_widths = bslib::breakpoints(md = c(12, 12), lg = c(8, 4)),

                # Left column with chart/table/download
                create_output_tabs(
                  "pupil_teacher", # base id (kept consistent with output IDs below)

                  # Mini tab 1 - chart
                  chart_output = div(
                    style = "margin-top: 1.5rem;",
                    tags$p(
                      "Pupil and teacher numbers are shown on separate y axes ",
                      "because they are on very different scales. ",
                      "The axes start above zero to make the trends easier to see."
                    ),
                    ggiraph::girafeOutput(
                      "pupil_teacher_plot",
                      width = "100%",
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
                ),

                # Right column: sidebar with filter, about this graph text box and reactive text box

                div(
                  class = "sidebar",
                  style = "top: 12px;",

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
                        "Historical and projected pupil numbers are presented here alongside historic teacher ",
                        "numbers and future teacher demand. These figures cover state-funded primary and ",
                        "state-funded secondary schools in England."
                      ),
                      p(
                        "Select a school phase to view its data and hover over the data points to see the value."
                      ),
                      tags$ul(
                        tags$li(HTML(
                          'Pupil numbers are shown by the <span style="color:#F46A25; font-weight: 600;">orange</span> line
                          with star markers; with projections shown as the dotted part.
                          Values correspond to the left hand axis.'
                        )),
                        tags$li(HTML(
                          'Teacher numbers are shown by the <span style="color:#12436D; font-weight: 600;">blue</span> line
                          with dot markers, with projected demand being the dotted part. Values correspond to the right hand axis.'
                        ))
                      )
                    )
                  )
                )
              ),

              # Reactive text box
              bslib::value_box(
                title = NULL,
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
                    "between key stage 3-5; KS3 has larger class sizes than KS4 and KS5."
                  ),
                  p(strong("Pupil projections")),
                  p(
                    "Going forward, primary pupil numbers are projected to continue falling, whilst ",
                    "secondary pupil numbers are projected to start falling in the mid-2020s."
                  ),
                  p(
                    em(
                      "Please note, these pupil projections differ slightly to those published in the national ",
                      "pupil projections due to slight coverage differences, to ensure that they are consistent ",
                      "with the methodology of the TWM. In particular, they include post-16 pupils in ",
                      "secondary schools. Finally, they have been adjusted slightly to account for the actual ",
                      "number of pupils captured in October 2025 via school returns."
                    )
                  ),
                  p(strong("Estimating projected teacher demand")),
                  p(
                    "Teacher demand is estimated using pupil projections and assumptions about future ",
                    "PTRs. It is assumed that PTRs move in line with historic patterns: rising when pupil ",
                    "numbers rise and falling when pupil numbers fall. There is no “optimal” PTR; instead, ",
                    "the TWM reflects how schools have historically responded to demographic change. "
                  ),
                  p(
                    "Using these assumed PTRs and projected pupil numbers, the number of teachers ",
                    "required to deliver those PTRs in future years is calculated as ‘future teacher demand’."
                  ),
                  p(
                    "These demand trajectories are not forecasted outcomes, actual workforce levels will ",
                    "depend upon the balance of movements into and out of the state-funded sector (among ",
                    "other factors)."
                  )
                )
              )
            ),

            #####################################################################

            #   PGITT trainee need calculation tab

            #####################################################################

            tabPanel(
              "PGITT trainee need calculation",
              h2(
                "Calculation of postgraduate initial teacher training (PGITT) trainee need"
              ),
              p(
                "This year, the teacher workforce model has estimated PGITT trainee need ",
                "for 2026/27 courses. This refers to trainees that will be recruited during 2025/26, to start ",
                "training in September 2026, to become newly qualified teachers in 2027/28, entering ",
                "the teaching workforce in September 2027."
              ),

              # Top box: flow chart

              bslib::card(
                bslib::card_header("Estimating PGITT trainee need for 2026/27"),
                bslib::card_body(
                  tags$img(
                    src = "pgitt_trainee_need_26_27_calculation_flow_chart.svg",
                    alt = "Schematic of how teacher demand trajectories and postgraduate
                    initial teacher training trainee need for 2026/27 is estimated by the teacher workforce model",
                    style = "max-width:80%; height:auto; display:block;"
                  )
                )
              ),

              # Bottom box: flow chart text guide

              bslib::card(
                bslib::card_header("Estimating PGITT trainee need for 2026/27"),
                bslib::card_body(
                  p(
                    "This diagram shows how the teacher workforce model (TWM) estimates the number of ",
                    "postgraduate initial teacher training (PGITT) trainees needed for 2026/27 for state-funded primary schools ",
                    "and for state-funded secondary schools for each secondary subject. ",
                    "The process happens in two main steps:"
                  ),
                  tags$ul(
                    tags$li("Calculating future teacher demand, and"),
                    tags$li(
                      "Estimating how many postgraduate initial trainees are needed to meet that demand ",
                      "once other expected workforce changes are taken into account."
                    )
                  ),
                  p(strong(
                    "Step 1: Calculate teacher demand trajectory to 2027/28"
                  )),
                  p(
                    "Firstly, the model estimates how many teachers are needed in future."
                  ),
                  tags$ul(
                    tags$li(
                      "The model assumes that the current numbers of teacher numbers from the latest school workforce census (2024/25) ",
                      "are sufficient to meet current pupil demand."
                    ),
                    tags$li(
                      "Using projected pupil numbers, the model makes an assumption that rising pupil numbers increase teacher demand, ",
                      "with part of the demand being met by growth in pupil to teacher ratios. ",
                      "The opposite is true when pupil numbers are projected to fall. ",
                      "This reflects historical relationships between these factors."
                    )
                  ),
                  p(
                    "This produces a teacher demand trajectory of the number of teachers needed up to and including 2027/28."
                  ),
                  p(strong(
                    "Step 2: Calculate the number of PGITT trainees needed for 2026/27"
                  )),
                  p(
                    "Using estimated teacher demand for 2027/28, the model estimates the number of trainees ",
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
                          "The difference between teacher demand for 2027/28 and the size of the ",
                          "teacher workforce in 2026/27. For subjects for which it is estimated there ",
                          "will be a 2026/27 teacher supply deficit, an estimate of ",
                          strong("SUPPLY"),
                          " is used as the basis for the 2026/27 teacher workforce for that subject. This ",
                          "ensures that PGITT need is inflated to correct that deficit. For subjects for ",
                          "which a 2026/27 teacher supply surplus is estimated, 2026/27 ",
                          strong("DEMAND"),
                          " is used as the basis for the 2026/27 teacher workforce for that subject. To",
                          "do otherwise would mean PGITT need would be deflated due to the surplus."
                        ),
                        # b.
                        tags$li(
                          "Estimated losses from the workforce in 2027/28 that require replacement. ",
                          "These include both leavers (teachers leaving the state-funded sector) and ",
                          "losses due to teachers reducing their individual working hours between years."
                        )
                      )
                    ),

                    # 2.
                    tags$li(
                      "From the teacher entrant need in 2027/28, the model subtracts the teachers ",
                      "expected to enter in 2027/28 through routes other than PGITT. These include ",
                      "returners, teachers new to the state-funded sector, and newly qualified entrants ",
                      "from undergraduate ITT or assessment-only routes. The remainder is the PGITT ",
                      "newly qualified entrant (NQE) need for 2027/28 in FTE."
                    ),

                    # 3.
                    tags$li(
                      "Finally, this is converted into the PGITT trainee need for 2026/27 (headcount) by ",
                      "applying an NQE-specific FTE-to-headcount conversion rate (not all NQEs will ",
                      "start in full-time roles) and applying ITT completion and post-training ",
                      "employment rates (to account for trainees who will not complete ITT and those ",
                      "who will not enter service in state-funded schools within four to six months)."
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
              h2(
                "Postgraduate initial teacher training (PGITT) trainee need time series"
              ),
              p(
                "PGITT trainee need by phase and subject, and how it has changed over ",
                "time from 2021/22 to 2026/27, as estimated by the teacher workforce model."
              ),
              bslib::layout_columns(
                col_widths = bslib::breakpoints(md = c(12, 12), lg = c(8, 4)),

                # Left column with chart/table/download
                create_output_tabs(
                  "pgitt_trainee_need",

                  # Mini tab 1 - chart
                  chart_output = div(
                    style = "margin-top: 3rem;",
                    ggiraph::girafeOutput(
                      "pgitt_need_timeseries_plot",
                      width = "100%",
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
                ),

                # Right column: sidebar with filters and about this graph text box

                div(
                  class = "sidebar",
                  style = "top: 12px; overflow: visible;",

                  # Filters
                  bslib::card(
                    bslib::card_header("Filters"),
                    bslib::card_body(
                      selectizeInput(
                        "filter_phase_pgitt_need",
                        "Select a school phase:",
                        choices = choices_pgitt_need_phase,
                        multiple = FALSE,
                        selected = "Total",
                        options = list(
                          dropdownParent = "body"
                        )
                      ),
                      conditionalPanel(
                        condition = "input.filter_phase_pgitt_need == 'Secondary'",
                        selectizeInput(
                          "filter_subject_pgitt_need",
                          "Subject",
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
                        "This page shows PGITT trainee need for both primary and secondary and each ",
                        "secondary subject for the 2021/22 to 2026/27 academic years."
                      ),
                      p(
                        "Select a school phase or secondary subject to view its data and hover over the columns ",
                        "to see the value."
                      ),
                      p("Footnotes:"),
                      tags$ul(
                        tags$li(
                          "PGITT trainee need has been rounded to the nearest 5."
                        ),
                        tags$li(
                          "‘Others’ includes Child Development, Citizenship, Law, Media Studies,
                                Other Social Studies, Other Technology, Politics, Psychology, Sociology,
                                and Social Sciences among others."
                        )
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
                    "Overall PGITT trainee need (primary and secondary combined) peaked in 2023/24 with ",
                    "the number of trainees needed falling to lower levels for both primary and the majority ",
                    "of secondary subjects in subsequent years."
                  ),
                  p(
                    "This lower PGITT need has been driven by pupil numbers falling more rapidly for primary, ",
                    "and growing less rapidly and levelling out for secondary."
                  ),
                  p(
                    "Additionally, both teacher retention forecasts and PGITT recruitment have become ",
                    "more favourable for most subjects, making supply forecasts more favourable and further ",
                    "helping to reduce PGITT recruitment needs."
                  ),
                  p(
                    "Changes in individual subjects have been driven by individual circumstances, with ",
                    "further information available on the ",
                    em("'Drivers of change in PGITT trainee need'"),
                    " tab."
                  )
                )
              )
            ),

            #####################################################################

            #   Drivers analysis tab

            #####################################################################

            tabPanel(
              "Drivers of change in PGITT trainee need",
              h2("Drivers of changes in PGITT trainee need this year"),
              p(
                "Comparison of the 2025/26 and 2026/27 PGITT trainee need and the ",
                "estimated drivers behind these changes."
              ),
              bslib::layout_columns(
                col_widths = bslib::breakpoints(md = c(12, 12), lg = c(8, 4)),

                # Left column with chart/table/download
                create_output_tabs(
                  "drivers_analysis",

                  # Mini tab 1 - chart
                  chart_output = div(
                    style = "margin-top: 3rem;",
                    ggiraph::girafeOutput(
                      "drivers_waterfall_plot",
                      width = NULL,
                      height = NULL
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
                ),

                # Right column: sidebar with filters and about this graph text box
                div(
                  class = "sidebar",
                  style = "top: 12px; overflow: visible;",

                  # Filters
                  bslib::card(
                    bslib::card_header("Filters"),
                    bslib::card_body(
                      selectizeInput(
                        "filter_phase_drivers",
                        "Select a school phase:",
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
                          "filter_subject_drivers",
                          "Subject",
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
                        "between these two bars are the estimated respective drivers behind the change in PGITT ",
                        "need between the two years."
                      ),
                      p(
                        "All numbers are unrounded as they relate to figures used to calculate the PGITT need ",
                        "before rounding is applied. For this reason, the PGITT need figures quoted may differ ",
                        "slightly to those published elsewhere."
                      ),
                      p(
                        "Orange bars show drivers that acted to reduce PGITT need this year, and green bars ",
                        "show drivers that acted to increase it."
                      ),
                      p(
                        "The scale of each driver is its estimated impact upon PGITT need, and not the amount ",
                        "that the driver itself changed. For example, returners did not increase/fall by ‘x’ returners ",
                        "this year, rather returners acted to increase/decrease PGITT need this year by ‘x’ PGITT ",
                        "trainees this year."
                      ),
                      p(
                        "As the graph shows, not all drivers have acted upon PGITT need this year in the same direction."
                      )
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
                      strong("2025/26 PGITT need: "),
                      "Last year’s PGITT trainee need as estimated by the teacher workforce model."
                    ),
                    tags$li(
                      strong("Demand growth YOY: "),
                      "Change in teacher demand YOY relating to pupil number change rates based on national pupil projections. ",
                      em(
                        "This year, negative values suggest teacher demand is falling more rapidly year-on-year (YOY) reducing PGITT need. ",
                        "This is a consequence of projected pupil numbers falling more rapidly than they were in last year’s projections."
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
                      "Losses of teachers through individual teachers reducing their working hours between years. ",
                      em(
                        "Negative numbers suggest fewer forecasted working hour losses that will require replacement ",
                        "leading to lower PGITT need, whilst positive numbers suggest more forecasted working hour losses."
                      )
                    ),
                    tags$li(
                      strong("Returners: "),
                      "Teachers who enter service having been employed as a regular teacher in the state-funded sector previously. ",
                      em(
                        "Negative numbers suggest more returners are expected acting to reduce PGITT need, ",
                        "whilst positive numbers suggest fewer expected returners."
                      )
                    ),
                    tags$li(
                      strong("NTSF: new to state-funded sector entrants. "),
                      "Teachers who enter service having not been employed as a regular teacher in the sector ",
                      "previously and are not newly qualified entrants (NQEs). This includes newly qualified teachers ",
                      "that defer entry into service by 4 to 16 months. ",
                      em(
                        "Negative numbers suggest more NTSF entrants are expected leading to ",
                        "lower PGITT need, whilst positive numbers suggest fewer expected NTSFs. "
                      )
                    ),
                    tags$li(
                      strong("NQEs from other sources: "),
                      "newly qualified entrants (NQE) sourced from routes other ",
                      "than PGITT courses, including undergraduate ITT, assessment only, Scotland/Wales, ",
                      "and recognition of overseas qualified status. ",
                      em(
                        "Negative numbers suggest more NQEs from other sources are expected, ",
                        "acting to reduce PGITT need, whilst positive numbers suggest fewer are expected."
                      )
                    ),
                    tags$li(
                      strong("ITT–NQE conversion rate: "),
                      "this rate is applied to reflect that not all NQEs start in full-time roles, ",
                      "there are some trainees that do not complete ITT, ",
                      "and there are those that do not immediately enter employment after ITT ",
                      "(i.e. ITT completion and post ITT employment rates).",
                      em(
                        "Negative numbers suggest a higher conversion rate between trainees and NQEs, ",
                        "acting to reduce PGITT need, the opposite is true for positive numbers."
                      )
                    ),
                    tags$li(
                      strong("Under-supply adjustment: "),
                      "this accounts for potential supply shortfalls between 2024/25 (the most recent SWC) and 2026/27, ",
                      "reflecting recruitment impacts from the two ITT cycles prior to 2026/27. ",
                      "These are ITT cycles that have already occurred but are yet to be reflected in the school workforce census. ",
                      "If a shortfall is estimated, the model assumes additional teachers will need to be recruited via PGITT to correct it. ",
                      "The model accounts for ITT recruitment, teacher retention, and other recruitment routes (e.g., returners). ",
                      "This holistic assessment means the impact of missing historical PGITT trainee need may be offset ",
                      "by wider recruitment or retention being better than expected. ",
                      em(
                        "Negative numbers reflect that the adjustment is smaller than last year ",
                        "resulting in reduced PGITT need. No bar means no adjustment was needed this year or ",
                        "last because there was no supply shortfall expected from the two prior ITT cycles.",
                      )
                    ),
                    tags$li(
                      strong("2026/27 PGITT need: "),
                      "This year’s PGITT trainee need as estimated by the teacher workforce model."
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
                "by the teacher workforce model."
              ),
              bslib::layout_columns(
                col_widths = bslib::breakpoints(md = c(12, 12), lg = c(8, 4)),

                # Left column with chart/table/download
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
                    tags$p(
                      "This table shows the latest data which relates to the April 2026 publication.
                           This was the latest data availability at this point in time but this data may differ to the latest school workforce census data."
                    ),
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
                ),

                # Right column:  sidebar with filters and about this graph
                div(
                  class = "sidebar",
                  style = "top: 12px; overflow: visible;",

                  # Filters
                  bslib::card(
                    bslib::card_header("Filters"),
                    bslib::card_body(
                      selectizeInput(
                        "filter_phase_flow",
                        "Select a school phase:",
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
                          "filter_subject_flow",
                          "Select a secondary subject:",
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
                        "filter_flow_type",
                        "Select entrant or leaver flow type:",
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
                        "The teacher workforce model uses trajectories of inflows into and outflows from the ",
                        "teacher workforce to estimate future PGITT trainee need by phase and secondary ",
                        "subject. These trajectories are in part based on historical data. "
                      ),
                      p(
                        "Use the filter list to scroll through trajectories for teacher entrants and leavers to ",
                        "state-funded primary and secondary schools."
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
                    "service each year and are under 55 years of age."
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
                    "The higher the teacher leaver rates are expected to be, the more PGITT newly qualified ",
                    "entrants (NQEs) may be needed to replace teacher leavers which will lead to higher ",
                    "PGITT trainee need (all else being equal)."
                  ),
                  p(
                    "Historical leaver rates were retrospectively revised downward in the SWC last year. This ",
                    "related to a data issue within the Teacher Pension Scheme extracts that inflated leavers ",
                    "and returners for <1,000 teachers per year that has since been resolved by the ",
                    "publication team. This revision has been a key driver in there being a lower leaver rate ",
                    "trajectory in this year’s calculations of 2026/27 PGITT trainee need compared to those for 2025/26."
                  ),
                  p(strong("Entrant types")),
                  p(
                    "Newly qualified entrants (NQEs) are teachers who gain qualified teacher status and will ",
                    "be recorded as entering service in the English state-funded schools sector (primary and ",
                    "secondary schools only). "
                  ),
                  p(
                    "New to state-funded sector entrants (NTSF) are teachers who enter service having not ",
                    "been employed as a regular teacher in the sector previously and are not newly qualified ",
                    "entrants (NQEs). This group includes newly qualified teachers that defer entry into the ",
                    "workforce by 4 to 16 months, and those that have only taught in other sectors, e.g. ",
                    "independent schools, Wales, and Scotland."
                  ),
                  p(
                    "Returners are teachers who enter service in the English state-funded schools sector, ",
                    "and are recorded within the school workforce census as having worked in the ",
                    "state-funded sector before."
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
