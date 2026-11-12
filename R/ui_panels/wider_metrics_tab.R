# # ---------------------------------------------------------------------------------------------------------------
#
# USER INTERFACE COMPONENTS FOR: *Wider teacher supply metrics* tab
# Contains the Introduction, Sufficiency, Quality and Subject level metrics sub-tabs
# (metrics sub-tabs all use data from the school workforce census)
#
# # ---------------------------------------------------------------------------------------------------------------

wider_metrics_tab_panel <- function() {
  tabPanel(
    "Wider teacher supply metrics",
    gov_main_layout(
      gov_row(
        # Header --------------------------------------------------------------
        column(
          width = 12,
          div(
            id = "wider_metrics_main_col",
            heading_text("Wider teacher supply metrics", level = 1, size = "l")
          )
        ),

        # Tabs ---------------------------------------------------------------
        column(
          width = 12,
          tabsetPanel(
            id = "wider_metrics_tabsetpanels",

            #####################################################################

            #   Introduction tab

            #####################################################################

            tabPanel(
              "Introduction",
              gov_main_layout(
                heading_text("Introduction", level = 2, size = "m"),
                heading_text("About this section", level = 3, size = "s"),
                gov_text(
                  "The ",
                  em("'Wider teacher supply metrics'"),
                  " section of the dashboard can be explored across three areas:"
                ),
                tags$ul(
                  tags$li(
                    gov_text(
                      actionLink("link_to_swc_measures", "Sufficiency metrics"),
                      " shows historic teacher vacancy, entrant and leaver rates, and pupil to teacher ratios, ",
                      "using data from the school workforce census (SWC)."
                    )
                  ),
                  tags$li(
                    gov_text(
                      actionLink("link_to_quality_metrics", "Quality metrics"),
                      " shows the percentage of teachers still in service after qualifying, the percentage ",
                      "of teachers with qualified teacher status, and the number of occasional teachers, ",
                      "using data from the school workforce census (SWC)."
                    )
                  ),
                  tags$li(
                    gov_text(
                      actionLink("link_to_subject_metrics", "Subject level metrics"),
                      " shows the specialism rate for secondary subjects, using data from the school ",
                      "workforce census (SWC)."
                    )
                  )
                ),
                gov_text(
                  "All of the data in this section comes from the school workforce census (SWC), rather ",
                  "than from the teacher workforce model (TWM) used in the Teacher demand and PGITT need ",
                  "section of this dashboard. Figures cover state-funded schools in England."
                )
              )
            ),

            #####################################################################

            #   Sufficiency metrics tab (data from the school workforce census)

            #####################################################################

            swc_tab_panel(
              prefix = "swc",
              title = "Sufficiency metrics",
              intro = gov_text(
                "This section shows historic trends in teacher vacancy rates, qualified teacher ",
                "entrant and leaver rates, and pupil to teacher ratios. All of the data in this section ",
                "comes from the school workforce census (SWC), rather than from the teacher workforce ",
                "model used in the other sections of this dashboard. Figures cover state-funded schools ",
                "in England."
              ),
              measure_choices = choices_swc_measure,
              teacher_type_choices = choices_swc_teacher_type,
              about = tagList(
                gov_text(
                  "Select a measure to view its data. Each line shows a different breakdown ",
                  "of the measure, and hovering over the chart shows the value of every line ",
                  "for that academic year."
                ),
                gov_text(
                  "When the pupil to teacher ratio is selected, choose whether to include only ",
                  "qualified teachers, or both qualified and unqualified teachers."
                ),
                gov_text(
                  "Rates are shown as percentages and pupil to teacher ratios as the number of ",
                  "pupils per teacher. The y axis starts at zero. ",
                  "Different shaped markers are used for each line as well as different colours."
                )
              ),
              definitions = tagList(
                gov_text(strong("Vacancy rates")),
                gov_text(
                  "Teacher vacancy rates are shown separately for state-funded nursery and primary ",
                  "schools and for state-funded secondary schools."
                ),
                gov_text(strong("Entrant types")),
                gov_text(
                  "Newly qualified entrants (NQEs) are teachers who gain qualified teacher status and ",
                  "are recorded as entering service in the English state-funded schools sector ",
                  "immediately after training."
                ),
                gov_text(
                  "Deferred newly qualified entrants are newly qualified teachers that defer entry into ",
                  "the workforce by 4 to 16 months."
                ),
                gov_text(
                  "Entrants new to the state-funded sector are teachers who enter service having not ",
                  "been employed as a regular teacher in the sector previously and are not newly ",
                  "qualified entrants. This includes those that have only taught in other sectors, ",
                  "e.g. independent schools, Wales, and Scotland."
                ),
                gov_text(
                  "Returners to the state-funded sector are teachers who enter service in the English ",
                  "state-funded schools sector, and are recorded within the school workforce census as ",
                  "having worked in the state-funded sector before."
                ),
                gov_text(strong("Pupil to teacher ratios")),
                gov_text(
                  "Pupil to teacher ratios show the number of pupils for each teacher, separately for ",
                  "state-funded nursery and primary schools and for state-funded secondary schools."
                ),
                gov_text(
                  "Selecting 'Qualified teachers only' counts qualified teachers only. Selecting ",
                  "'Qualified and unqualified teachers' counts both qualified and unqualified ",
                  "teachers, so the ratio is lower."
                ),
                gov_text(strong("Leaver types")),
                gov_text(
                  "Leaver rates are shown for teachers who left service because they retired, died, or ",
                  "left for any other reason (out of service)."
                ),
                gov_text(
                  "The total entrant and leaver rates are the sum of the individual types shown. ",
                  "Totals may differ slightly from the sum of the types due to rounding."
                ),
                gov_text(strong("Academic year naming")),
                gov_text(
                  "Leavers follow the SWC naming convention: a teacher that leaves service between the ",
                  "November 2023 and November 2024 SWC is classified as a 2023/24 leaver. This differs ",
                  "from the naming convention used in the teacher workforce model in the other tabs of ",
                  "this dashboard. Leaver rates are therefore available up to 2023/24, one year earlier ",
                  "than entrant and vacancy rates."
                )
              )
            ),

            #####################################################################

            #   Quality metrics tab (data from the school workforce census)

            #####################################################################

            swc_tab_panel(
              prefix = "quality",
              title = "Quality metrics",
              intro = gov_text(
                "This section shows historic trends in the percentage of teachers still in service ",
                "after qualifying, the percentage of teachers with qualified teacher status (QTS), and ",
                "the number of occasional teachers. All of the data in this section comes from the ",
                "school workforce census (SWC), rather than from the teacher workforce model used in ",
                "the other sections of this dashboard. Figures cover state-funded schools in England."
              ),
              measure_choices = choices_quality_measure,
              about = tagList(
                gov_text(
                  "Select a measure to view its data. Each line shows a different breakdown ",
                  "of the measure, and hovering over the chart shows the value of every line ",
                  "for that academic year."
                ),
                gov_text(
                  "For the percentage of teachers still in service after qualifying, the academic year ",
                  "is the year teachers qualified. Longer periods after qualifying are only available for ",
                  "earlier years, because the time has not yet passed for more recent years."
                ),
                gov_text(
                  "Percentages and headcounts are shown as labelled on each chart. The y axis starts at ",
                  "zero. Different shaped markers are used for each line as well as different colours."
                )
              ),
              definitions = tagList(
                gov_text(strong("Teachers still in service after qualifying")),
                gov_text(
                  "The percentage of teachers who qualified in each academic year who were still in ",
                  "service 1, 3, 5 and 10 years after qualifying."
                ),
                gov_text(strong("Qualified teacher status (QTS)")),
                gov_text(
                  "The percentage of teachers who have qualified teacher status, shown separately for ",
                  "state-funded nursery and primary schools and for state-funded secondary schools."
                ),
                gov_text(strong("Occasional teachers")),
                gov_text(
                  "The headcount of occasional teachers, which is the number of individual teachers ",
                  "rather than full-time equivalents (FTE). It is shown separately for state-funded ",
                  "nursery and primary schools and for state-funded secondary schools, and in total."
                )
              )
            ),

            #####################################################################

            #   Subject level metrics tab (data from the school workforce census)

            #####################################################################

            swc_tab_panel(
              prefix = "subject",
              title = "Subject level metrics",
              intro = gov_text(
                "This section shows historic trends in the specialism rate for secondary subjects. ",
                "All of the data in this section comes from the school workforce census (SWC), rather ",
                "than from the teacher workforce model used in the other sections of this dashboard. ",
                "Figures cover state-funded secondary schools in England."
              ),
              measure_choices = choices_subject_measure,
              breakdown_choices = choices_subject_breakdown,
              breakdown_label = "Select a subject:",
              about = tagList(
                gov_text(
                  "Select a subject to view its specialism rate over time. The rate for all secondary ",
                  "subjects is also shown for comparison. Hovering over the chart shows the value of ",
                  "each line for that academic year."
                ),
                gov_text(
                  "Rates are shown as percentages and the y axis starts at zero. ",
                  "Different shaped markers are used for each line as well as different colours."
                )
              ),
              definitions = tagList(
                gov_text(strong("Specialism rate")),
                gov_text(
                  "The specialism rate is the proportion of hours taught in a subject which were taught ",
                  "by a teacher with a relevant post A level qualification."
                ),
                gov_text(
                  "A selection of subjects and subject groups is available, such as all sciences and all ",
                  "modern foreign languages. 'All subjects' is the rate across all secondary subjects."
                )
              )
            )
          )
        )
      )
    )
  )
}
