# # ---------------------------------------------------------------------------------------------------------------
#
# USER INTERFACE COMPONENTS FOR: *User guide* tab
# Includes layout, text and table output
#
# # ---------------------------------------------------------------------------------------------------------------

user_guide_panel <- function() {
  tabPanel(
    "User guide",
    gov_main_layout(
      gov_row(
        column(
          12,
          heading_text("Teacher workforce analysis dashboard user guide", level = 1, size = "l"),
          heading_text("Introduction", level = 2, size = "m"),
          gov_text(
            "The Department for Education (DfE) has developed the ",
            em("Teacher workforce analysis dashboard "),
            "to provide clear, accessible insight into how postgraduate initial teacher ",
            "training (PGITT) trainee need is estimated using the teacher workforce model."
          ),
          gov_text(
            "The teacher workforce model (TWM) is a national stocks-and-flows model covering all ",
            "state-funded primary schools (including maintained nurseries attached to schools) ",
            "and secondary schools in England, including post-16 provision, academies, ",
            "and free schools."
          ),
          gov_text(
            "The model estimates the future number of qualified and unqualified teachers required ",
            "in these settings beyond 2024/25, both for primary and for each individual secondary ",
            "subject. By projecting the expected inflows and outflows of teachers, the TWM then ",
            "calculates the number of PGITT trainees needed in 2026/27 to supply sufficient ",
            "teachers for the 2027/28 academic year."
          ),
          gov_text(
            "This interactive dashboard accompanies the ",
            a(
              "Teacher demand and postgraduate trainee need publication.",
              href = "https://explore-education-statistics.service.gov.uk/find-statistics/teacher-demand-and-postgraduate-trainee-need/2026-27",
              target = "_blank"
            )
          ),
          heading_text("Context and purpose", level = 2, size = "m"),
          gov_text(
            "The ",
            em("‘teacher demand and PGITT need’"),
            " section of the dashboard supports analysis of ",
            "teacher demand and supply and trainee demand by visualising several components of the teacher ",
            "workforce model. It enables users to:"
          ),
          tags$ul(
            tags$li(
              gov_text(
                "Explore how projections of pupil numbers influence the future teacher demand ",
                "trajectory for both primary and secondary state-funded schools."
              )
            ),
            tags$li(
              gov_text(
                "Understand the factors involved in estimating PGITT trainee need."
              )
            ),
            tags$li(
              gov_text(
                "Examine how PGITT trainee need is changing over time and the drivers of these changes."
              )
            ),
            tags$li(
              gov_text(
                "Explore the inflows and outflows to the teacher workforce, both historical and ",
                "forecast, and how these relate to PGITT trainee need."
              )
            )
          ),
          heading_text("Disclaimers and caveats", level = 2, size = "m"),
          tags$ul(
            tags$li(
              gov_text(
                "The inputs to the teacher workforce model are the most timely data available as ",
                "of February 2026. It has been presented within the ",
                em("'teacher demand and PGITT need'"),
                " part of the dashboard as it was data used to estimate 2026/27 PGITT need."
              ),
              tags$ul(
                tags$li(
                  gov_text(
                    "2026/27 PGITT need will not be retrospectively updated in future. "
                  )
                ),
                tags$li(
                  gov_text(
                    "As a consequence, the ",
                    em("'teacher demand and PGITT need'"),
                    " part of the dashboard will not be updated after publication."
                  )
                ),
                tags$li(
                  gov_text(
                    "Therefore, these data may differ slightly to that in subsequent updates to ",
                    "the school workforce census (SWC), ITT census, and ITT performance ",
                    "profiles data."
                  )
                )
              )
            ),
            tags$li(
              gov_text(
                "Note – next year PGITT need will be calculated for 2027/28 and will reflect any ",
                "data updates."
              )
            ),
            tags$li(
              gov_text(
                "Figures used within the TWM may differ to the SWC publication (school workforce in England) ",
                "which includes special schools and PRUs within the state-funded schools sector."
              )
            ),
            tags$li(
              gov_text(
                "This publication uses a different naming convention to the SWC for teachers leaving service."
              ),
              tags$ul(
                tags$li(
                  gov_text(
                    "A teacher that leaves service between the November 2023 and November ",
                    "2024 SWC is classified as being a 2023/24 leaver in the School Workforce ",
                    "publication. By contrast, in the TWM, such leavers are classified as being ",
                    "leavers in the 2024/25 academic year."
                  )
                ),
                tags$li(
                  gov_text(
                    "This approach is taken within the TWM for modelling purposes to ensure ",
                    "leavers align with entrants coming in to replace them."
                  )
                ),
                tags$li(
                  gov_text(
                    "I.e. teachers leaving at the very end of the 2023/24 academic year would ",
                    "largely be replaced by entrants entering service in September 2024."
                  )
                )
              )
            ),
            tags$li(
              gov_text(
                "Finally, pupil projections in the dashboard differ slightly from those in the ",
                "national pupil projections release due to:"
              ),
              tags$ul(
                tags$li(
                  gov_text(
                    "Coverage differences which are needed to align with the TWM methodology ",
                    "(e.g. post-16 pupils in secondary schools are included within the TWM), and"
                  )
                ),
                tags$li(
                  gov_text(
                    "Adjustments reflecting the actual number of pupils captured in October ",
                    "2025 school returns."
                  )
                )
              )
            )
          ),
          heading_text("Data sources and updates", level = 2, size = "m"),
          gov_text(em("'Teacher demand and PGITT need'"), " section:"),
          govReactableOutput("data_sources_updates", caption = ""),
          heading_text("User tips", level = 2, size = "m"),
          tags$ul(
            tags$li(
              gov_text(
                "This dashboard is built over multiple tabs that can be moved through using ",
                "either the navigation buttons (up and down or left and right) or by selecting the ",
                "tab in the list in the top left-hand side of the page. The currently selected view ",
                "will be highlighted in blue."
              )
            ),
            tags$li(
              gov_text(
                "The ",
                em("'Teacher demand and PGITT need' "),
                "tab contains several charts, diagrams and tables. These are organised into tabbed ",
                "panels, and users can move between them by clicking the chart titles displayed ",
                "above the chart space. The active chart tab will be highlighted in yellow. "
              )
            ),
            tags$li(
              gov_text(
                "Most charts in this dashboard are interactive. When users move their cursor ",
                "across a chart, a hover label will appear showing additional context, underlying ",
                "data values, or comparisons between groups."
              )
            ),
            tags$li(
              gov_text(
                "Many pages include dropdown menus that allow users to filter results by school ",
                "phase or secondary subject. Changing a selection will automatically update all ",
                "charts on the page. "
              )
            ),
            tags$li(
              gov_text(
                "Most pages also provide tabs which allow users to switch between examining ",
                "data in chart or table form, and provides users with a tab to download the table ",
                "or chart image."
              )
            )
          )
        )
      )
    )
  )
}
