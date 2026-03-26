user_guide_panel <- function() {
  tabPanel(
    "User guide",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("Teacher workforce supply dashboard user guide"),
          h2("Introduction"),
          p(
            "The Department for Education (DfE) has developed the Teacher Workforce Supply ",
            "Dashboard to provide clear, accessible insight into how Postgraduate Initial Teacher ",
            "Training (PGITT) trainee need is estimated using the teacher workforce model."
          ),
          p(
            "The teacher workforce model (TWM) is a national stocks and flows model covering all ",
            "state-funded primary schools (including maintained nursery classes attached to ",
            "schools) and secondary schools in England, including post 16 provision, academies, ",
            "and free schools"
          ),
          p(
            "The model estimates the future number of qualified and unqualified teachers required ",
            "in these settings beyond 2024/25, both for primary and for each individual secondary ",
            "subject. By projecting the expected inflows and outflows of teachers, the TWM then ",
            "calculates the number of PGITT trainees needed in 2026/27 to supply sufficient ",
            "teachers for the 2027/28 academic year."
          ),
          p(
            "This interactive dashboard accompanies the ",
            em("Teacher demand and postgraduate trainee need"),
            "publication, available here: [link]."
          ),
          h2("Context and purpose"),
          p(
            "The ‘Teacher demand and PGITT need’ section of the dashboard supports analysis of ",
            "teacher supply and trainee demand by visualising several components of the teacher ",
            "workforce model. It enables users to:"
          ),
          tags$ul(
            tags$li(
              "Explore how projections of pupil numbers influence the future teacher demand ",
              "trajectory for both primary and secondary state-funded schools."
            ),
            tags$li("Understand the factors involved in estimating PGITT trainee need."),
            tags$li("Examine how PGITT trainee need is changing over time and the drivers of these changes."),
            tags$li(
              "Explore the inflows and outflows to the teacher workforce, both historical and ",
              "forecast, and how these relate to PGITT trainee need."
            )
          ),
          h2("Disclaimers and caveats"),
          tags$ul(
            tags$li(
              "Figures used within the TWM may differ to the School Workforce in England ",
              "publication (SWC) which includes special schools and PRUs within the state-funded ",
              "schools sector."
            ),
            tags$li(
              "Leavers are counted in different academic years in the TWM and SWC. In the ",
              "TWM, teachers that are recorded as being in service in the November 2023 SWC, ",
              "but not within the November 2024 SWC are assumed to be leavers in the ",
              "2024/25 academic year. Whereas, in the SWC, these leavers would be counted ",
              "as leavers in the 2023/24 academic year. ",
              tags$ul(
                tags$li(
                  "This approach is taken within the TWM for modelling purposes to ensure ",
                  "these teachers align with entrant numbers coming in."
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
                  "Coverage differences needed to align with TWM methodology (e.g.post-16 ",
                  "pupils in secondary schools are included within the TWM), and"
                ),
                tags$li(
                  "Adjustments reflecting the actual number of pupils captured in October ",
                  "2025 school returns."
                )
              )
            ),
            h2("Data sources and updates"),
            p(em("Teacher demand and PGITT need"), " tab:"),
            reactableOutput("data_sources_updates"),
            h2("User tips"),
            tags$ul(
              tags$li(
                "This dashboard is built over multiple tabs that can be moved through using ",
                "either the navigation buttons (up and down or left and right) or by selecting the ",
                "tab in the list in the top left-hand side of the page. The currently selected view ",
                "will be highlighted in blue."
              ),
              tags$li(
                "The ", em("Teacher demand and PGITT need "),
                "tab contains several charts, diagrams and tables. These are organised into tabbed ",
                "panels, and users can move between them by clicking the chart titles displayed ",
                "above the chart space. The active chart tab will be highlighted in yellow. "
              ),
              tags$li(
                "Most charts in this dashboard are interactive. When users move their cursor ",
                "across a chart, a hover label will appear showing additional context, underlying ",
                "data values, or comparisons across groups."
              ),
              tags$li(
                "Many pages include dropdown menus that allow users to filter results by school ",
                "phase or secondary subject. Changing a selection will automatically update all ",
                "charts on the page. "
              ),
              tags$li(
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
