user_guide_panel <- function() {
  tabPanel(
    "User guide",
    gov_main_layout(
      gov_row(
        column(
          12,
          h1("Teacher workforce supply dashboard user guide"),
          h2("Introduction"),
          p("xxxx"),
          h2("Context and purpose"),
          p("xxxx"),
          h2("Disclaimers and caveats"),
          p("xxxx"),
          h2("Data sources and updates"),
          p("xxxx"),
          h2("User tips"),
          p("xxxx")
        )
      )
    )
  )
}
