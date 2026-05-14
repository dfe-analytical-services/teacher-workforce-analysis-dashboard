dash_a11y_panel <- function() {
  shiny::tabPanel(
    value = "a11y_panel",
    "Accessibility",
    dfeshiny::a11y_panel(
      dashboard_title = site_title,
      dashboard_url = site_primary,
      date_tested = "22nd April 2026",
      date_prepared = "22nd April 2026",
      date_reviewed = "22nd April 2026",
      issues_contact = "ittstatistics.publications@education.gov.uk",
      non_accessible_components = c(
        "Site navigation has some tagging limitations.",
        "Charts may not be compatible with keyboard navigation (tabulated data is provided as an alternative for keyboard users)."
      ),
      specific_issues = c(
        "Some navigation elements with an ARIA [role] that require children to contain a specific [role] are missing some or all of those required children.",
        "Some instances of background and foreground colors do not have a sufficient contrast ratio.",
        "Some list items (<li>) are not contained within <ul>, <ol> or <menu> parent elements."
      )
    )
  )
}
