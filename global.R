# -----------------------------------------------------------------------------
# This is the global file.
#
# Use it to store functions, library calls, source files etc.
#
# Moving these out of the server file and into here improves performance as the
# global file is run only once when the app launches and stays consistent
# across users whereas the server and UI files are constantly interacting and
# responsive to user input.
#
# Library calls ---------------------------------------------------------------
shhh <- suppressPackageStartupMessages # It's a library, so shhh!

# Core Shiny and R packages
shhh(library(shiny))
shhh(library(bslib))
shhh(library(rstudioapi))

# Custom packages
shhh(library(dfeR))
shhh(library(dfeshiny))
shhh(library(shinyGovstyle))

# Creating charts and tables
shhh(library(ggplot2))
shhh(library(htmltools))
shhh(library(reactable))
shhh(library(svglite))
shhh(library(afcharts))
shhh(library(showtext))

# Reading parquet files
shhh(library(arrow))

# Data and string manipulation
shhh(library(dplyr))
shhh(library(stringr))
shhh(library(ggiraph))
shhh(library(janitor))

# Shiny extensions
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(shinytitle))
shhh(library(xfun))
shhh(library(metathis))
shhh(library(shinyalert))

# Dependencies needed for testing or CI but not for the app itself ------------
# The if(FALSE) block ensures dependency scanners (e.g. renv::dependencies())
# see these packages and keep them in renv.lock, while preventing them from
# being loaded when the app starts saving on load time.
if (FALSE) {
  library(shinytest2)
  library(rsconnect)
  library(chromote)
  library(testthat)
}

# Source scripts --------------------------------------------------------------

# Source any scripts here. Scripts may be needed to process data before it gets
# to the server file or to hold custom functions to keep the main files shorter
#
# It's best to do this here instead of the server file, to improve performance.

# Source script for loading in data
source("R/read_data.R")

# Source custom functions script
source("R/helper_functions.R")

# Source all files in the ui_panels and standard_panels folders
lapply(list.files("R/ui_panels/", full.names = TRUE), source)
lapply(list.files("R/standard_panels/", full.names = TRUE), source)

# Set global variables --------------------------------------------------------

# Name of app
site_title <- "Teacher workforce analysis dashboard (England)"

# Name of source publication
parent_pub_name <- "Teacher demand and postgraduate trainee need"

# Link to source publication
parent_publication <-
  "https://explore-education-statistics.service.gov.uk/find-statistics/teacher-demand-and-postgraduate-trainee-need/2026-27"

# Set the URLs that the site will be published to
site_primary <- "https://department-for-education.shinyapps.io/teacher-workforce-analysis-dashboard/"

# Combine URLs into list for disconnect function
# We can add further mirrors where necessary. Each one can generally handle
# about 2,500 users simultaneously
sites_list <- c(site_primary)

# Set the key for Google Analytics tracking
google_analytics_key <- "437MHW92CL"

# End of global variables -----------------------------------------------------

# Enable bookmarking so that input choices are shown in the URL ---------------
enableBookmarking("url")

# Fonts for charts ------------------------------------------------------------
font_add("dejavu", "www/fonts/DejaVuSans.ttf")
register_font(
  "dejavu",
  plain = "www/fonts/DejaVuSans.ttf",
  bold = "www/fonts/DejaVuSans-Bold.ttf",
  italic = "www/fonts/DejaVuSans-Oblique.ttf",
  bolditalic = "www/fonts/DejaVuSans-BoldOblique.ttf"
)
showtext_auto()

# Read in the data ------------------------------------------------------------

# Add data for teacher and pupil numbers ----------------------------------------

pupil_teacher_numbers <- read_pupil_teacher_numbers()

# Choices for the school phase filter on the Teacher demand trajectories tab
# Sort phase alphabetically so Primary is listed first

choices_pupil_teacher_phase <- sort(unique(pupil_teacher_numbers$phase))

# Add data for PGITT trainee need ---------------------------------------------------

pgitt_need_timeseries <- read_pgitt_need_timeseries()

# Choices for the phase filter on the PGITT trainee need time series tab
# Sort alphabetically but with Total listed first

choices_pgitt_need_phase <- c(
  "Total",
  sort(setdiff(unique(pgitt_need_timeseries$phase), "Total"))
)

# Choices for the secondary subject filter on the PGITT trainee need time series tab
# Sort alphabetically but with Total listed first

choices_pgitt_need_subject <- c(
  "Total",
  sort(setdiff(unique(pgitt_need_timeseries$subject), "Total"))
)

# Add data for drivers ----------------------------------------------------------

drivers_data <- read_drivers_data()

# Choices for the school phase filter on the Drivers of change in PGITT trainee need tab
# Sort phase alphabetically so Primary is listed first

choices_drivers_phase <- sort(unique(drivers_data$phase))

# Choices for the secondary subject filter on the Drivers of change in PGITT trainee need tab
# Sort alphabetically but with Total listed first

choices_drivers_subject <- c(
  "Total",
  sort(setdiff(unique(drivers_data$subject), "Total"))
)

# Add data for flow trajectories  ---------------------------------------------------

# Read in 2025 publication data

flow_data_2025_publication <- read_flows_2025_publication_data()

# Read in 2026 publication data

flow_data_2026_publication <- read_flows_2026_publication_data()

# Final dataset

flow_data <- bind_rows(flow_data_2025_publication, flow_data_2026_publication)

# Choices for the school phase filter on the Flow trajectories tab
# Sort phase alphabetically so Primary is listed first

choices_flow_phase <- sort(unique(flow_data$phase))

# Choices for the secondary subject filter on the Flow trajectories tab
# Sort alphabetically

choices_flow_subject <- sort(unique(flow_data$subject))

# Choices for the entrant or leaver flow type filter on the Flow trajectories tab
# Ordered manually to group leaver rates before entrant types

choices_flow_type <- c(
  "Total leaver rate",
  "Under 55 leaver rate",
  "55+ leaver rate",
  "Newly qualified entrants",
  "New to state-funded sector entrants",
  "Returners"
)

# Display labels for the entrant or leaver flow type filter on the
# Flow trajectories tab. Adds NQE and NTSF abbreviations for
# consistency with the table column headings while preserving the
# underlying values used for filtering.

flow_type_labels <- dplyr::case_when(
  choices_flow_type == "Newly qualified entrants" ~
    "Newly qualified entrants (NQEs)",
  choices_flow_type == "New to state-funded sector entrants" ~
    "New to state-funded sector (NTSF) entrants",
  TRUE ~ choices_flow_type
)

names(choices_flow_type) <- flow_type_labels
