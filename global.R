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

# Core shiny and R packages
shhh(library(shiny))
shhh(library(bslib))
shhh(library(rstudioapi))

# Custom packages
shhh(library(dfeR))
shhh(library(dfeshiny))
shhh(library(shinyGovstyle))

# Creating charts and tables
shhh(library(ggplot2))
shhh(library(DT))
shhh(library(sf))
shhh(library(leaflet))
shhh(library(htmltools))
shhh(library(reactable))
shhh(library(svglite))
shhh(library(afcharts))
shhh(library(ggrepel))
shhh(library(showtext))

# Reading files
shhh(library(openxlsx))
shhh(library(readxl)) # added
shhh(library(arrow)) # added

# Data and string manipulation
shhh(library(dplyr))
shhh(library(stringr))
shhh(library(ggiraph))
shhh(library(janitor)) # added

# Shiny extensions
shhh(library(shinyjs))
shhh(library(tools))
shhh(library(shinytitle))
shhh(library(xfun))
shhh(library(metathis))
shhh(library(shinyalert))

# Dependencies needed for testing or CI but not for the app -------------------
# Including them here keeps them in renv but avoids the app needlessly loading
# them, saving on load time.
if (FALSE) {
  shhh(library(shinytest2))
  shhh(library(chromote))
  shhh(library(testthat))
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

gbp <- enc2utf8("\u00A3")

# Source all files in the ui_panels folder
lapply(list.files("R/ui_panels/", full.names = TRUE), source)

# Set global variables --------------------------------------------------------

site_title <- "Teacher Workforce Supply Dashboard (England)" # name of app # UPDATED
parent_pub_name <- "Statistical publication" # name of source publication # TBC
parent_publication <- # link to source publication
  "https://explore-education-statistics.service.gov.uk/find-statistics/apprenticeships" # TBC

# Set the URLs that the site will be published to
site_primary <- "https://department-for-education.shinyapps.io/dfe-shiny-template/" # ??

# Combine URLs into list for disconnect function
# We can add further mirrors where necessary. Each one can generally handle
# about 2,500 users simultaneously
sites_list <- c(site_primary)

# Set the key for Google Analytics tracking
google_analytics_key <- "Z967JJVQQX" # ??

# End of global variables -----------------------------------------------------

# Enable bookmarking so that input choices are shown in the url ---------------
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

# phase list for teacher demand trajectory tab filter
# sort phase so primary first

choices_pupil_teacher_phase <- sort(unique(pupil_teacher_numbers$phase))

# Add data for PGITT trainee need ---------------------------------------------------

pgitt_need_timeseries <- read_pgitt_need_timeseries()

# phase and subject list for pgitt trainee need tab filter
# sort phase so primary first

choices_pgitt_need_phase <- sort(unique(pgitt_need_timeseries$phase))

# make a unique subject list but it starts with total

choices_pgitt_need_subject <- c("Total", sort(setdiff(unique(pgitt_need_timeseries$subject), "Total")))


# Add data for drivers ----------------------------------------------------------

drivers_data <- read_drivers_data()

# phase and subject list for drivers tab

# sort phase so primary first

choices_drivers_phase <- sort(unique(drivers_data$phase))

# make a unique subject list but it starts with total

choices_drivers_subject <- c("Total", sort(setdiff(unique(drivers_data$subject), "Total")))


# Add data for flow trajectories  ---------------------------------------------------

flow_data_last_year <- read_flows_data()

# make dummy data for this year

# copy values for final year and make them dummy values for 2027/28 - REMOVE ONCE REAL DATA IS PUBLISHED

dummy_27_flow_data <- flow_data_last_year %>%
  filter(academic_year == "2026/27") %>%
  mutate(academic_year = "2027/28", year = 2027)

# bind to original dataset

dummy_flow_data_this_year <- bind_rows(flow_data_last_year, dummy_27_flow_data) %>%
  mutate(
    historic_or_trajectory = ifelse(year >= 2025, "Trajectory", "Historic"),
    value = value * 1.1,
    #  value = ifelse(year>2023, value*1.1, value),
    version = "This year (dummy data)"
  )

# final dataset

flow_data <- bind_rows(flow_data_last_year, dummy_flow_data_this_year)

# remove others

rm(flow_data_last_year, dummy_27_flow_data, dummy_flow_data_this_year)

# save values of phase, subject and flow type

choices_flow_phase <- sort(unique(flow_data$phase))

choices_flow_subject <- sort(unique(flow_data$subject))

choices_flow_type <- c("Total leaver rate", "Under 55 leaver rate", "55+ leaver rate", "Newly qualified entrants", "New to state-funded sector entrants", "Returners")
