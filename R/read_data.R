# -----------------------------------------------------------------------------
# Script where we provide functions to read in the data file(s).
#
# IMPORTANT: Data files pushed to GitHub repositories are immediately public.
# You should not be pushing unpublished data to the repository prior to your
# publication date. You should use dummy data or already-published data during
# development of your dashboard.
#
# In order to help prevent unpublished data being accidentally published, the
# template will not let you make a commit if there are unidentified csv, xlsx,
# tex or pdf files contained in your repository. To make a commit, you will need
# to either add the file to .gitignore or add an entry for the file into
# datafiles_log.csv.
# -----------------------------------------------------------------------------

# Pupil and teacher numbers data ----------------------------------------------

read_pupil_teacher_numbers <- function(file = "data/dummy_1_pupil_teacher_numbers.parquet") {
  read_parquet(file) %>%
    clean_names() %>% # make r friendly column names

    mutate(
      start_year = as.integer(substr(academic_year, 1, 4)), # create start year column
      pupil_numbers = round(pupil_numbers, 0), # round pupil numbers to nearest 0
      teacher_numbers = round(
        teacher_numbers, 0 # round teachers numbers to nearest 0
      )
    )
}


# PGITT need time series data --------------------------------------------------

read_pgitt_need_timeseries <- function(file = "data/dummy_2_pgitt_targets_timeseries.parquet") {
  read_parquet(file) %>%
    clean_names() %>% # make r friendly column names

    rename(phase = subject_filter_group, ees_subject = subject) %>% # rename columns from pub names

    mutate(
      start_year = as.integer(substr(time_period, 1, 4)), # create start year column
      subject = if_else(phase == "Primary", "Total", ees_subject # if phase is primary set subject as total
      )
    )
}


# Drivers analysis data -----------------------------------------------------------

read_drivers_data <- function(file = "data/dummy_3_drivers_analysis.parquet") {
  read_parquet(file) %>%
    clean_names() # make r friendly column names
}


# # Flow trajectories data ---------------------------------------------------------

read_flows_data <- function(file = "data/dummy_4_flow_trajectories_2025_publication.parquet") {
  read_parquet(file) %>%
    clean_names() %>% # make r friendly column names

    rename(academic_year = year) %>%
    mutate(
      unit = if_else(str_detect(type, "leaver"), "%", "FTE"), # make a column of unit which is % for leaver rates and FTE for entrants
      year = as.integer(substr(academic_year, 1, 4)), # create start year column
      version = "Last year"
    )
}
