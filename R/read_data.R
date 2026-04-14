# -----------------------------------------------------------------------------
# Script with functions to read in the data files.
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

read_pupil_teacher_numbers <- function(
  file = "data/dummy_1_pupil_teacher_numbers.parquet"
) {
  df <- read_parquet(file) %>%
    clean_names() # make r friendly column names

  # required columns
  required_cols <- c(
    "academic_year",
    "pupil_numbers",
    "teacher_numbers",
    "projection",
    "phase"
  )

  # check required columns
  missing <- setdiff(required_cols, names(df))

  if (length(missing) > 0) {
    stop(
      paste0(
        "❌ Missing required columns in pupil/teacher numbers file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  # transformations
  df <- df %>%
    mutate(
      start_year = as.integer(substr(academic_year, 1, 4)), # create start year column
      pupil_numbers = round(pupil_numbers, 0), # round pupil numbers to nearest 0
      teacher_numbers = round(
        teacher_numbers,
        0 # round teachers numbers to nearest 0
      )
    )
  return(df)
}


# PGITT need time series data --------------------------------------------------

read_pgitt_need_timeseries <- function(
  file = "data/dummy_2_pgitt_targets_timeseries.parquet"
) {
  df <- read_parquet(file) %>%
    clean_names() # make r friendly column names

  # required columns
  required_cols <- c(
    "time_period",
    "subject",
    "subject_filter_group",
    "pgitt_trainee_need",
    "percentage_difference_to_previous_year"
  )

  # check required columns
  missing <- setdiff(required_cols, names(df))

  if (length(missing) > 0) {
    stop(
      paste0(
        "❌ Missing required columns in pgitt need time series file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df <- df %>%
    rename(phase = subject_filter_group, ees_subject = subject) %>% # rename columns from pub names
    mutate(
      start_year = as.integer(substr(time_period, 1, 4)), # create start year column
      academic_year = paste0(start_year, "/", sprintf("%02d", (start_year + 1) %% 100)),
      subject = if_else(
        phase == "Primary",
        "Total",
        ees_subject # if phase is primary set subject as total
      )
    )
  return(df)
}


# Drivers analysis data -----------------------------------------------------------

read_drivers_data <- function(file = "data/dummy_3_drivers_analysis.parquet") {
  df <- read_parquet(file) %>%
    clean_names() # make r friendly column names

  # required columns
  required_cols <- c(
    "driver",
    "value",
    "phase",
    "subject"
  )

  # check required columns
  missing <- setdiff(required_cols, names(df))

  if (length(missing) > 0) {
    stop(
      paste0(
        "❌ Missing required columns in drivers file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  df <- df %>%
    mutate(value = round(value, digits = 1))

  return(df)
}


# Flow trajectories data ------------------------------------------------------------------------------------------

read_flows_data <- function(
  file = "data/dummy_4_flow_trajectories_2025_publication.parquet"
) {
  df <- read_parquet(file) %>%
    clean_names() %>% # make r friendly column names
    rename(academic_year = year) %>%
    mutate(
      unit = if_else(str_detect(type, "leaver"), "%", "FTE"), # make a column of unit which is % for leaver rates and FTE for entrants
      start_year = as.integer(substr(academic_year, 1, 4)), # create start year column
      version = "Last year"
    )

  # required columns
  required_cols <- c(
    "phase",
    "subject",
    "type",
    "academic_year",
    "value",
    "unit",
    "historic_or_trajectory",
    "publication_year"
  )

  # check required columns
  missing <- setdiff(required_cols, names(df))

  if (length(missing) > 0) {
    stop(
      paste0(
        "❌ Missing required columns in flows file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(df)
}
