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
  file = "data/1_pupil_teacher_numbers_2026-04-23.parquet"
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
      # update to floor() because round() uses "round to even" (banker's rounding) on x.5 values by default
      teacher_numbers = floor(teacher_numbers + 0.5),
      pupil_numbers = floor(pupil_numbers + 0.5)
    )
  return(df)
}


# PGITT need time series data --------------------------------------------------

read_pgitt_need_timeseries <- function(
  file = "data/2_pgitt_need_timeseries_2026-04-23.parquet"
) {
  df <- read_parquet(file) %>%
    clean_names() # make r friendly column names

  # required columns
  required_cols <- c(
    "time_period",
    "subject",
    "education_phase",
    "pgitt_trainee_need_count",
    "difference_to_previous_year_count",
    "difference_to_previous_year_percent"
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
    rename(phase = education_phase) %>% # rename column from pub names
    mutate(
      start_year = as.integer(substr(time_period, 1, 4)), # create start year column
      # create academic year column
      academic_year = paste0(start_year, "/", sprintf("%02d", (start_year + 1) %% 100))
    )
  return(df)
}


# Drivers analysis data -----------------------------------------------------------

read_drivers_data <- function(file = "data/3_drivers_analysis_2026-04-23.parquet") {
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
    mutate(value = round(value, digits = 1)) # round values to 1 dp

  return(df)
}


# Flow trajectories data ------------------------------------------------------------------------------------------

# 2025 publication data

read_flows_2025_publication_data <- function(
  file = "data/4_flow_trajectories_2025_publication_2026-04-23.parquet"
) {
  df <- read_parquet(file) %>%
    clean_names() %>% # make r friendly column names
    mutate(
      start_year = as.integer(substr(academic_year, 1, 4)) # create start year column
    ) %>%
    # NQE trajectories are only for two years ahead
    # remove 3rd year row which has NA data
    # to prevent the table/downloads having an NA row
    filter(!is.na(value))

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
        "❌ Missing required columns in flows 2025 file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(df)
}


# 2026 publication data

read_flows_2026_publication_data <- function(
  file = "data/5_flow_trajectories_2026_publication_2026-04-23.parquet"
) {
  df <- read_parquet(file) %>%
    clean_names() %>% # make r friendly column names
    mutate(
      start_year = as.integer(substr(academic_year, 1, 4)) # create start year column
    ) %>%
    # NQE trajectories are only for two years ahead
    # remove 3rd year row which has NA data
    # to prevent the table/downloads having an NA row
    filter(!is.na(value))

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
        "❌ Missing required columns in flows 2026 file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(df)
}
