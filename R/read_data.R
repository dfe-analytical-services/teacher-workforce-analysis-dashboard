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
      # use round_five_up() instead of round() to avoid banker's rounding
      # (round-to-even behaviour for x.5 values).
      teacher_numbers = dfeR::round_five_up(teacher_numbers, dp = 0),
      pupil_numbers = dfeR::round_five_up(pupil_numbers, dp = 0)
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
      academic_year = paste0(
        start_year,
        "/",
        sprintf("%02d", (start_year + 1) %% 100)
      ),
      # format all numeric columns to appropriate number of dps
      # use round_five_up() instead of round() to avoid banker's rounding
      # (round-to-even behaviour for x.5 values).
      pgitt_trainee_need_count = dfeR::round_five_up(
        pgitt_trainee_need_count,
        dp = 0
      ),
      difference_to_previous_year_count = dfeR::round_five_up(
        difference_to_previous_year_count,
        dp = 0
      ),
      difference_to_previous_year_percent = dfeR::round_five_up(
        difference_to_previous_year_percent,
        dp = 1
      )
    )
  return(df)
}


# Drivers analysis data -----------------------------------------------------------

read_drivers_data <- function(
  file = "data/3_drivers_analysis_2026-04-23.parquet"
) {
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

  # round values to 1 dp
  # use round_five_up() instead of round() to avoid banker's rounding
  # (round-to-even behaviour for x.5 values).
  df <- df %>%
    mutate(value = dfeR::round_five_up(value, dp = 1))

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
    filter(!is.na(value)) %>%
    # round values using round_five_up() because base R's round() uses
    # banker's rounding (round-to-even) for values ending in .5.
    # leaver rates are stored as proportions (e.g. 0.056 = 5.6%), so keep 3 dp.
    # entrant values are counts, so round to 0 dp.
    mutate(
      value <- case_when(
        grepl("leaver", type, ignore.case = TRUE) ~
          dfeR::round_five_up(value, dp = 3),
        TRUE ~
          dfeR::round_five_up(value, dp = 0)
      )
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
    filter(!is.na(value)) %>%
    # round values using round_five_up() because base R's round() uses
    # banker's rounding (round-to-even) for values ending in .5.
    # leaver rates are stored as proportions (e.g. 0.056 = 5.6%), so keep 3 dp.
    # entrant values are counts, so round to 0 dp.
    mutate(
      value <- case_when(
        grepl("leaver", type, ignore.case = TRUE) ~
          dfeR::round_five_up(value, dp = 3),
        TRUE ~
          dfeR::round_five_up(value, dp = 0)
      )
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
        "❌ Missing required columns in flows 2026 file: ",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  return(df)
}
